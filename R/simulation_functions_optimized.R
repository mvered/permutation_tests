#### Simulation functions #####
### to compare permutation test and non-permutation test ###


################################################
#### Simulation for Mann-Whitney U test #######

### Two sample simulation
simulation_two_sample <- function(x_pool, y_pool, n = 100, n_perms = 100, alpha = 0.05) {
  
  # 1. Efficient Sampling: Extract prices once to avoid repetitive $ access
  # Note: Use 'replace = TRUE' if simulating from a distribution, 
  # but 'FALSE' is fine if sampling from a finite pilot dataset.
  samp_x <- sample(x_pool, size = n, replace = FALSE)
  samp_y <- sample(y_pool, size = n, replace = FALSE)
  
  # 2. Compute Observed U (using the fast C++ version)
  # We combine x and y once to avoid multiple allocations
  u_observed <- mw_u_logic(c(samp_x, samp_y), n)
  
  # 3. Standard P-Value (Normal Approx)
  p_std <- mann_whitney_normal(samp_x, samp_y, u_observed)
  
  # 4. Permutation P-Value (using the C++ internal loop)
  # This is where 99% of your time is saved
  p_perm <- mann_whitney_perm_fast(samp_x, samp_y, u_observed, n_perms)
  
  # 5. Return numeric results (1 for Reject, 0 for Fail)
  # This makes calculating Power = mean(results)
  return(c(
    standard    = as.numeric(p_std <= alpha),
    permutation = as.numeric(p_perm <= alpha)
  ))
}

################################################
#### Simulation for Independence Testing #######

simulation_independence_test <- function(x_pool, y_pool, n = 100, 
                                         n_perms = 100, alpha = 0.05) {
  
  # 1. Faster Sampling
  # Extracting values here ensures we pass clean vectors to the tests
  samp_x <- sample(x_pool, size = n, replace = FALSE)
  samp_y <- sample(y_pool, size = n, replace = FALSE) 
  
  # 2. Chi-Squared Standard (using R's highly optimized engine)
  # We wrap in suppressWarnings because chisq.test often warns about 
  # small cell counts, which can clutter logs.
  chisq_res <- suppressWarnings(chisq.test(samp_x, samp_y))
  p_chisq_std <- chisq_res$p.value
  
  # 3. Chi-Squared Simulated (B = n_perms)
  # This is R's internal Monte Carlo version of the Chi-Square test.
  p_chisq_sim <- suppressWarnings(
    chisq.test(samp_x, samp_y, simulate.p.value = TRUE, B = n_perms)$p.value
  )
  
  # 4. Multinomial U-Test (Using our fast C++ Permutation Wrapper)
  # Ensure the inputs are character vectors to match our C++ types
  p_multi_u <- get_multinomial_u_p(as.character(samp_x), 
                                   as.character(samp_y), 
                                   n_perms)
  
  # 5. Return Numeric Boolean (1 = Reject, 0 = Fail)
  # This allows you to use colMeans() on the result matrix to get Power/Type I Error
  return(c(
    chisq_standard  = as.numeric(p_chisq_std <= alpha),
    chisq_simulated = as.numeric(p_chisq_sim <= alpha),
    permutation_u   = as.numeric(p_multi_u <= alpha)
  ))
}