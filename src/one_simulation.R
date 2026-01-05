#!/usr/bin/env Rscript

### Simulation functions #####
### to compare permutation test and non-permutation test ###
### each function runs a single simulation 
### and calculates a p-value for each different method


################################################
#### Simulation for Mann-Whitney U test #######

### Two sample simulation
simulation_two_sample <- function(n_obs = 100, 
                                  n_perms = 100,
                                  method = c("pool","distribution"),
                                  x_pool = NULL, y_pool = NULL, 
                                  dist_function = rnorm, ...) {
  
  # 1. Sampling data
  if (method == "pool") {
    # Sampling from finite pool of data
    if (is.null(x_pool) || is.null(y_pool)) stop("Pool data required for method='pool'")
    samp_x <- sample(x_pool, size = n_obs, replace = FALSE) 
    samp_y <- sample(y_pool, size = n_obs, replace = FALSE) 
  } else {
    # Generating from a distribution (e.g., rnorm, rpois, etc.)
    # The '...' allows you to pass mean/sd/lambda to the dist_func
    samp_x <- dist_func(n_obs, ...)
    samp_y <- dist_func(n_obs, ...)
  }
  
  # 2. Compute Observed U (using the fast C++ version)
  # We combine x and y once to avoid multiple allocations
  u_observed <- mw_u_logic(c(samp_x, samp_y), n_obs)
  
  # 3. Standard P-Value (Normal Approx)
  p_std <- mann_whitney_normal(samp_x, samp_y, u_observed)
  
  # 4. Permutation P-Value (using the C++ internal loop)
  # This is where 99% of your time is saved
  p_perm <- mann_whitney_perm_fast(samp_x, samp_y, u_observed, n_perms)
  
  # 5. Return results
  return(c(
    standard    = p_std,
    permutation = p_perm
  ))
}

################################################
#### Simulation for Independence Testing #######

simulation_independence_test <- function(n_obs = 100, 
                                         n_perms = 100,
                                         method = c("pool","distribution"),
                                         x_pool = NULL, y_pool = NULL, 
                                         dist_function = rnorm, ...
                                         ) {
  
  # 1. Sampling data
  if (method == "pool") {
    # Sampling from finite pool of data
    if (is.null(x_pool) || is.null(y_pool)) stop("Pool data required for method='pool'")
    samp_x <- sample(x_pool, size = n_obs, replace = FALSE) 
    samp_y <- sample(y_pool, size = n_obs, replace = FALSE) 
  } else {
    # Generating from a distribution (e.g., rnorm, rpois, etc.)
    # The '...' allows you to pass mean/sd/lambda to the dist_func
    samp_x <- dist_func(n_obs, ...)
    samp_y <- dist_func(n_obs, ...)
  }
  
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
  
  # 5. Return results
  return(c(
    chisq_standard  = p_chisq_std,
    chisq_simulated = p_chisq_sim,
    permutation_u   = p_multi_u
  ))
}