#### Simulation functions #####
### to compare permutation test and non-permutation test ###

################################################
#### Simulation for Mann-Whitney U test #######

# simulation function for two-sample testing
simulation_two_sample <- function(x, y, n = 100, n_perms = 100, alpha = 0.05){
  
  # "original" sample data
  x <- sample(x$price, size = n, replace = FALSE)
  y <- sample(y$price, size = n, replace = FALSE) 
  
  # empty vector to store results
  results <- c(NA, NA)
  names(results) <- c("standard", "permutation")
  
  # calculate mann-whitney u statistic
  u_observed <- mann_whitney_u(x, y)
  
  # get p-value from normal approximation
  standard_p <- mann_whitney_normal(x, y, u_observed)
  if (standard_p <= alpha){
    results["standard"] <- "reject"
  } else {
    results["standard"] <- "fail to reject"
  }
  
  # get p-value from permutation distribution
  permutation_p <- mann_whitney_perm(x, y, u_observed, n_perms)
  if (permutation_p <= alpha){
    results["permutation"] <- "reject"
  } else {
    results["permutation"] <- "fail to reject"
  }
  
  # return results of this simulation  
  return(results)
}

################################################
#### Simulation for Independence Testing #######

simulation_independence <- function(x, y, n = 100, 
                                         n_perms = 100, alpha = 0.05){
  
  # "original" sample data
  x <- sample(x, size = n, replace = FALSE)
  y <- sample(y, size = n, replace = FALSE) 
  
  # empty vector to store results
  results <- c(NA, NA, NA)
  names(results) <- c("chisq_standard","chisq_simulated","permutation")
  #results <- c(NA, NA)
  #names(results) <- c("chisq_standard","permutation")
  
  # perform standard chi-squared test
  chisq_standard_p <- chisq.test(x, y)$p.value
  if (chisq_standard_p <= alpha){
    results["chisq_standard"] <- "reject"
  } else {
    results["chisq_standard"] <- "fail to reject"
  }
  
  # perform chi-squared test with simulated/bootstrap p-values
  chisq_simulated_p <- chisq.test(x, y, 
                                  simulate.p.value = TRUE, B=n_perms)$p.value
  if (chisq_simulated_p <= alpha){
    results["chisq_simulated"] <- "reject"
  } else {
    results["chisq_simulated"] <- "fail to reject"
  }
  
  # perform multinomial u test with permutation 
  multinomial_u_p <- get_multinomial_u_p(x, y, n_perms)
  if (multinomial_u_p <= alpha){
    results["permutation"] <- "reject"
  } else {
    results["permutation"] <- "fail to reject"
  }
  
  # return results of this simulation  
  return(results)
}