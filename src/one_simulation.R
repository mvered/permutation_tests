#!/usr/bin/env Rscript

### Simulation functions #####
### to compare permutation test and non-permutation test ###
### each function runs a single simulation 
### and calculates a p-value for each different method


################################################
### TWO SAMPLE SIMULATION ###
### SIMULATION FOR MANN-WHITNEY U TEST ######

simulation_two_sample <- function(n_obs = 100, 
                                  n_perms = 100,
                                  data_source = "real",
                                  x_data = NULL, y_data = NULL, 
                                  mu = 0.0, shift = 0.0) {
  
  # 1. Sampling data
  if (data_source == "real") {
    # Sampling from finite pool of data
    if (is.null(x_data) || is.null(y_data)) stop("Real data required for data_source='real'")
    samp_x <- sample(x_data, size = n_obs, replace = FALSE) 
    samp_y <- sample(y_data, size = n_obs, replace = FALSE) 
  
  } else if (data_source == "normal") {
    # Generating from two identical normal distributions
    samp_x <- rnorm(n_obs, mean = mu, sd = 1)
    samp_y <- rnorm(n_obs, mean = mu, sd = 1)
  
  } else if (data_source == "t_dist"){
    # generate from two identical t distributions
    samp_x <- rt(n_obs, df = 2)
    samp_y <- rt(n_obs, df = 2) 

  } else if (data_source == "normal_shift") {
    # Generating from a normal distribution and a shifted version of that distribution
    samp_x <- rnorm(n_obs, mean = mu, sd = 1)
    samp_y <- rnorm(n_obs, mean = mu + shift, sd = 1)
  
  } else if (data_source == "t_dist_shift"){
    # Generating from a t distribution and a shifted version of that distribution
    samp_x <- rt(n_obs, df = 2)
    samp_y <- rt(n_obs, df = 2) + shift

  } else {
    stop(paste("Invalid data source configuration for two-sample test matching statement:", data_source))
  }
  
  # 2. Compute Observed U (using the fast C++ version)
  # We combine x and y once to avoid multiple allocations
  u_observed <- mw_u_logic(c(samp_x, samp_y), n_obs)
  
  # 3. Standard P-Value (Normal Approx)
  p_std <- mann_whitney_normal(samp_x, samp_y, u_observed)
  
  # 4. Permutation P-Value (using the C++ internal loop)
  p_perm <- mann_whitney_perm_fast(samp_x, samp_y, u_observed, n_perms)
  
  # 5. Return results
  return(c(
    standard    = p_std,
    permutation = p_perm
  ))
}

################################################
#### SIMULATION FOR INDEPENDENCE TESTING - MULTINOMIAL #######

simulation_independence_test <- function(n_obs = 100, 
                                         n_perms = 100,
                                         data_source = "real",
                                         x_data = NULL, y_data = NULL, 
                                         d_cats = 300, epsilon = 0.0
                                         ) {
  
  # 1. Sampling data
  if (data_source == "real") {
    # Sampling from finite pool of real data
    if (is.null(x_data) || is.null(y_data)) stop("Real data required for data_source='real'")
    samp_x <- sample(x_data, size = n_obs, replace = FALSE) 
    samp_y <- sample(y_data, size = n_obs, replace = FALSE) 
  
  } else if (data_source == "multi_uniform") {
    # uniform multinomial distribution
    p_matrix <- rep(1/d_cats, d_cats)
    samp_x <- sample(1:d_cats, size = n_obs, replace = TRUE, prob = p_matrix)
    samp_y <- sample(1:d_cats, size = n_obs, replace = TRUE, prob = p_matrix)
  
  } else if (data_source == "multi_perturb"){
    # small pertubances in the data in the first two cells
    p_x <- rep(1/d_cats, d_cats)
    p_y <- rep(1/d_cats, d_cats)
    
    p_y[1] <- p_y[1] + epsilon
    p_y[2] <- p_y[2] - epsilon

    samp_x <- sample(1:d_cats, size = n_obs, replace = TRUE, prob = p_x)
    samp_y <- sample(1:d_cats, size = n_obs, replace = TRUE, prob = p_y)

  } else if (data_source == "multi_dense") {
    # dense alternative - tiny shifts over all categories
    p_x <- rep(1/d_cats, d_cats)
    
    # note we are already starting a global seed with indexing in the sim_worker script
    # calling set.seed(NULL) guarantees that if we evaluate multiple iterations within a single worker call, the noise additions wouldn't get stuck duplicating themselves
    set.seed(NULL)  

    # Enforce zero-sum vector tracking constraints independently for each execution
    noise <- rnorm(d_cats, mean = 0, sd = 0.01)
    noise <- noise - mean(noise) 
    p_y   <- p_x + noise
    p_y   <- pmax(p_y, 0.00001) 
    p_y   <- p_y / sum(p_y)

    samp_x <- sample(1:d_cats, size = n_obs, replace = TRUE, prob = p_x)
    samp_y <- sample(1:d_cats, size = n_obs, replace = TRUE, prob = p_y)
  
  } else {
    stop(paste("Invalid data source configuration for multinomial testing logic:", data_source))
  }

  # convert sampling data to character
  # Ensure the inputs are character vectors to match our C++ types
  samp_x <- as.character(samp_x)
  samp_y <- as.character(samp_y)

  # 2. Build a 2-Row Contingency Table - This is the format R Needs for chisq.test to perform a test for homogeneity
  # Row 1 is Group X category counts, Row 2 is Group Y category counts
  all_possible_categories <- unique(c(samp_x, samp_y))
  pooled_factors <- factor(c(samp_x, samp_y), levels = all_possible_categories)
  group_indicator <- c(rep("Group_X", length(samp_x)), rep("Group_Y", length(samp_y)))
  contingency_table <- table(group_indicator, pooled_factors)
  
  # 3. Chi-Squared Standard (using R's highly optimized engine)
  # We wrap in suppressWarnings because chisq.test often warns about 
  # small cell counts, which can clutter logs.
  chisq_res <- suppressWarnings(chisq.test(contingency_table))
  p_chisq_std <- chisq_res$p.value
  
  # 4. Chi-Squared Simulated (B = n_perms)
  # This is R's internal Monte Carlo version of the Chi-Square test.
  p_chisq_sim <- suppressWarnings(
    chisq.test(contingency_table, simulate.p.value = TRUE, B = n_perms)$p.value
  )
  
  # 5. Multinomial U-Test (Using our fast C++ Permutation Wrapper)
  p_multi_u <- get_multinomial_u_p(samp_x, samp_y, n_perms)
  
  # 6. Return results
  return(c(
    chisq_standard  = p_chisq_std,
    chisq_simulated = p_chisq_sim,
    permutation_u   = p_multi_u
  ))
}