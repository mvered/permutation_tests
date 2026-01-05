#!/usr/bin/env Rscript

## functions to calculate test statistics used in simulations ##
## and their associated p-values ##

#######################################################
###### calculate mann-whitney p-value #################

# get p-value for mann-whitney u using permutation distribution
mann_whitney_perm_fast <- function(x, y, u_observed, n_perms) {
  pooled <- c(x, y)
  
  # Call C++ wrapper to get the entire distribution at once
  perm_dist <- mw_perm_dist_cpp(pooled, length(x), n_perms)
  
  # Calculate p-value
  p_value <- (sum(abs(perm_dist) >= abs(u_observed)) + 1) / (n_perms + 1)
  return(p_value)
}

# get p-value for mann-whitney u using normal approximation
mann_whitney_normal <- function(x, y, u_observed){
  n_x <- length(x)
  n_y <- length(y)
  
  mu <- (n_x*n_y)/2
  sigma <- sqrt((n_x*n_y*(n_x+n_y+1))/12)
  z_score <- (u_observed - mu)/sigma
  p_value <- 1-pnorm(abs(z_score))
  return(p_value)
}



###########################################################
####### multinomial u-statistic p-value ##################

# get p-value fast version
get_multinomial_u_p <- function(x, y, n_perms = 100) {
  # 1. Convert to character once to ensure C++ compatibility
  x_char <- as.character(x)
  y_char <- as.character(y)
  pooled <- c(x_char, y_char)
  
  # 2. Get observed statistic using C++
  u_observed <- multinomial_u_cpp(x_char, y_char)
  
  # 3. Get the entire permutation distribution in one C++ call
  # This replaces your 'for (i in 1:n_perms)' loop
  perm_dist <- get_perm_dist_cpp(pooled, length(x_char), n_perms)
  
  # 4. Standard p-value calculation
  p_value <- (sum(abs(perm_dist) >= abs(u_observed)) + 1) / (n_perms + 1)
  
  return(p_value)
}

