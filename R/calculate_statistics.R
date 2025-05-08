## functions to calculate test statistics used in simulations ##
## and their associated p-values ##
library(foreach)
library(doParallel)

#######################################################
##### mann-whitney u statistic #######

# calculate mann-whitney u statistic
mann_whitney_u <- function(x, y){
  n_x <- length(x)
  n_y <- length(y)
  pooled <- c(x, y)
  
  ranks <- rank(pooled, ties.method = "average")
  #u_x <- sum(ranks[1:n_x]) -  (n_x*(n_x+1))/2
  #u_y <- sum(ranks[(n_x+1):(n_x+n_y)]) - (n_y*(n_y+1))/2
  u_x <- n_x*n_y + (n_x*(n_x+1))/2 - sum(ranks[1:n_x])
  #u_y <- n_x*n_y + (n_y*(n_y+1))/2 - sum(ranks[(n_x+1):(n_x+n_y)])
  return(u_x)
}

#######################################################
###### calculate mann-whitney p-value #################

# get p-value for mann-whitney u using permutation distribution
mann_whitney_perm <- function(x, y, u_observed, n_perms){
  
  #n_perms <- 1000
  pooled <- c(x, y)
  perm_dist <- rep(NA, n_perms)
  
  for (i in 1:n_perms){
    permuted_data <- sample(pooled, replace = FALSE)
    x_star <- permuted_data[1:length(x)]
    y_star <- permuted_data[(length(x)+1):length(pooled)]
    perm_dist[i] <- mann_whitney_u(x_star, y_star)
  }
  p_value <-(sum(abs(perm_dist) >= abs(u_observed)) + 1) / (n_perms + 1)
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


#####################################################################
###### new multinomial U-test statistic proposed by Kim et al ########

# this is functionally the same thing as sum_{k=1}^d I(x=k)I(y=k)
# x and y should be individual observations
# page 235 of original paper
g_multi <- function(x, y){
  if (x == y){
    return(1)
  } else if (x != y){
    return(0)
  }
}

# kernel function
# takes as inputs 2 element tuples from each distribution 
# page 234 of original paper
h_ts <- function(y1,y2,z1,z2){
  total <- g_multi(y1,y2) + g_multi(z1,z2) - g_multi(y1,z2) - g_multi(y2,z1)
  return(total)
}

# calculate u statistic
# expects vectors of sample values (categorical data)
# page 234 of original paper
multinomial_u <- function(sample1, sample2){
  
  # get initial part of u statistic
  n1 <- length(sample1)
  n2 <- length(sample2)
  frac <- 1/(n1*(n1-1)*n2*(n2-1))
  
  # get tuples
  y_tuples <- combn(sample1, 2, simplify=TRUE) 
  z_tuples <- combn(sample2, 2, simplify=TRUE)
  
  # keep running sum
  sum <- 0
  
  # outer sum
  for (i in 1:ncol(y_tuples)){
    
    # inner sum
    for (j in 1:ncol(z_tuples)){
      
      # run kernel function
      sum <- sum + h_ts(y_tuples[1,i], y_tuples[2,i],
                        z_tuples[1,j], z_tuples[2, j])
    }
    
  }
  
  # return total u statistic
  return(frac*sum)
  
}

###########################################################
####### multinomial u-statistic p-value ##################

# get p-value for multinomial u using permutation distribution
get_multinomial_u_p <- function(x, y, n_perms=100){
  
  # observed sample statistic
  u_observed <- multinomial_u(x, y)
  
  # set up for permutation distribution
  pooled <- c(x, y)
  
  # Set your number of cores
  cl <-  makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  # Run permutations in parallel
  perm_dist <- foreach(i=1:n_perms, .combine=c, 
                       .export=c("multinomial_u","h_ts","g_multi")) %dopar% {
    permuted_data <- sample(pooled, replace = FALSE)
    x_star <- permuted_data[1:length(x)]
    y_star <- permuted_data[(length(x)+1):length(pooled)]
    multinomial_u(x_star, y_star)
  }
  
  p_value <-(sum(abs(perm_dist) >= abs(u_observed)) + 1) / (n_perms + 1)
  print(p_value)
  stopCluster(cl)
  return(p_value)
}


