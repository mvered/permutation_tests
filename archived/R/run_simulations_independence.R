## Code to run simulations for two sample testing ##

#######################################
######### Set up ######################

# packages
library(tidyverse)

# import functions
source("R/calculate_statistics.R")
source("R/simulation_functions.R")

# set random seed for reproducibility
set.seed(05072025)

# global params
n_sims <- 100 # 500  
n_perms <- 100 # 500 
n_obs <- c(10, 20) # c(10, 20, 30, 50, 100) 
alpha <- 0.1
n_cores <- 7

######################################
#### Data import and cleaning #######

# read in data and split into 2 samples
data <- read.csv("data/home_data.csv") |>
  
  # add has basement/no basement column
  mutate(has_basement = case_when(
    sqft_basement > 0 ~ TRUE, 
    sqft_basement == 0 ~ FALSE)
  )

# set up for sampling procedure
with_basement <- data |>
  filter(has_basement == TRUE)
no_basement <- data |>
  filter(has_basement == FALSE)


##############################################################
#### Simulation where correct answer is to reject the null ###

print("Sims for Type II")

# run a simulation where the correct answer is to reject the null
error_t2_chisq_standard <- rep(NA, length(n_obs))
error_t2_chisq_simulated <- rep(NA, length(n_obs))
error_t2_perm <- rep(NA, length(n_obs))

# run the simulation for different sample sizes
for (j in 1:length(n_obs)){
  chisq_standard <- rep(NA, n_sims)
  chisq_simulated <- rep(NA, n_sims)
  perm <- rep(NA, n_sims)
  
  # run the simulations
  for (i in 1:n_sims){
    # run one simulation
    temp <- simulation_independence(x = no_basement$zipcode, 
                                    y = with_basement$zipcode, 
                                    n = n_obs[j], n_perms = n_perms, 
                                    alpha=alpha)
    # save info on whether null was rejected or failed to reject
    chisq_standard[i] <- temp["chisq_standard"]
    chisq_simulated[i] <- temp["chisq_simulated"]
    perm[i] <- temp["permutation"]
  }
  
  # Error rate for standard chi-square test
  error_chisq_standard <- (length(chisq_standard[chisq_standard== "fail to reject"])/
                             n_sims)*100
  error_t2_chisq_standard[j] <- paste0(round(error_chisq_standard,2),"%")
  
  # Error rate for chi-squared test with bootstrap p-value
  error_chisq_simulated <- (length(chisq_simulated[chisq_simulated== "fail to reject"])/
                              n_sims)*100
  error_t2_chisq_simulated[j] <- paste0(round(error_chisq_simulated,2),"%")
  
  # Error rate for permuation test
  error_perm <- (length(perm[perm == "fail to reject"])/n_sims)*100
  error_t2_perm[j] <- paste0(round(error_perm,2),"%")
}

# tally up type II errors
independence_sims.type_2 <- data.frame(`Observations` = n_obs,
                                       `Standard Chi-Square` = error_t2_chisq_standard,
                                       `Bootstrap Chi-Square` = error_t2_chisq_simulated,
                                       `Multinomial U Permutation` = error_t2_perm)

# save simulation results
saveRDS(independence_sims.type_2, "output/independence_sims_type_2.rds")

##############################################################
#### Simulation where correct answer is fail to reject the null ###

print("Sims for Type I")

# initialize empty vectors to store type I error rate for each sample size
error_t1_chisq_standard <- rep(NA, length(n_obs))
error_t1_chisq_simulated <- rep(NA, length(n_obs))
error_t1_perm <- rep(NA, length(n_obs))

# run the simulation for different sample sizes
# run the simulation for different sample sizes
for (j in 1:length(n_obs)){
  chisq_standard <- rep(NA, n_sims)
  chisq_simulated <- rep(NA, n_sims)
  perm <- rep(NA, n_sims)
  
  # run simulations
  for (i in 1:n_sims){
    # run one simulation
    temp <- simulation_independence(x = with_basement$zipcode, 
                                    y = with_basement$zipcode, 
                                    n = n_obs[j], n_perms = n_perms,
                                    alpha=alpha)
    # store whether we rejected or failed to reject the null
    chisq_standard[i] <- temp["chisq_standard"]
    chisq_simulated[i] <- temp["chisq_simulated"]
    perm[i] <- temp["permutation"]
  }
  
  # calculate Type I error rate
  error_chisq_standard <- (length(chisq_standard[chisq_standard== "reject"])/
                             n_sims)*100
  error_t1_chisq_standard[j] <- paste0(round(error_chisq_standard,2),"%")
  
  error_chisq_simulated <- (length(chisq_simulated[chisq_simulated== "reject"])/
                              n_sims)*100
  error_t1_chisq_simulated[j] <- paste0(round(error_chisq_simulated,2),"%")
  
  error_perm <- (length(perm[perm == "reject"])/n_sims)*100
  error_t1_perm[j] <- paste0(round(error_perm,2),"%")
}

# tally up type II errors
independence_sims.type_1 <- data.frame(`Observations` = n_obs,
                                       `Standard Chi-Square` = error_t1_chisq_standard,
                                       `Bootstrap Chi-Square` = error_t1_chisq_simulated,
                                       `Multinomial U Permutation` = error_t1_perm)


# save simulation results
saveRDS(independence_sims.type_1, "output/independence_sims_type_1.rds")
