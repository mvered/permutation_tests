## Code to run simulations for two sample testing ##

#######################################
######### Set up ######################

# packages
library(tidyverse)

# import functions
source("R/calculate_statistics.R", local=FALSE)
source("R/simulation_functions.R", local=FALSE)

# set random seed for reproducibility
set.seed(04252025)

# global params
n_sims <- 500  
n_perms <- 500  
n_obs <- c(10, 20, 30, 50, 100) 
alpha <- 0.1

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

# initialize empty vectors to store Type II error rate for each tested sample size
error_t2_standard <- rep(NA, length(n_obs))
error_t2_perm <- rep(NA, length(n_obs))

# run the simulation for different sample sizes
for (j in 1:length(n_obs)){
  print(paste("sims for",n_obs[j],"observations"))
  standard <- rep(NA, n_sims)
  perm <- rep(NA, n_sims)
  
  # run the simulations
  for (i in 1:n_sims){
    # run one simulation
    temp <- simulation_two_sample(x = no_basement, y = with_basement, 
                                  n = n_obs[j], n_perms = n_perms, alpha=alpha)
    # save info on whether null was rejected or failed to reject
    standard[i] <- temp["standard"]
    perm[i] <- temp["permutation"]
  }
  
  # calculate Type II error rate for the Mann-Whitney U test that uses normal approximation for p-value
  error_standard <- (length(standard[standard == "fail to reject"])/n_sims)*100
  error_t2_standard[j] <- paste0(round(error_standard,2),"%")
  
  # calculate Type II error rate for the permutation Mann-Whitney u test
  error_perm <- (length(perm[perm == "fail to reject"])/n_sims)*100
  error_t2_perm[j] <- paste0(round(error_perm,2),"%")
}

# tally up type II errors for each sample size into data frame
two_sample_sims.type_2 <- data.frame(`Observations` = n_obs,
                                     `Standard` = error_t2_standard,
                                     `Permutation` = error_t2_perm)

# save simulation results
saveRDS(two_sample_sims.type_2, "output/two_sample_sims_type_2.rds")


##############################################################
#### Simulation where correct answer is fail to reject the null ###

print("Sims for Type I")

# initialize empty vectors to store Type II error rate for each tested sample size
error_t1_standard <- rep(NA, length(n_obs))
error_t1_perm <- rep(NA, length(n_obs))

# run the simulation for different sample sizes
for (j in 1:length(n_obs)){
  print(paste("sims for",n_obs[j],"observations"))
  standard <- rep(NA, n_sims)
  perm <- rep(NA, n_sims)
  
  # run the simulations
  for (i in 1:n_sims){
    temp <- simulation_two_sample(with_basement, with_basement, n = n_obs[j],
                                  n_perms = n_perms, alpha=alpha)
    standard[i] <- temp["standard"]
    perm[i] <- temp["permutation"]
  }
  
  # calculate Type I error rate for the Mann-Whitney U test that uses normal approximation for p-value
  error_standard <- (length(standard[standard == "reject"])/n_sims)*100
  error_t1_standard[j] <- paste0(round(error_standard,2),"%")
  
  # calculate Type I error rate for the permutation Mann-Whitney u test
  error_perm <- (length(perm[perm == "reject"])/n_sims)*100
  error_t1_perm[j] <- paste0(round(error_perm,2),"%")
}

# tally up type I errors for each sample size into data frame
two_sample_sims.type_1 <- data.frame(`Observations` = n_obs,
                                     `Standard` = error_t1_standard,
                                     `Permutation` = error_t1_perm)

# save simulation results
saveRDS(two_sample_sims.type_1, "output/two_sample_sims_type_1.rds")
