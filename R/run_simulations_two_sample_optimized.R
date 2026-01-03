#######################################
######### Set up ######################
library(tidyverse)
library(Rcpp)
library(future.apply)

# Global Params
n_sims <- 10 #1000   
n_perms <- 10 #500  
n_obs <- c(10, 20, 30, 50, 100)
alpha_level <- 0.05

# Parallel Setup
if (.Platform$OS.type == "unix") {
  plan(multicore) # Optimal for AWS/GCP (Linux)
} else {
  plan(multisession) # Windows
}

######################################
#### Data import and cleaning #######

data <- read.csv("../data/home_data.csv") |>
  mutate(has_basement = sqft_basement > 0)

# Pull just the prices once to minimize data transfer to workers
with_basement_prices <- data |> filter(has_basement == TRUE) |> pull(price)
no_basement_prices <- data |> filter(has_basement == FALSE) |> pull(price)

##############################################################
#### Runner Function #########################################

run_two_sample_suite <- function(x_pool, y_pool, label) {
  message(paste("Starting Two-Sample simulation for:", label))
  
  results_list <- lapply(n_obs, function(n_val) {
    
    # Parallel execution
    sim_data <- future_lapply(1:n_sims, function(i) {
      # --- THE FIX STARTS HERE ---
      # Force the worker to load the functions if they are missing
      if (!exists("simulation_two_sample")) {
        library(Rcpp)
        # Ensure these file paths are correct relative to your working directory
        source("calculate_statistics_optimized.R", local = TRUE)
        source("simulation_functions_optimized.R", local = TRUE)
        sourceCpp("stats_functions.cpp") 
      }
      # --- THE FIX ENDS HERE ---
      
      simulation_two_sample(
        x_pool = x_pool, 
        y_pool = y_pool, 
        n = n_val, 
        n_perms = n_perms, 
        alpha = alpha_level
      )
    }, 
    future.seed = TRUE,
    future.packages = c("Rcpp","tidyverse"))
    
    # Process results (means of 1/0 rejections)
    sim_matrix <- do.call(cbind, sim_data)
    rates <- rowMeans(sim_matrix)
    
    return(data.frame(
      Observations = n_val,
      standard_rate = rates["standard"],
      permutation_rate = rates["permutation"]
    ))
  })
  
  bind_rows(results_list)
}

##############################################################
#### Run and Save ############################################

# Format helper
format_pct <- function(x) paste0(round(x * 100, 2), "%")

# 1. TYPE II ERROR (Different distributions: Null is False)
# Error = 1 - Rejection Rate (1 - Power)
two_sample_type_2 <- run_two_sample_suite(no_basement_prices, with_basement_prices, "Type II") |>
  mutate(across(ends_with("_rate"), ~ (1 - .)))
rownames(two_sample_type_2) <- NULL

# 2. TYPE I ERROR (Same distributions: Null is True)
# Error = Rejection Rate
two_sample_type_1 <- run_two_sample_suite(with_basement_prices, with_basement_prices, "Type I")
rownames(two_sample_type_1) <- NULL

# Apply formatting and save
raw_results_two_sample_type_2 <- two_sample_type_2
saveRDS(raw_results_two_sample_type_2, "../output/raw_results_two_sample_type_2.rds")
raw_results_two_sample_type_1 <- two_sample_type_1
saveRDS(raw_results_two_sample_type_1, "../output/raw_results_two_sample_type_1.rds")

two_sample_type_2 <- two_sample_type_2 |> 
  mutate(across(ends_with("_rate"), format_pct)) |>
  rename(`Standard Mann-Whitney U Error Rate` = standard_rate,
         `Permutation Mann-Whitney U Error Rate` = permutation_rate)

two_sample_type_1 <- two_sample_type_1 |> 
  mutate(across(ends_with("_rate"), format_pct)) |>
  rename(`Standard Mann-Whitney U Error Rate` = standard_rate,
         `Permutation Mann-Whitney U Error Rate` = permutation_rate)

saveRDS(two_sample_type_2, "../output/two_sample_sims_type_2.rds")
saveRDS(two_sample_type_1, "../output/two_sample_sims_type_1.rds")

message("Two-sample simulations complete.")