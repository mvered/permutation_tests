#######################################
######### Set up ######################
library(tidyverse)
library(Rcpp)
library(future.apply)

# Global Params
n_sims <- 1000 # 1000  # Increased for quality
n_perms <- 500 # 500   # Standard for robust p-values
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

with_basement <- data |> filter(has_basement == TRUE) |> pull(zipcode)
no_basement <- data |> filter(has_basement == FALSE) |> pull(zipcode)

##############################################################
#### Optimized Simulation Runner Function ####################
run_sim_suite <- function(x_pool, y_pool, label) {
  message(paste("Starting simulation for:", label))
  
  results_list <- lapply(n_obs, function(n_val) {
    
    # We use future_lapply with specific settings to ignore the 'broken' globals
    sim_data <- future_lapply(1:n_sims, function(i) {
      
      # If the function doesn't exist in THIS worker's memory, load it
      if (!exists("simulation_two_sample")) {
        library(Rcpp)
        # Ensure these file paths are correct relative to your working directory
        source("calculate_statistics_optimized.R", local = TRUE)
        source("simulation_functions_optimized.R", local = TRUE)
        sourceCpp("stats_functions.cpp") 
      }
      
      simulation_independence_test(
        x_pool = x_pool, 
        y_pool = y_pool, 
        n = n_val, 
        n_perms = n_perms, 
        alpha = alpha_level
      )
    }, 
    future.seed = TRUE,
    future.packages = c("Rcpp", "tidyverse") 
    )
    
    sim_matrix <- do.call(cbind, sim_data)
    rates <- rowMeans(sim_matrix)
    
    return(data.frame(
      Observations = n_val,
      standard_rate = rates["chisq_standard"],
      simulated_rate = rates["chisq_simulated"],
      permutation_rate = rates["permutation_u"]
    ))
  })
  
  bind_rows(results_list)
}

##############################################################
#### Run Both Simulations ####################################

# 1. TYPE II ERROR (Reject Null is Correct)
# We calculate (1 - Power) which is the Fail to Reject rate
type_2_results <- run_sim_suite(no_basement, with_basement, "Type II") |>
  mutate(across(ends_with("Rate"), ~ (1 - .))) # Error = 1 - Rejection Rate
rownames(type_2_results) <- NULL

# 2. TYPE I ERROR (Fail to Reject is Correct)
# Error = Rejection Rate
type_1_results <- run_sim_suite(with_basement, with_basement, "Type I")
rownames(type_1_results) <- NULL

##############################################################
#### Save and Format #########################################

# Helper to format as percentage strings for your report
format_pct <- function(x) paste0(round(x * 100, 2), "%")

saveRDS(type_2_results, "../output/raw_results_independence_type_2.rds")
saveRDS(type_1_results, "../output/raw_results_independence_type_1.rds")

independence_sims_type_2 <- type_2_results |>
  mutate(across(ends_with("Rate"), format_pct)) |>
  rename(`Standard Chi-Squared Error Rate` = standard_rate,
         `Bootstrap Chi-Squared Error Rate` = simulated_rate,
         `Multinomial U Permutation Error Rate` = permutation_rate)
independence_sims_type_1 <- type_1_results |>
  mutate(across(ends_with("Rate"), format_pct)) |>
  rename(`Standard Chi-Squared Error Rate` = standard_rate,
         `Bootstrap Chi-Squared Error Rate` = simulated_rate,
         `Multinomial U Permutation Error Rate` = permutation_rate)
saveRDS(independence_sims_type_2, "../output/independence_sims_type_2.rds")
saveRDS(independence_sims_type_1, "../output/independence_sims_type_1.rds")

message("Simulations complete. Files saved.")