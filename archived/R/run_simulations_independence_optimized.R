#######################################
######### Set up ######################
library(tidyverse)
library(Rcpp)
library(future.apply)

start.time <- Sys.time()

# Global Params
n_sims <- 1 # 1000  # Increased for quality
n_perms <- 500 # 500   # Standard for robust p-values
n_obs <- c(10, 20, 30, 50, 100)

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
run_sim_suite <- function(method, label,
                          x_pool = NULL, y_pool = NULL,
                          dist_func = rnorm, ...) {
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
      
      p_vals <- simulation_independence_test(
        x_pool = x_pool, 
        y_pool = y_pool, 
        n_obs = n_val, 
        n_perms = n_perms)
      
      return(c(
        chisq_standard_05  = as.numeric(p_vals["chisq_standard"] < 0.05),
        chisq_standard_std_10  = as.numeric(p_vals["chisq_standard"] < 0.10),
        chisq_simulated_05  = as.numeric(p_vals["chisq_simulated"] < 0.05),
        chisq_simulated_10  = as.numeric(p_vals["chisq_simulated"] < 0.10),
        permutation_u_05 = as.numeric(p_vals["permutation_u"] < 0.05),
        permutation_u_10 = as.numeric(p_vals["permutation_u"] < 0.10)
      ))
      
    }, 
    future.seed = TRUE,
    future.packages = c("Rcpp", "tidyverse") 
    )
    
    sim_matrix <- do.call(cbind, sim_data)
    rates <- rowMeans(sim_matrix) # This calculates the proportion for each row
    
    return(data.frame(
      Observations = n_val,
      chisq_standard_05 = rates["chisq_standard_05"],
      chisq_standard_10 = rates["chisq_standard_10"],
      chisq_simulated_05 = rates["chisq_simulated_05"],
      chisq_simulated_10 = rates["chisq_simulated_10"],
      permutation_u_05 = rates["permutation_u_05"],
      permutation_u_10 = rates["permutation_u_10"],
    ))
  })
  
  bind_rows(results_list)
}

##############################################################
#### Run Both Simulations ####################################

# 1. TYPE II ERROR (Reject Null is Correct)
# We calculate (1 - Power) which is the Fail to Reject rate
type_2_results <- run_sim_suite(no_basement, with_basement, "Type II") |>
  mutate(across(ends_with("05"), ~ (1 - .)),
         across(ends_with("10"), ~ (1 - .))) # Error = 1 - Rejection Rate
rownames(type_2_results) <- NULL

# 2. TYPE I ERROR (Fail to Reject is Correct)
# Error = Rejection Rate
type_1_results <- run_sim_suite(with_basement, with_basement, "Type I")
rownames(type_1_results) <- NULL

##############################################################
#### Save and Format #########################################

saveRDS(type_2_results, "../output/raw_results_independence_type_2.rds")
saveRDS(type_1_results, "../output/raw_results_independence_type_1.rds")

message("Simulations complete. Files saved.")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken