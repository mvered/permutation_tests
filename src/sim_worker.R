#!/usr/bin/env Rscript

### Calls/executes one simulation using environment vars

# 1. LOAD LIBRARIES
# Base R and Rcpp for maximum speed and minimum Docker bloat
library(Rcpp)
library(optparse)

# 2. PARSE ARGUMENTS
# Passed from entrypoint.sh (e.g., --index 42 --test twosample --n_obs 50)
option_list <- list(
  make_option(c("-i", "--index"), type = "integer", default = 0, help = "Job index for seeding"),
  make_option(c("-t", "--test"), type = "character", default = "twosample", help = "Test type"),
  make_option(c("-n_o", "--n_obs"), type = "integer", default = 10, help = "Sample size"),
  make_option(c("-n_p","n_perms"), type="integer", default=500, help="Num permutations for distribution"),
  make_option(c("-d1","--data1"), type="character", default = "home_data.rds", help="data for 1st sample"),
  make_option(c("-d2","--data2"), type="character", default = "home_data.rds", help="data for 2nd sample")
)
opt <- parse_args(OptionParser(option_list = option_list))

# 3. ENVIRONMENT SETUP
# Set the seed based on the job index to ensure reproducibility across 1,000 jobs
set.seed(opt$index)

# Load helper functions and pre-compiled C++ logic
# Note: Paths are relative to the /app/src/ directory in Docker
source("calculate_p.R")
source("one_simulation.R")
sourceCpp("stats_functions.cpp")

# 4. LOAD DATA POOL
data1 <- readRDS(paste0("../data/",opt$data1))
data2 <- readRDS(paste0("../data/",opt$data2))

# 5. EXECUTION LOGIC
# We run exactly ONE simulation iteration per job
message(sprintf("Running %s simulation [Index: %d, N_Obs: %d]", opt$test, opt$index, opt$n_obs))

if (opt$test == "twosample") {
  # Two-sample comparison (e.g., with_basement vs no_basement)
  p_vals <- simulation_two_sample(
    n_obs = opt$n_obs,
    n_perms = opt$n_perms,
    method = "pool",
    x_pool  = data1, 
    y_pool  = data2
  )
  
  # Format results for CSV
  results_row <- data.frame(
    job_index      = opt$index,
    test_type      = "twosample",
    n_obs          = opt$n_obs,
    n_perms        = opt$n_perms,
    p_val_std      = p_vals["standard"],
    p_val_perm     = p_vals["permutation"],
    reject_std_05  = as.numeric(p_vals["standard"] < 0.05),
    reject_perm_05 = as.numeric(p_vals["permutation"] < 0.05),
    reject_std_10  = as.numeric(p_vals["standard"] < 0.10),
    reject_perm_10 = as.numeric(p_vals["permutation"] < 0.10)
  )
  
} else if (opt$test == "independence") {
  # Independence testing logic
  p_vals <- simulation_independence_test(
    n_obs = opt$n_obs,
    n_perms = opt$n_perms,
    method = "pool",
    x_pool  = data1, 
    y_pool  = data2
  )
  
  results_row <- data.frame(
    job_index      = opt$index,
    test_type      = "independence",
    n_obs          = opt$n_obs,
    n_perms        = opt$n_perms,
    p_val_chisq_std= p_vals["chisq_standard"],
    p_val_chisq_sim= p_vals["chisq_simulated"],
    p_val_chisq_sim= p_vals["permutation_u"],
    reject_chisq_std_05   = as.numeric(p_vals["chisq_standard"] < 0.05),
    reject_chisq_sim_05   = as.numeric(p_vals["chisq_simulated"] < 0.05),
    reject_perm_u_05      = as.numeric(p_vals["permutation_u"] < 0.05),
    reject_chisq_std_10   = as.numeric(p_vals["chisq_standard"] < 0.10),
    reject_chisq_sim_10   = as.numeric(p_vals["chisq_simulated"] < 0.10),
    reject_perm_u_10      = as.numeric(p_vals["permutation_u"] < 0.10)
  )
} else {
  stop(paste("Unknown test type:", opt$test))
}

# 6. EXPORT
# Create a unique filename that includes parameters to avoid S3 collisions
output_file <- sprintf("sim_out_%s_n%d_idx%d.csv", opt$test, opt$n_obs, opt$index)
write.csv(results_row, file = output_file, row.names = FALSE)

message(sprintf("Successfully saved: %s", output_file))