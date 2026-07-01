#!/usr/bin/env Rscript

### Calls/executes one simulation using environment vars

# 1. LOAD LIBRARIES
# Base R and Rcpp for maximum speed and minimum Docker bloat
library(Rcpp)
library(optparse)

# 2. PARSE ARGUMENTS
# Passed from entrypoint.sh (e.g., --index 42 --test twosample --n_obs 50)
option_list <- list(
  make_option(c("-i", "--index"), type = "integer", 
              default = 0, help = "Job index for seeding"),
  make_option(c("-t", "--test"), type = "character", 
              default = "twosample", help = "Test type, two sample or independence"),
  make_option(c("-o", "--n_obs"), type = "integer", 
              default = 10, help = "Sample size"),
  make_option(c("-p","--n_perms"), type="integer", 
              default=500, help="Num permutations for distribution"),
  make_option(c("-s", "--data_source"), type = "character", 
              default = "real", help="real data or specific theoretical distribution"),

  make_option(c("-d","--data1"), type="character", 
              default = "home_data.rds", help="data for 1st sample"),
  make_option(c("-e","--data2"), type="character", 
              default = "home_data.rds", help="data for 2nd sample"),

  make_option(c("-c", "--d_cats"), type = "integer", default = 300,
              help="number of categories for multinomial distribution"),
  make_option(c("-m", "--mu"), type = "double", default = 0.0,
              help="mean for normal distribution"),
  make_option(c("-f", "--shift"), type = "double", default = 0.0,
              help="shift for mean of t or normal distribution"),
  make_option(c("-k", "--epsilon"), type = "double", default = 0.0,
              help="pertubation for multinomial uniform distribution")
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

# 4. LOAD REAL DATA, IF USING
if (opt$data_source == "real"){
  data1_path <- paste0("../data/", opt$data1)
  data2_path <- paste0("../data/", opt$data2)
  if (!file.exists(data1_path) || !file.exists(data2_path)) {
    stop(sprintf("Data files missing for real scenario evaluation path check: %s or %s", data1_path, data2_path))
  }
  data1 <- readRDS(data1_path)
  data2 <- readRDS(data2_path)
}
else {
  data1 <- NULL
  data2 <- NULL
}

# 5. EXECUTION LOGIC
# We run exactly ONE simulation iteration per job
message(sprintf("Running %s simulation [Source: %s, Index: %d, N_Obs: %d]", 
                opt$test, opt$data_source, opt$index, opt$n_obs))

if (opt$test == "twosample") {
  # Two-sample comparison (e.g., with_basement vs no_basement)
  p_vals <- simulation_two_sample(
    n_obs       = opt$n_obs,
    n_perms     = opt$n_perms,
    data_source = opt$data_source,
    mu          = opt$mu,
    shift       = opt$shift,
    x_data      = data1, 
    y_data      = data2
  )
  
  # Format results for CSV
  results_row <- data.frame(
    job_index      = opt$index,
    test_type      = "twosample",
    data_source    = opt$data_source,
    n_obs          = opt$n_obs,
    n_perms        = opt$n_perms,
    mu             = opt$mu,
    shift          = opt$shift,
    p_val_std      = p_vals["standard"],
    p_val_perm     = p_vals["permutation"]
  )
  
} else if (opt$test == "independence") {
  # Independence testing logic
  p_vals <- simulation_independence_test(
    n_obs       = opt$n_obs,
    n_perms     = opt$n_perms,
    data_source = opt$data_source,
    d_cats.     = opt$d_cats,
    epsilon.    = opt$epsilon,
    x_data      = data1, 
    y_data      = data2
  )
  
  results_row <- data.frame(
    job_index           = opt$index,
    test_type           = "independence",
    data_source         = opt$data_source,
    n_obs               = opt$n_obs,
    n_perms             = opt$n_perms,
    p_val_chisq_std     = p_vals["chisq_standard"],
    p_val_chisq_sim     = p_vals["chisq_simulated"],
    p_val_perm_u        = p_vals["permutation_u"]
  )
} else {
    stop(paste("Unknown test type:", opt$test))
}

# 6. EXPORT
# Create a unique filename that includes parameters to avoid S3 collisions
output_file <- sprintf("sim_out_%s_%s_n%d_idx%d.csv", opt$test, opt$data_source, opt$n_obs, opt$index)
write.csv(results_row, file = output_file, row.names = FALSE)

message(sprintf("Successfully saved: %s", output_file))