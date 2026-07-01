######
# Initial EDA of Simulation Results
######

### Set up
# packages
library(tidyverse)

##############
# Testing Data
##############
# Make sure data is outputted from simulation code correctly before running the full set of sims

# Read in data
two_sample <- read_csv("results/raw_simulation_output/run_20260701_105842_2sims/twosample_run_20260701_105842_2sims.csv")
independence <- read_csv("results/raw_simulation_output/run_20260701_105842_2sims/independence_run_20260701_105842_2sims.csv")

# some helper columns
# separate by scenario and whether we are looking for frequency of type I error or type II error
two_sample <- two_sample |>
  mutate(scenario = case_when(
    data_source == "real" & x_data != y_data ~ "real_two_populations",
    data_source == "real" & x_data == y_data ~ "real_one_population",
    TRUE ~ data_source
  ),
  error_type = case_when(
    scenario %in% c("real_one_population", "normal", "t_dist") ~ "Type_I",
    scenario %in% c("real_two_populations", "normal_shift", "t_dist_shift") ~ "Type_II"
  ),
  data_source_type = case_when(
    data_source == "real" ~ "real",
    TRUE ~ "theoretical"
  )
  )

independence <- indepdence |>
  mutate(scenario == case_when(
    data_source == "real" & x_data != y_data ~ "real_two_populations",
    data_source == "real" & x_data == y_data ~ "real_one_population",
    TRUE ~ data_source
  ),
  error_type = case_when(
    scenario %in% c("real_one_population", "multi_uniform") ~ "Type_I",
    scenario %in% c("real_two_populations", "multi_perturb", "multi_dense") ~ "Type_II"
  ),
  data_source_type = case_when(
    data_source == "real" ~ "real",
    TRUE ~ "theoretical"
  )
  )

# Look at sims by scenario
two_sample |>
  count(scenario) 

# make sure the mu and shift look right
two_sample |>
  group_by(scenario) |>
  summarize(mu_min = min(mu), 
            mu_max = max(mu),
            shift_min = min(shift),
            shift_max = max(shift))