#### Prep data for inclusion in dockerfile

# set up
library(tidyverse)

# read data
home_data <- read_csv("data/home_data.csv") |>
  mutate(has_basement = sqft_basement > 0)

# get just the parts of the data we want to make the files as small as possible
zipcodes.with_basement <- home_data |> filter(has_basement == TRUE) |> pull(zipcode)
zipcodes.no_basement <- home_data |> filter(has_basement == FALSE) |> pull(zipcode)
price.with_basement <- home_data |> filter(has_basement == TRUE) |> pull(price)
price.no_basement <- home_data |> filter(has_basement == FALSE) |> pull(price)

# save as RDS files
write_rds(zipcodes.with_basement, "data/zipcodes_with_basement.rds")
write_rds(zipcodes.no_basement, "data/zipcodes_no_basement.rds")
write_rds(price.with_basement, "data/price_with_basement.rds")
write_rds(price.no_basement, "data/price_no_basement.rds")

# clear workspace
rm(home_data)