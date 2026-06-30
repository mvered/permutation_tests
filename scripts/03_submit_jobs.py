#!/usr/bin/env python3

# 1. SET UP WITH AWS
import boto3
batch = boto3.client('batch', region_name='us-east-2')
MY_RESULTS_BUCKET = "stats-research"

# 2. DEFINE SCENARIOS FOR SIMULATION
SCENARIOS = [
    {
        "test": "twosample", 
        "n": 100, 
        "d1": "price_no_basement.rds", 
        "d2": "price_with_basement.rds"
    },
    {
        "test": "twosample", 
        "n": 100, 
        "d1": "price_no_basement.rds", 
        "d2": "price_no_basement.rds"
    },
    {
        "test": "independence", 
        "n": 500, 
        "d1": "home_data_x.rds", 
        "d2": "home_data_y.rds"
    },
    {
        "test": "independence", 
        "n": 500, 
        "d1": "home_data_x.rds", 
        "d2": "home_data_x.rds"
    }
]

# 3. TRIGGER ARRAY JOBS TO EXECUTE SIMULATIONS
if __name__ == "__main__":
    for s in SCENARIOS:
        submit_sim_array(
            test_name=s["test"], 
            n_size=s["n"], 
            bucket=MY_RESULTS_BUCKET,
            d1=s["d1"],
            d2=s["d2"]
        )