#!/usr/bin/env python3


# 1. SET UP WITH AWS
# packages/functions
import boto3
import json
from aws_utils import submit_sim_array
from datetime import datetime

# aws setup
session = boto3.Session(profile_name='stats-research')
batch = session.client('batch', region_name='us-east-2')
MY_RESULTS_BUCKET = "stats-research"

# 2. DEFINE SCENARIOS FOR SIMULATION
N_PERMS = 10
JOB_COUNT = 2

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

# 3. create a unique folder name for this execution
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
RUN_FOLDER = f"run_{timestamp}_{N_PERMS}perms_{JOB_COUNT}sims"

# 4. TRIGGER ARRAY JOBS TO EXECUTE SIMULATIONS
if __name__ == "__main__":
    print("Initiating job submission to AWS Batch...")

    for s in SCENARIOS:
        try:
            response = submit_sim_array(
                test_name=s["test"], 
                n_size=s["n"], 
                bucket=MY_RESULTS_BUCKET,
                d1=s["d1"],
                d2=s["d2"],
                job_count = JOB_COUNT,
                n_perms = N_PERMS,
                run_folder = RUN_FOLDER
            )

            #print("--- AWS Response Received ---")
            #print(json.dumps(response, indent=4, default=str))
            #print("-----------------------------\n")

        except Exception as e:
            print(f"\nCRITICAL ERROR: Failed to submit scenario {s['test']}.")
            print(f"Error Details: {str(e)}\n")