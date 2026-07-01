#!/usr/bin/env python3


# 1. SET UP 
# packages/functions
import boto3
import time
import os
from aws_utils import submit_sim_array
from datetime import datetime

# aws setup
session = boto3.Session(profile_name='stats-research')
batch = session.client('batch', region_name='us-east-2')
MY_RESULTS_BUCKET = "stats-research"

# ==========================================
# SCENARIOS AND PARAMS
# ==========================================

# 2. GLOBAL PARAMETERS FOR SIMULATION
JOB_COUNT = 2            # How many simulations (eventually want 5000)
PERMS_TWOSAMPLE = 10     # permutations for two-sample tests (eventually want 1000)
PERMS_INDEPENDENCE = 10  # permutations for independence tests (eventually want 500, lower due to computational complexity)

# 3. STARTING SCENARIOS FOR SIMULATION (real and theoretical)
SCENARIOS = [

    # =========================================================================
    # PART 1: TWO-SAMPLE TESTING (Mann-Whitney U vs. Permutation)
    # =========================================================================
    # Scenario 1: Symmetric Normal Baseline
    {"test": "twosample", "n": 20, "data_source": "normal", "n_perms": PERMS_TWOSAMPLE},
    {"test": "twosample", "n": 20, "data_source": "normal_shift", "shift": 0.5, "n_perms": PERMS_TWOSAMPLE},
    {"test": "twosample", "n": 50, "data_source": "normal", "n_perms": PERMS_TWOSAMPLE},
    {"test": "twosample", "n": 50, "data_source": "normal_shift", "shift": 0.5, "n_perms": PERMS_TWOSAMPLE},
    
    # Scenario 2: Heavy-Tail Collapse
    {"test": "twosample", "n": 15, "data_source": "t_dist", "n_perms": PERMS_TWOSAMPLE},
    {"test": "twosample", "n": 15, "data_source": "t_dist_shift", "shift": 0.6, "n_perms": PERMS_TWOSAMPLE},
    {"test": "twosample", "n": 40, "data_source": "t_dist", "n_perms": PERMS_TWOSAMPLE},
    {"test": "twosample", "n": 40, "data_source": "t_dist_shift", "shift": 0.6, "n_perms": PERMS_TWOSAMPLE},
    
    # Scenario 3: Real-World Housing Price Skewness
    {"test": "twosample", "n": 30, "data_source": "real", "d1": "price_no_basement.rds", "d2": "price_no_basement.rds", "n_perms": PERMS_TWOSAMPLE},
    {"test": "twosample", "n": 30, "data_source": "real", "d1": "price_no_basement.rds", "d2": "price_with_basement.rds", "n_perms": PERMS_TWOSAMPLE},
    {"test": "twosample", "n": 100, "data_source": "real", "d1": "price_no_basement.rds", "d2": "price_no_basement.rds", "n_perms": PERMS_TWOSAMPLE},
    {"test": "twosample", "n": 100, "data_source": "real", "d1": "price_no_basement.rds", "d2": "price_with_basement.rds", "n_perms": PERMS_TWOSAMPLE},

    # =========================================================================
    # PART 2: CATEGORICAL INDEPENDENCE & MULTINOMIAL TESTING
    # =========================================================================
    # Scenario 7: Real-World Zip Code Simulation
    {"test": "independence", "n": 20, "data_source": "real", "d1": "home_data_x.rds", "d2": "home_data_x.rds", "n_perms": PERMS_INDEPENDENCE},
    {"test": "independence", "n": 20, "data_source": "real", "d1": "home_data_x.rds", "d2": "home_data_y.rds", "n_perms": PERMS_INDEPENDENCE},
    {"test": "independence", "n": 40, "data_source": "real", "d1": "home_data_x.rds", "d2": "home_data_x.rds", "n_perms": PERMS_INDEPENDENCE},
    {"test": "independence", "n": 40, "data_source": "real", "d1": "home_data_x.rds", "d2": "home_data_y.rds", "n_perms": PERMS_INDEPENDENCE},
    {"test": "independence", "n": 60, "data_source": "real", "d1": "home_data_x.rds", "d2": "home_data_x.rds", "n_perms": PERMS_INDEPENDENCE},
    {"test": "independence", "n": 60, "data_source": "real", "d1": "home_data_x.rds", "d2": "home_data_y.rds", "n_perms": PERMS_INDEPENDENCE}
]

# 4. DYNAMICALLY GENERATE THE 15-CONFIGURATION GRID MATRIX FOR SCENARIOS 4, 5, AND 6
n_grid = [20, 40, 60]
d_grid = [10, 50, 100, 300, 500]

for n in n_grid:
    for d in d_grid:
        # Scenario 4: The High-Dimensional Sparse Null
        SCENARIOS.append({
            "test": "independence", "n": n, "data_source": "multi_uniform", 
            "d_cats": d, "n_perms": PERMS_INDEPENDENCE
        })
        
        # Scenario 5: The Sparse Localized Alternative (Epsilon = 0.06)
        SCENARIOS.append({
            "test": "independence", "n": n, "data_source": "multi_perturb", 
            "d_cats": d, "epsilon": 0.06, "n_perms": PERMS_INDEPENDENCE
        })
        
        # Scenario 6: The Dense, Low-Signal Alternative
        SCENARIOS.append({
            "test": "independence", "n": n, "data_source": "multi_dense", 
            "d_cats": d, "n_perms": PERMS_INDEPENDENCE
        })

# 5. CREATE RUN ENVIRONMENT FOLDER
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
RUN_FOLDER = f"run_{timestamp}_{JOB_COUNT}sims"


# ==========================================
# 6. TRIGGER ARRAY JOBS TO EXECUTE SIMULATIONS
# ==========================================
if __name__ == "__main__":
    print("Initiating job submission to AWS Batch...")

    submitted_jobs = []
    total_attempted = len(SCENARIOS)  # Track the total number of attempted jobs

    for s in SCENARIOS:
        try:
            d1_val = s.get("d1", "home_data.rds")
            d2_val = s.get("d2", "home_data.rds")
            d_cats_val = s.get("d_cats", 300)
            mu_val = s.get("mu", 0.0)
            shift_val = s.get("shift", 0.0)
            epsilon_val = s.get("epsilon", 0.0)

            response = submit_sim_array(
                test_name=s["test"], 
                n_size=s["n"], 
                bucket=MY_RESULTS_BUCKET,
                d1=d1_val,
                d2=d2_val,
                job_count=JOB_COUNT,
                n_perms=s["n_perms"],
                data_source=s["data_source"],
                run_folder=RUN_FOLDER,
                d_cats=d_cats_val,
                mu=mu_val,
                shift=shift_val,
                epsilon=epsilon_val
            )
            
            if "jobId" in response:
                submitted_jobs.append({
                    "id": response["jobId"],
                    "name": response["jobName"],
                    "scenario": s
                })

        except Exception as e:
            print(f"AWS SUBMISSION CRASH: Config rejected by API: {s['test']} ({s['data_source']})")
            print(f"Details: {str(e)}\n")

    # Dynamically reports successfully submitted jobs out of total attempted
    print(f"All {len(submitted_jobs)}/{total_attempted} jobs pushed to queue. Monitoring containers...")

    # ==========================================
    # 7. LIFECYCLE MONITORING LOOP (Outputs to log ONLY on Fargate/R/C++ container errors)
    # ==========================================

    # Set log file path
    log_dir = "results/logs"
    if not os.path.exists(log_dir):
        os.makedirs(log_dir)     
    log_file_path = os.path.join(log_dir, f"simulation_errors_{timestamp}.log")
    
    # Open log file and initialize header text
    with open(log_file_path, "w") as log_file:
        log_file.write(f"=== SIMULATION TASK RUNTIME ERROR LOG | EXECUTION: {RUN_FOLDER} ===\n\n")

    # Track distinct statuses for active reporting
    completed_jobs = set()
    failed_count = 0
    
    while len(completed_jobs) < len(submitted_jobs):
        # Calculate remaining jobs for the console tracker
        remaining_count = len(submitted_jobs) - len(completed_jobs)
        print(f"[{datetime.now().strftime('%H:%M:%S')}] Status Update: {remaining_count} jobs still PENDING/RUNNING | {len(completed_jobs) - failed_count} SUCCEEDED | {failed_count} FAILED")
        
        time.sleep(30)
        
        for job in submitted_jobs:
            if job["id"] in completed_jobs:
                continue
                
            try:
                status_resp = batch.describe_jobs(jobs=[job["id"]])
                if not status_resp.get("jobs"):
                    continue
                    
                job_detail = status_resp["jobs"][0]
                status = job_detail.get("status")
                
                if status == "FAILED":
                    completed_jobs.add(job["id"])
                    failed_count += 1
                    reason = job_detail.get("statusReason", "Internal container runtime crash")
                    
                    # Append error details cleanly to local file matrix
                    with open(log_file_path, "a") as log_file:
                        log_file.write("--------------------------------------------------\n")
                        log_file.write(f"TIMESTAMP:   {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
                        log_file.write(f"JOB NAME:    {job['name']}\n")
                        log_file.write(f"JOB ID:      {job['id']}\n")
                        log_file.write(f"TEST TYPE:   {job['scenario']['test']}\n")
                        log_file.write(f"DATA SOURCE: {job['scenario']['data_source']}\n")
                        log_file.write(f"N OBS:       {job['scenario']['n']}\n")
                        log_file.write(f"AWS REASON:  {reason}\n")
                        log_file.write("--------------------------------------------------\n\n")
                    
                elif status == "SUCCEEDED":
                    completed_jobs.add(job["id"])
                    
            except Exception:
                pass

    # Final wrap-up print statement
    print(f"\nExecution complete. Total tracked jobs finalized: {len(completed_jobs)} ({len(completed_jobs) - failed_count} Succeeded, {failed_count} Failed).")
    print(f"If any structural container tasks failed, check your local results at: {log_file_path}")