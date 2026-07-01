#!/usr/bin/env python3

# set up
import boto3
import pandas as pd
import os
import io
session = boto3.Session(profile_name='stats-research')
batch = session.client('batch', region_name='us-east-2')
s3 = session.client('s3', region_name='us-east-2')

# function to call an aws batch job
def submit_sim_array(test_name, n_size, bucket, d1="home_data.rds", d2="home_data.rds", 
                     job_count=2, n_perms=500, data_source="real", run_folder="default_run",
                     d_cats=300, mu=0.0, shift=0.0, epsilon=0.0):
    """
    Submits a single parallel array job block to AWS Batch with flexible simulation parameters.
    Allows for both empirical (real-world data files) and synthetic data sets from theoretical distributions. 
    """
    
    JOB_QUEUE = "PermutationSimQueue"
    JOB_DEF   = "permutation-sim-job-def:3" 
    
    # Structure job name dynamically based on data framework source mapping
    if data_source == "real":
        d1_clean = d1.split('.')[0]
        d2_clean = d2.split('.')[0]
        job_name = f"Sim_{test_name}_real_N{n_size}_{d1_clean}_vs_{d2_clean}"
    
    # For theoretical distributions, include the landscape parameters in the name
    elif data_source in ["normal","t_dist"]:
        job_name = f"Sim_{test_name}_{data_source}_N{n_size}_Mu{mu}"
    elif data_source in ["normal_shift","t_dist_shift"]:
        job_name = f"Sim_{test_name}_{data_source}_N{n_size}_Mu{mu}_Shift{shift}"
    elif data_source in ["multi_uniform","multi_dense"]:
        job_name = f"Sim_{test_name}_{data_source}_N{n_size}_Cats{d_cats}"
    elif data_source in ["multi_perturb"]:
        job_name = f"Sim_{test_name}_{data_source}_N{n_size}_Cats{d_cats}_Epsilon{epsilon}"
   
    # This turns "Shift0.5" into "Shift0_5" and "Epsilon0.06" into "Epsilon0_06"
    job_name = job_name.replace('.', '_')

    # Compile the environment variable payload array
    environment_payload = [
        {'name': 'TEST', 'value': str(test_name)},
        {'name': 'N_OBS', 'value': str(n_size)},
        {'name': 'N_PERMS', 'value': str(n_perms)},
        {'name': 'DATA_SOURCE', 'value': str(data_source)},
        {'name': 'S3_BUCKET', 'value': str(bucket)},
        {'name': 'RUN_FOLDER', 'value': str(run_folder)},
        {'name': 'DATA1', 'value': str(d1)},
        {'name': 'DATA2', 'value': str(d2)},
        {'name': 'D_CATS', 'value': str(d_cats)},
        {'name': 'MU', 'value': str(mu)},
        {'name': 'SHIFT', 'value': str(shift)},
        {'name': 'EPSILON', 'value': str(epsilon)}
    ]

    # submit AWS batch job
    response = batch.submit_job(
        jobName=job_name,
        jobQueue=JOB_QUEUE,
        jobDefinition=JOB_DEF,
        arrayProperties={
            'size': job_count
        },
        containerOverrides={
            'environment': environment_payload
        }
    )

    # Format console feedback output 
    print(f"Submitted {job_count} parallel array items: {job_name}")
    
    # return AWS batch response
    return response

def retrieve_compile_data(run_folder, BUCKET_NAME="stats-research"):
    """gets individual csvs from aws and combines into one results file"""

    # 1. SCAN FOR FILES
    base_prefix = f"perm_sim_results/{run_folder}/"
    print(f"Scanning S3 Bucket under: {base_prefix}...\n")

    paginator = s3.get_paginator('list_objects_v2')
    pages = paginator.paginate(Bucket=BUCKET_NAME, Prefix=base_prefix)
    
    twosample_chunks = []
    independence_chunks = []

    for page in pages:
        if 'Contents' not in page:
            continue
            
        for obj in page['Contents']:
            key = obj['Key']
            
            # Filter for valid CSV files
            if not key.endswith('.csv') or obj['Size'] == 0:
                continue
                
            # Isolate the relative path after the RUN_FOLDER
            # Example: twosample/normal/n20/chunk_1.csv
            relative_path = key.replace(base_prefix, "")
            parts = relative_path.split('/')
            
            if len(parts) < 2:
                continue 
                
            test_type = parts[0]  # 'twosample' or 'independence'
            filename = parts[-1]  # 'chunk_1.csv'

            try:
                # Stream directly from S3 memory
                response = s3.get_object(Bucket=BUCKET_NAME, Key=key)
                csv_bytes = response['Body'].read()
                
                # Load CSV (Using its own internal columns for data_source, n_obs, etc.)
                df_chunk = pd.read_csv(io.BytesIO(csv_bytes))
                
                # Light tracking tag just to know the exact source chunk file name if needed
                df_chunk['s3_filename'] = filename
                
                # Route data matrix to its appropriate array bucket
                if test_type == "twosample":
                    twosample_chunks.append(df_chunk)
                    print(f"  Processed [TWOSAMPLE] ➔ {filename}")
                elif test_type == "independence":
                    independence_chunks.append(df_chunk)
                    print(f"  Processed [INDEPENDENCE] ➔ {filename}")
                
            except Exception as e:
                print(f"Failed processing object at {key}: {str(e)}")

    # 2. PROCESS AND EXPORT DATA
    output_dir = "results/raw_simulation_output/{run_folder}"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    if twosample_chunks:
        print("\nMerging all Two-Sample data chunks...")
        df_twosample = pd.concat(twosample_chunks, ignore_index=True)
        ts_path = os.path.join(output_dir, f"twosample_{run_folder}.csv")
        df_twosample.to_csv(ts_path, index=False)
        print(f"Saved Combined Two-Sample File ({df_twosample.shape[0]} rows) ➔ {ts_path}")
    else:
        print("\n No Two-Sample simulation datasets detected in this directory run.")
    
    if independence_chunks:
        print("\nMerging all Independence data chunks...")
        df_independence = pd.concat(independence_chunks, ignore_index=True)
        ind_path = os.path.join(output_dir, f"independence_{run_folder}.csv")
        df_independence.to_csv(ind_path, index=False)
        print(f" Saved Combined Independence File ({df_independence.shape[0]} rows) ➔ {ind_path}")
    else:
        print("\n No Independence simulation datasets detected in this directory run.")

    print(f"\n====================================================")
    print(f"SUCCESS: Results Retrieval Finished.")
    print(f"====================================================")