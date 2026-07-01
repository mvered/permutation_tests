#!/usr/bin/env python3

# set up
import boto3
session = boto3.Session(profile_name='stats-research')
batch = session.client('batch', region_name='us-east-2')

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
    job_name = raw_name.replace('.', '_')

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
