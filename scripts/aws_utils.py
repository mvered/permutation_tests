#!/usr/bin/env python3

# set up
import boto3
batch = boto3.client('batch', region_name='us-east-2')

# function to call an aws batch job
def submit_sim_array(test_name, n_size, bucket, d1, d2, job_count=1000):
    """Submits a single array of 1,000 simulations with specific data files"""
    
    JOB_QUEUE = "PermutationSimQueue"
    JOB_DEF   = "permutation-sim-job-def" 
    
    # We include data names in the job name for easier tracking in the console
    job_name = f"Sim_{test_name}_N{n_size}_{d1.split('.')[0]}"

    response = batch.submit_job(
        jobName=job_name,
        jobQueue=JOB_QUEUE,
        jobDefinition=JOB_DEF,
        arrayProperties={
            'size': job_count
        },
        containerOverrides={
            'environment': [
                {'name': 'TEST', 'value': test_name},
                {'name': 'N_OBS', 'value': str(n_size)},
                {'name': 'S3_BUCKET', 'value': bucket},
                {'name': 'DATA1', 'value': d1}, # Injected into entrypoint.sh
                {'name': 'DATA2', 'value': d2}  # Injected into entrypoint.sh
            ]
        }
    )
    
    print(f"Submitted {job_count} jobs: {job_name} (Using {d1} and {d2})")
