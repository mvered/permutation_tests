#!/bin/bash
# docker/entrypoint.sh

# 1. SETUP
# Assign AWS variables to local variables
INDEX=${AWS_BATCH_JOB_ARRAY_INDEX:-0}
N_OBS=${N_OBS:-10}
N_PERMS=${N_PERMS:-500}
TEST=${TEST:-"twosample"}
DATA1=${DATA1:-"home_data.rds"}
DATA2=${DATA2:-"home_data.rds"}
S3_BUCKET=${S3_BUCKET:-"stats-research"}

# 2. RUN SIMULATION
# print message that worker is initializing
echo "Initializing worker for Test: $TEST, N_Obs:$N_OBS, Index: $INDEX"

# call R script located in the src file
# which runs one single simulation
Rscript /app/src/sim_worker.R --index "$INDEX" \
  --test "$TEST" --n_obs "$N_OBS" --n_perms "$N_PERMS" \
  --data1 "$DATA1" --data2 "$DATA2"

# 3. SAVE IN S3
# find file created by sim worker
FILENAME=$(ls sim_out_*.csv)

# upload to S3
if [ -f "$FILENAME" ]; then
    echo "Uploading $FILENAME to S3..."
    
    # 'aws s3 mv' moves the file (uploads then deletes local copy)
    aws s3 mv "$FILENAME" "s3://${S3_BUCKET}/perm_sim_results/${TEST}/n${N_OBS}/${FILENAME}"
    
    # print message if successful copying or not
    if [ $? -eq 0 ]; then
        echo "UPLOAD SUCCESSFUL"
    else
        echo "UPLOAD FAILED" >&2
        exit 1
    fi
else
    # if no filename could be found, print warning
    echo "ERROR: R script did not produce an output file." >&2
    exit 1
fi
