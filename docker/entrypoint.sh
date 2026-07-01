#!/bin/bash
# docker/entrypoint.sh

# 1. SETUP
# Implentation and storage variables
INDEX=${AWS_BATCH_JOB_ARRAY_INDEX:-0}
S3_BUCKET=${S3_BUCKET:-"stats-research"}
RUN_FOLDER=${RUN_FOLDER:-"default_run"}

# Test parameters that are always required
TEST=${TEST:-"twosample"}
N_OBS=${N_OBS:-10}
N_PERMS=${N_PERMS:-500}
DATA_SOURCE=${DATA_SOURCE:-"real"}  #"real", "normal", "normal_shift", "t_dist", "t_dist_shift","multi_uniform", "multi_perturb", "multi_dense"

# Test parameters needed if using real world data
DATA1=${DATA1:-"home_data.rds"}
DATA2=${DATA2:-"home_data.rds"}

# Parameters that may be needed, depending on theoretical distribution used
D_CATS=${D_CATS:-300}          # Dimensions / Categories for multinomial tests
MU=${MU:-0.0}                  # Mean for Normal distributions
SHIFT=${SHIFT:-0.0}            # Translation shift step for alternatives (t-test/normal)
EPSILON=${EPSILON:-0.0}        # Local perturbation magnitude for multinomial alternative

# 2. RUN SIMULATION
# print message that worker is initializing
echo "========================================="
echo "Initializing AWS Batch Worker"
echo "Job Index:   $INDEX"
echo "Test Type:   $TEST"
echo "Data Source: $DATA_SOURCE"
echo "--Parameters--"
echo "N Observations: $N_OBS"
echo "N Permutations: $N_PERMS"
echo "Theoretical Params: D_CATS: $D_CATS, MU: $MU, SHIFT: $SHIFT, EPSILON: $EPSILON"
echo "Real Data Params: DATA1: $DATA1, DATA2: $DATA2"
echo "========================================="

# change directory so we know where files are being read/written
cd /app/src

# call R script located in the src file
# which runs one single simulation
Rscript /app/src/sim_worker.R \
  --index "$INDEX" \
  --test "$TEST" \
  --n_obs "$N_OBS" \
  --n_perms "$N_PERMS" \
  --data_source "$DATA_SOURCE" \
  --d_cats "$D_CATS" \
  --mu "$MU" \
  --shift "$SHIFT" \
  --epsilon "$EPSILON" \
  --data1 "$DATA1" \
  --data2 "$DATA2"

# 3. SAVE IN S3
# Safely grab the first matching CSV file in the current directory
FILENAME=$(ls sim_out_*.csv 2>/dev/null | head -n 1)

# upload to S3
if [ -n "$FILENAME" ] && [ -f "$FILENAME" ]; then
   
   if [ "$DATA_SOURCE" = "real" ]; then
        # Strip the '.rds' extension from the file names for cleaner folder names
        # e.g., "price_no_basement.rds" becomes "price_no_basement"
        D1_CLEAN=$(basename "$DATA1" .rds)
        D2_CLEAN=$(basename "$DATA2" .rds)
        # Combine them into a single folder descriptor
        # e.g., "price_no_basement_vs_price_with_basement"
        DATA_FOLDER="${D1_CLEAN}_vs_${D2_CLEAN}"
    else
        DATA_FOLDER="$DATA_SOURCE"
    fi

    # Organize S3 destination path 
    S3_DEST="s3://${S3_BUCKET}/perm_sim_results/${RUN_FOLDER}/${TEST}/${DATA_FOLDER}/n${N_OBS}/${FILENAME}"

    echo "Uploading $FILENAME to S3..."
    # 'aws s3 mv' moves the file (uploads then deletes local copy)
    aws s3 mv "$FILENAME" "$S3_DEST"

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
