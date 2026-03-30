#!/bin/bash
#SBATCH --job-name=pred_models
#SBATCH --array=1-16
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=24:00:00
#SBATCH --output=logs/12a_model_%a_%A.out
#SBATCH --error=logs/12a_model_%a_%A.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=YOUR_EMAIL@ubc.ca

# ---- Activate your conda environment ----
# Adjust the path/name below to match your setup on Sockeye
source activate moo4feed

mkdir -p logs

cd "$SLURM_SUBMIT_DIR"

Rscript scripts/12a_predictability_models_hpc.r "$SLURM_ARRAY_TASK_ID"
