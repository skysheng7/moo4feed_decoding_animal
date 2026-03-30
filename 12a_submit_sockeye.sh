#!/bin/bash
#SBATCH --job-name=pred_models
#SBATCH --account=st-nina-1
#SBATCH --array=1-16
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=24:00:00
#SBATCH --output=logs/12a_model_%a_%A.out
#SBATCH --error=logs/12a_model_%a_%A.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=skysheng@mail.ubc.ca

cd $SLURM_SUBMIT_DIR
mkdir -p logs

module load miniconda3
source activate moo4feed

Rscript scripts/12a_predictability_models_hpc.r "$SLURM_ARRAY_TASK_ID"
