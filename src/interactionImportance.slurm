#!/usr/bin/bash
#SBATCH --job-name=interactionImportance
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=10G
#SBATCH --time=15:45:00
#SBATCH --partition=jclarke,batch,guest
#SBATCH --output=/work/schnablelab/jdavis132/HIPS/interactionImportance.out
#SBATCH --error=/work/schnablelab/jdavis132/HIPS/interactionImportance.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jdavis132@huskers.unl.edu

cd /work/schnablelab/jdavis132/HIPS

ml R/4.3
R CMD BATCH interactionImportance.R
