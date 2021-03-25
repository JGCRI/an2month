#!/bin/zsh
#SBATCH -n 4 # This is set for testing will want to scale up (originally set at 120)
#SBATCH -t 144:0:0
#SBATCH -A IHESD


# About this script
# This script uses functions defined in the L3 script to generate the alpha distrubtuions from the 
# monthly fraction netcdf files. To launch this script use a variation of the following command 
# sbatch --array=1-2 jobrun.zsh
# whre the array is used to indicate the number of model/experiments to process. These are 
# defined in the L3.fit_dirichlet_params.R script. And because indexing in R starts at 1
# the slurm array must also start at 1 otherwise it will run nothing.

source /etc/profile.d/modules.sh >& /dev/null
#module load gcc/6.3.0
module load gcc/5.2.0
limit stacksize unlimited
module load R/3.4.3

date

Rscript /pic/projects/GCAM/Dorheim/an2month/data-raw/L3B.run_rstan_alpha_code.R

date
