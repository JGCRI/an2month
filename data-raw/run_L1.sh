#!/bin/bash
#SBATCH -A IHESD
#SBATCH -t 24:00:00
#SBATCH -N 1
#SBATCH -p shared
#SBATCH -n 1

#Set up your environment you wish to run in with module commands.
module load R/3.4.3

#Actually codes starts here
now=$(date)
echo "Current time : $now"

Rscript /pic/projects/GCAM/Dorheim/grand_exp/an2month/data-raw/L1A.monthly_fractions.R --nosave --no-restore

now=$(date)
echo "Current time : $now"