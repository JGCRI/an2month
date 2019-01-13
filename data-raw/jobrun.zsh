#!/bin/zsh

#SBATCH -n 96
#SBATCH -t 24:0:0
#SBATCH -A GCAM

date

tmpdir=`mktemp -d`
nodefile=$tmpdir/nodes.txt

scontrol show hostnames > $nodefile

tid=$SLURM_ARRAY_TASK_ID

program="./L3.fit_dirichlet_params.R"

cmd="source('$program'); procmodel($tid, nodefile='$nodefile', nproc=$SLURM_NTASKS)"
echo "Run command:"
echo $cmd

Rscript -e $cmd

rm -rf $tmpdir

date
