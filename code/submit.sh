#!/bin/bash -l
#SBATCH --job-name=WaSSI
#SBATCH --account=director100
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --partition=debugq
#SBATCH --time=00:30:00
#SBATCH --export=NONE
export OMP_NUM_THREADS=1
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out <<< $'1\n'

