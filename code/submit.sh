#!/bin/bash -l
#SBATCH --job-name=WaSSI
#SBATCH --account=director1234
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --partition=debugq
#SBATCH --time=00:30:00
#SBATCH --export=NONE
INPATH=../Inputs_01_12
OUTPATH=../rthd_1
if [ ! -d ${OUTPATH} ]; then
	mkdir ${OUTPATH}
fi 
export OMP_NUM_THREADS=1
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out 1 ${INPATH} ${OUTPATH}


OUTPATH=../output
if [ ! -d ${OUTPATH} ]; then
	mkdir ${OUTPATH}
fi 
export OMP_NUM_THREADS=4
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out 1 ${INPATH} ${OUTPATH}
