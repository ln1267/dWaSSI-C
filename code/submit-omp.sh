#!/bin/bash -l
#SBATCH --job-name=WaSSI
#SBATCH --account=director1234
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --partition=workq
#SBATCH --time=00:30:00
#SBATCH --export=NONE
INPATH=/group/director1234/nliu/INPUTS
OUTPATH=/group/director1234/nliu/rthd1
if [ ! -d ${OUTPATH} ]; then
	mkdir ${OUTPATH}
fi 
export OMP_NUM_THREADS=1
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out 1 ${INPATH} ${OUTPATH}


OUTPATH=/group/director1234/nliu/rthd2
if [ ! -d ${OUTPATH} ]; then
	mkdir ${OUTPATH}
fi 
export OMP_NUM_THREADS=2
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out 1 ${INPATH} ${OUTPATH}

OUTPATH=/group/director1234/nliu/rthd4
if [ ! -d ${OUTPATH} ]; then
	mkdir ${OUTPATH}
fi 
export OMP_NUM_THREADS=4
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out 1 ${INPATH} ${OUTPATH}

OUTPATH=/group/director1234/nliu/rthd8
if [ ! -d ${OUTPATH} ]; then
	mkdir ${OUTPATH}
fi 
export OMP_NUM_THREADS=8
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out 1 ${INPATH} ${OUTPATH}

OUTPATH=/group/director1234/nliu/rthd16
if [ ! -d ${OUTPATH} ]; then
	mkdir ${OUTPATH}
fi 
export OMP_NUM_THREADS=16
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out 1 ${INPATH} ${OUTPATH}


