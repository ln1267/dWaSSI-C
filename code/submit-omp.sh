#!/bin/bash -l
#SBATCH --job-name=WaSSI
#SBATCH --account=director100
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --partition=workq
#SBATCH --time=00:30:00
#SBATCH --export=NONE
INPATH=../INPUTS_20KM
OUTPATH=/group/director1234/nliu/rthd_1
if [ ! -d ${OUTPATH} ]; then
	mkdir ${OUTPATH}
fi 
export OMP_NUM_THREADS=1
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out+pat 1 ${INPATH} ${OUTPATH}


OUTPATH=/group/director1234/nliu/rthd_2
if [ ! -d ${OUTPATH} ]; then
	mkdir ${OUTPATH}
fi 
export OMP_NUM_THREADS=2
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out+pat 1 ${INPATH} ${OUTPATH}

OUTPATH=/group/director1234/nliu/rthd_4
if [ ! -d ${OUTPATH} ]; then
	mkdir ${OUTPATH}
fi 
export OMP_NUM_THREADS=4
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out+pat 1 ${INPATH} ${OUTPATH}

OUTPATH=/group/director1234/nliu/rthd_8
if [ ! -d ${OUTPATH} ]; then
	mkdir ${OUTPATH}
fi 
export OMP_NUM_THREADS=8
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out+pat 1 ${INPATH} ${OUTPATH}

OUTPATH=/group/director1234/nliu/rthd_16
if [ ! -d ${OUTPATH} ]; then
	mkdir ${OUTPATH}
fi 
export OMP_NUM_THREADS=16
time -p aprun -n 1 -N 1 -d ${OMP_NUM_THREADS} ./a.out+pat 1 ${INPATH} ${OUTPATH}


