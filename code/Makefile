FC= ftn
OBJ= GENERAL.o PET.o VALIDATION.o CARBONBAL.o OUTPUT.o SUMMARY.o WARMUP.o WATERBAL.o ARRAY_ALLO.o ARRAY_DEALLO.o MPI_SUBROUTINES.o PARALLEL_FILE_IO.o
DEBUG = 
PRGENV= ${PE_ENV}
PREPROCESS= 

ifeq ($(PRGENV),INTEL)
	OMPFLAG=-openmp
	DEBUG=-g -dynamic -mcmodel=medium -fpp
else ifeq ($(PRGENV),GNU)
	OMPFLAG=-fopenmp
	DEBUG=-g -dynamic -mcmodel=medium -cpp
else ifeq ($(PRGENV),CRAY)
	OMPFLAG=
	DEBUG=-G2 -dynamic -hpic
	PREPROCESS="-F" 
#	CRAY_REVEAL="-h profile_generate"
	
endif 
FCFLAGS = $(DEBUG) $(CRAY_REVEAL) $(FPPFLAGS)
FFLAGS = $(DEBUG)

all: $(OBJ)
	$(FC) $(FFLAGS) $(PREPROCESS) $+ $(OMPFLAG) 
%.o: %.f90 
	$(FC) $(FCFLAGS) $(PREPROCESS) -c $(OMPFLAG) $<

clean:
	rm *.o *.mod a.out fort.* file*
