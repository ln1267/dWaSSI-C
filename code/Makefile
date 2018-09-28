FC= gfortran
OBJ= GLOBALVARS.o GENERAL.o PET.o VALIDATION.o CARBONBAL.o OUTPUT.o SUMMARY.o WARMUP.o WATERBAL.o WATERBAL_LC.o 
DEBUG = 
PRGENV= ${PE_ENV}

ifeq ($(PRGENV),INTEL)
	OMPFLAG=-openmp
	DEBUG=-g
else ifeq ($(PRGENV),GNU)
	OMPFLAG=-fopenmp
	DEBUG=-g
else ifeq ($(PRGENV),CRAY)
	OMPFLAG=-homp
	DEBUG=-G2
else
	OMPFLAG=-fopenmp
	DEBUG=-g
#	CRAY_REVEAL="-h profile_generate"
	
endif 
FCFLAGS = $(DEBUG) $(CRAY_REVEAL)
FFLAGS = $(DEBUG)
all: $(OBJ)
	$(FC) $(FFLAGS)  $+ $(OMPFLAG) 
%.o: %.F90 
	$(FC) $(FCFLAGS) -c $(OMPFLAGS) $<

clean:
	rm *.o *.mod a.out fort.*
