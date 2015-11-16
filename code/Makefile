FC= ftn
OBJ= GENERAL.o PET.o VALIDATION.o CARBONBAL.o OUTPUT.o SUMMARY.o WARMUP.o WATERBAL.o ARRAY_ALLO.o ARRAY_DEALLO.o
DEBUG = 
PRGENV= ${PE_ENV}

ifeq ($(PRGENV),INTEL)
	OMPFLAG=-openmp
	DEBUG=-g -dynamic -mcmodel=medium
else ifeq ($(PRGENV),GNU)
	OMPFLAG=-fopenmp
	DEBUG=-g -dynamic -mcmodel=medium 
else ifeq ($(PRGENV),CRAY)
	OMPFLAG=
	DEBUG=-G2 -dynamic -hpic
#	CRAY_REVEAL="-h profile_generate"
	
endif 
FCFLAGS = $(DEBUG) $(CRAY_REVEAL)
FFLAGS = $(DEBUG)

all: $(OBJ)
	$(FC) $(FFLAGS)  $+ $(OMPFLAG) 
%.o: %.f90 
	$(FC) $(FCFLAGS) -c $(OMPFLAGS) $<

clean:
	rm *.o *.mod a.out fort.*
