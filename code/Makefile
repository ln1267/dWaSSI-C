FC= ftn
OBJ= GENERAL.o PET.o VALIDATION.o CARBONBAL.o OUTPUT.o SUMMARY.o WARMUP.o WATERBAL.o 
DEBUG = -G2
PRGENV= ${PE_ENV}

ifeq ($(PRGENV),INTEL)
	OMPFLAG=-openmp
else ifeq ($(PRGENV),GNU)
	OMPFLAG=-fopenmp
else ifeq ($(PRGENV),CRAY)
	OMPFLAG=-homp
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
