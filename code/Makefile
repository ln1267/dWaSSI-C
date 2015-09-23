FC= ftn
OBJ= GENERAL.o PET.o VALIDATION.o CARBONBAL.o OUTPUT.o SUMMARY.o WARMUP.o WATERBAL.o 
DEBUG = -g
PRGENV= ${PE_ENV}

ifeq ($(PRGENV),INTEL)
	OMPFLAG=-openmp
else ifeq ($(PRGENV),GNU)
	OMPFLAG=-fopenmp
else ifeq ($(PRGENV),CRAY)
	OMPFLAG=-homp
endif 
FCFLAGS = $(DEBUG)
FFLAGS = $(DEBUG)

all: $(OBJ)
	$(FC) $(FFLAGS)  $+ $(OMPFLAG) 
%.o: %.f90 
	$(FC) $(FCFLAGS) -c $<

clean:
	rm *.o *.mod a.out
