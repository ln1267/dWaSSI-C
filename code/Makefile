FC= ftn
OBJ= GENERAL.o PET.o VALIDATION.o CARBONBAL.o OUTPUT.o SUMMARY.o WARMUP.o WATERBAL.o 
DEBUG = -g
OMPFLAG= -fopenmp
FCFLAGS = $(DEBUG)
FFLAGS = $(DEBUG)

all: $(OBJ)
	$(FC) $(FFLAGS)  $+ $(OMPFLAG) 
%.o: %.f90 
	$(FC) $(FCFLAGS) -c $<

clean:
	rm *.o *.mod a.out
