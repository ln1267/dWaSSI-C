FC= ftn
OBJ= GENERAL.o PET.o VALIDATION.o CARBONBAL.o OUTPUT.o SUMMARY.o WARMUP.o WATERBAL.o ARRAY_ALLO.o ARRAY_DEALLO.o MPI_SUBROUTINES.o PARALLEL_FILE_IO.o
DEBUG = 
PRGENV= ${PE_ENV}
PREPROCESS= 
#LDFLAGS=-L/group/pawsey0001/mshaikh/software/cle52up04/forge-libs/$(PE_ENV) -lmap-sampler-pmpi -lmap-sampler -Wl,--eh-frame-hdr
ifeq ($(PRGENV),INTEL)
	OMPFLAG=-openmp
	DEBUG=-g -dynamic -mcmodel=medium -fpp -fPIC
else ifeq ($(PRGENV),GNU)
	OMPFLAG=-fopenmp
	DEBUG=-g -dynamic -mcmodel=medium -cpp -fPIC
else ifeq ($(PRGENV),CRAY)
	OMPFLAG= -homp
	DEBUG=-G2 -dynamic -hpic -fPIC
	PREPROCESS="-F" 
#	CRAY_REVEAL="-h profile_generate"
endif 
ifeq "$(EXE)" ""
	EXE=WaSSI
endif
FCFLAGS = $(DEBUG) $(CRAY_REVEAL) $(FPPFLAGS)
FFLAGS = $(DEBUG) $(LDFLAGS)

all: $(OBJ)
	$(FC) $(FFLAGS) $(PREPROCESS) $+ $(OMPFLAG) -o $(EXE)
%.o: %.f90 
	$(FC) $(FCFLAGS) $(PREPROCESS) -c $(OMPFLAG) $<

clean:
	rm *.o *.mod $(EXE) fort.* file*

