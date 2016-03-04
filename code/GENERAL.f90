! This is the latest version of Gridded daily WASSIC for parellel simulating

!******************Defining Common Variables for the whole program********
Module common_var
    implicit none

    ! Grid numbers
    INTEGER MAX_GRIDS, MAX_YEARS
    PARAMETER (MAX_GRIDS=10108,MAX_YEARS=32)

    ! BASIC
    INTEGER NGRID,TGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND
    COMMON/BASIC/NGRID,TGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND
    ! Only used in MPI program
    INTEGER my_grid_start,my_grid_end

    ! LANDCHANGE
    REAL FPERD, FPERDLAI
    COMMON/LANDCHANGE/FPERD,FPERDLAI
	  
	  
    ! VAL This is used for model calibration
    REAL VAL_1, VAL_2, VAL_3,VAL_4,VAL_5,VAL_6
    COMMON/VAL/VAL_1(100), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6

    ! VALID This is used for model valiadation
    !  REAL BASEFLOW_V,FLOW_V,FLOW
    REAL,POINTER:: BASEFLOW_V(:,:), FLOW_V(:,:),FLOW(:,:)
		 
		 
    ! VALID This is used for model valiadation
    !      REAL GEP_V,ET_V,GPP_V,NPP_V,RUNOFF_V,RUN_OFF
        
    REAL,POINTER:: GEP_V(:,:,:), ET_V(:,:,:)&
        ,GPP_V(:,:),NPP_V(:,:)&
        ,RUNOFF_V(:,:),RUN_OFF(:,:)
      

    ! CELLINFO
    !		REAL LADUSE,LATUDE,LONGI,HUCELE
    !		INTEGER(kind=4) HUCNO
    INTEGER(kind=4),POINTER:: HUCNO(:)
    REAL,POINTER:: LADUSE(:),LATUDE(:),LONGI(:),HUCELE(:)

    ! VEGINFO
    !      INTEGER veg
    INTEGER,POINTER:: VEG(:,:)


    ! OUTPUT1
    !      REAL PET,PAET,AET,RUNOFF,BASEFLOW,PRIBF,SECBF,INTF,TRUNOFF, &
    !         RUN_HRU,BASE_HRU
    REAL,POINTER:: PET(:,:,:),PAET(:,:,:),AET(:,:, :),&
        RUNOFF(:,:,:) ,BASEFLOW(:,:,:), PRIBF(:,:,:) , &
        SECBF(:,:,:) , TRUNOFF(:,:,:), &
        INTF(:,:,:), RUN_HRU(:,:,:),BASE_HRU(:,:,:)


    ! Monthly RUNOFF

           ! Real RUNOFF_MON, BASEFLOW_MON
    REAL,POINTER:: RUNOFF_MON(:,:),BASEFLOW_MON(:,:)
      
    ! CLIMATE
          ! REAL  RAIN,TEMP,AAPPT
    REAL,POINTER:: RAIN(:,:,:), TEMP(:,:,:), AAPPT(:)
      
    ! LAI
          ! REAL LAI
    REAL,POINTER:: LAI(:,:,:)

    ! SNOWPACK
          ! REAL SP
	      ! INTEGER NSPM
    INTEGER,POINTER:: NSPM(:,:)
    REAL,POINTER:: SP(:,:,:)

    ! SUMMARY1
          ! REAL ANURAIN,ANURUN,ANUPET,ANUAET,ANUPAET,RAINALL, AETALL, PETALL, RUNALL, RUNRATIO,ETRATIO_GRD,TRATIO,RALL
	      ! INTEGER NUM_Year
    INTEGER,POINTER:: NUM_Year(:)
    REAL,POINTER:: ANURAIN(:,:),ANURUN(:,:),ANUPET(:,:),&
        ANUAET(:,:),ANUPAET(:,:),&
        RAINALL(:), AETALL(:), PETALL(:), RUNALL(:), RUNRATIO(:),&
        ETRATIO_GRD(:),TRATIO(:),RALL(:)

    ! SOIL parameters input
          ! REAL LZTWM, LZFPM, LZFSM,LZSK,LZPK, UZTWM, UZFWM, UZK, ZPERC,&
            ! REXP, PFREE
    REAL,POINTER:: LZTWM(:), LZFPM(:), LZFSM(:), LZSK(:),LZPK(:), UZTWM(:), &
        UZFWM(:),UZK(:), ZPERC(:),REXP(:), PFREE(:)

    ! Soil Mositure (AV-average,EM-end of month)
           ! REAL AVSMC,AVUZTWC,AVUZFWC,AVLZTWC,AVLZFPC,AVLZFSC,EMSMC,EMUZTWC,EMUZFWC,EMLZTWC,EMLZFPC,EMLZFSC
    REAL,POINTER:: AVSMC(:,:,:),AVUZTWC(:,:,:), AVUZFWC(:,:,:), &
        AVLZTWC(:,:,:), AVLZFPC(:,:,:),AVLZFSC(:,:,:), &
        EMSMC(:,:,:),EMUZTWC(:,:,:),EMUZFWC(:,:,:),&
        EMLZTWC(:,:,:), EMLZFPC(:,:,:),EMLZFSC(:,:,:)
	   
    ! FLOW
          ! REAL STRFLOW,STRET,STRGEP
    REAL,POINTER:: STRFLOW(:,:,:),STRET(:,:,:),&
        STRGEP(:,:,:)

    ! CARBON
            ! REAL GEPM,RECOM,NEEM,GEPA,NEEA,AHUCGEP, AHUCNEE,AHUCRE
    ! INTEGER NUM_YEAR_C
    INTEGER,POINTER:: NUM_YEAR_C(:)
    REAL,POINTER:: GEPM(:,:,:),RECOM(:,:,:), &
        NEEM(:,:,:),REOA(:,:),GEPA(:,:),NEEA(:,:),&
        AHUCGEP(:), AHUCNEE(:),AHUCRE(:)
      
    ! HUCPETAET
          ! REAL HUCAET, HUCPET,HUCPAET
    REAL,POINTER:: HUCAET(:,:), HUCPET(:,:),&
        HUCPAET(:,:)
  
    ! R
    !	  REAL RFACTOR,ETRATIO,ARUNRT,AETRT
    REAL,POINTER:: RFACTOR(:,:),ETRATIO(:,:),&
        ARUNRT(:,:), AETRT(:,:)
	  
    REAL,POINTER:: RUNLAND(:,:,:,:),ETLAND(:,:,:,:),GEPLAND(:,:,:,:)
	  

    !!MPI variables
	       ! Globalizing it so that other subroutines can access these variables too.
    INTEGER ierr,rank,nprocs

    ! File I/O section
    ! Declaration of Path variables
    character(len=256) ARCH,INPATH,OUTPATH
    ! MPI File I/O related varibales globalized but assingments done in various different subroutines.
    ! Aliases for filenames or units
    ! input files
    integer general_fh, cellinfo_fh, climate_fh, soilinfo_fh, landlai_fh

    ! output files
    integer basicout_fh,monthflow_fh,annualflow_fh,hucflow_fh,ceshi_fh
    integer monthcarbon_fh,annualcarbon_fh,huccarbon_fh
    integer soilstorage_fh,data_v_f_fh,validation_fh

    !Information on composition of the input files to estimate extents while reading the files in parallel
    integer cellinfo_columns,climate_columns,soilinfo_columns,landlai_columns
    !Information on composition of the input files to estimate extents while writing the files in parallel
    integer monthflow_columns,validation_columns,soilstorage_columns,monthcarbon_columns,annualflow_columns
    integer annualcarbon_columns,hucflow_columns,huccarbon_columns

end


!**************---Program Starting--------*************************
     
      
PROGRAM WaSSICBZB
    use mpi
    use common_var

    implicit none

    !OpenMP variables
    INTEGER TID,NTHREADS,OMP_GET_THREAD_NUM,OMP_GET_NUM_THREADS,CHUNK,REM,ST_INDX,END_INDX

    INTEGER ICELL,ICOUNT,IYEAR,MONTHD(12),MONTHL(12)
    INTEGER YEAR,NDAY,IM,MNDAY,PRESS

    CHARACTER input_mulu,output_mulu
    REAL VAL_L1 ,VAL_L2
      
    ! --- Number of days for each month during regular year
    DATA MONTHD/31,28,31,30,31,30,31,31,30,31,30,31/

    ! --- Number of days for each month during leap year
    DATA MONTHL/31,29,31,30,31,30,31,31,30,31,30,31/
      
    ! --- For reading in the command line arguments
    CHARACTER(len=52),ALLOCATABLE:: ARGS(:)
    INTEGER (kind=4) iargc,INDX

    !     Initialize I/O unit identifiers
    general_fh=11
    cellinfo_fh=12
    climate_fh=13
    soilinfo_fh=14
    landlai_fh=15
    basicout_fh=10

    monthflow_fh=21;annualflow_fh=22;hucflow_fh=23;ceshi_fh=24
    monthcarbon_fh=25;annualcarbon_fh=26;huccarbon_fh=27
    soilstorage_fh=28;data_v_f_fh=29;validation_fh=30

    ! --- Write introductory information to screen
      
    WRITE(*,10)
10  FORMAT(' *************************************************'//,&
        '                   *** Revised Gridded daily WaSSI-CB by Ning Liu  ***'//,&
        '   Water Supply Stress Index Modeling System'//,&
        ' Eastern Forest Environmental Threat Assessment Center'/,&
        ' USDA Forest Service Southern Research Station '/,&
        ' Raleigh, NC'//,&
        ' April 2015 -'//,&
        ' ' //)

20  FORMAT(A1)

    ALLOCATE (ARGS(iargc()))
    DO INDX=1,iargc()
        CALL getarg(INDX, ARGS(INDX))
    END DO
    ARCH=ARGS(1)
    INPATH=ARGS(2)
    OUTPATH=ARGS(3)

    IF (ARCH == '1') THEN
        PRESS=1
    ELSE IF (ARCH == '2') THEN
        PRESS=2
    END IF
	
    WRITE(*,*) 'ARCH set to ',TRIM(ARCH)
    WRITE(*,*) 'INPUT files will be read from directory ',TRIM(INPATH)
    WRITE(*,*) 'OUTPUT files will be written in directory ',TRIM(OUTPATH)

    !Initializing MPI
#ifdef MPI
    call MPI_INIT(ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
#endif
    WRITE(*,30)
30  FORMAT('       *** PROGRAM IS RUNNING, PLEASE WAIT ***')
    ! ---------Array define
    !  --------- Read input data -------------------------------

#ifdef MPI
    call open_io
#else
    IF (PRESS == 1)  then

        !!!!-----------Open files------------------
        !!! This is for Linux
        !--Open Input files----------------------------------------------

        OPEN(1,FILE=TRIM(INPATH)//'/GENERAL.TXT')
        OPEN(2,FILE=TRIM(INPATH)//'/CELLINFO.DAT',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=24,STATUS='OLD')
        OPEN(4,FILE=TRIM(INPATH)//'/CLIMATE.DAT',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=16,STATUS='OLD')
        OPEN(7,FILE=TRIM(INPATH)//'/SOILINFO.DAT',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=52,STATUS='OLD')
        OPEN(8,FILE=TRIM(INPATH)//'/LANDLAI.DAT',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=12,STATUS='OLD')

        ! ---Open Output files----------------------------------------

        OPEN(77,FILE=TRIM(OUTPATH)//'/BASICOUT.TXT')
        OPEN(78,FILE=TRIM(OUTPATH)//'/MONTHFLOW.TXT')
        OPEN(79,FILE=TRIM(OUTPATH)//'/ANNUALFLOW.TXT')
        OPEN(80,FILE=TRIM(OUTPATH)//'/HUCFLOW.TXT')
        OPEN(99,FILE=TRIM(OUTPATH)//'/ceshi.TXT')
        OPEN(400,FILE=TRIM(OUTPATH)//'/MONTHCARBON.TXT')
        OPEN(500,FILE=TRIM(OUTPATH)//'/ANNUALCARBON.TXT')
        OPEN(600,FILE=TRIM(OUTPATH)//'/HUCCARBON.TXT')

        OPEN(900,FILE=TRIM(OUTPATH)//'/SOILSTORAGE.TXT')
        ! --- Open Output FILES (WARMUP.FOR)
        OPEN(2002,FILE=TRIM(OUTPATH)//'/DATA_V_F.TXT')
        OPEN(2003,FILE=TRIM(OUTPATH)//'/VALIDATION.TXT')

    ENDIF
#endif

    CALL RPSDF       ! Set up column headings for each output files

#ifdef MPI
    call decompose_ngrid
#endif

    CALL ARRAY_ALLO  ! allocate all the global pointer arraies

    CALL RPSINT      ! Read Landuse, elevation and Soil parameters

    CALL RPSLAI     ! Read LAI data

    CALL RPSCLIMATE  ! Read calimate data


    !!----------------------Modelling for each Cell and year start------------------------------------
    !

    !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(ICELL,IYEAR,IM,MNDAY)
    DO 200 ICELL=1,NGRID


        ICOUNT=0

        DO 300 IYEAR=1, NYEAR
            YEAR = IYSTART + ICOUNT
            ICOUNT=ICOUNT+1
            NDAY = 365
            IF(YEAR/4*4.NE.YEAR) GO TO 110
            IF(YEAR/400*400.EQ.YEAR) GO TO 110
            NDAY=366

110     CONTINUE

        DO 400 IM=1, 12
            IF (NDAY .EQ. 365) THEN
                MNDAY=MONTHD(IM)
            ELSE
                MNDAY=MONTHL(IM)
            ENDIF


            CALL WARMPET(ICELL, IYEAR, IM, MNDAY)  ! Caculate MONTHLY PET AND POTENTIAL AET
            CALL WATERBAL(ICELL, IYEAR, IM, MNDAY) !,RUNLAND,ETLAND,GEPLAND) ! Caculate MONTHLY GPP and ET


400     CONTINUE ! END LOOP MONTH
        !
        !!    WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT
        !!     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT
        !!     CALCULATE TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP
        !!     PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT
        !
        !!     CALCULATE R FACTOR AND OUTPUT TO ANNUALFLOW.TXT
        !
        CALL SUMMARY_MONTH(ICELL,IYEAR)
300 CONTINUE  ! END LOOP YEAR
    !
    !!     CALCULATE AVERAGE WATER BALANCE COMPONENTS FROM IYSTART TO IYEND
    !!     WRITE TO SUMMARRUNOFF.TXT
    !
    !
    CALL SUMMARY_YEAR(ICELL)
    CALL SUMMARY_CABON(ICELL)
!
200 CONTINUE  ! END LOOP GRID
    !$OMP END PARALLEL DO
    !
    !
    !! This is for output result
    !
    CALL OUTPUT !(ICELL,IYEAR)  ! Output Annual water and carbon balances

    CALL ARRAY_DEALLO ! Deallocated all global arries

#ifdef MPI
    call close_io
#endif
  
    PRINT *, '-------------PROGRAM RUN ENDS----------------!'
#ifdef MPI
    call MPI_FINALIZE(ierr)
#endif
    Stop
!      ELSE
!    	    	STOP 
!	ENDIF		
END

