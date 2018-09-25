! This is the latest version of Gridded daily WASSIC for parellel simulating

!******************Defining Common Variables for the whole program********

      Module common_var
      implicit none

! Grid numbers
      INTEGER MAX_GRIDS, MAX_YEARS
      PARAMETER (MAX_GRIDS=10108,MAX_YEARS=32)

! BASIC
      INTEGER NGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND
      COMMON/BASIC/NGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND

! VAL This is used for model calibration       
      REAL VAL_1, VAL_2, VAL_3,VAL_4,VAL_5,VAL_6 
      COMMON/VAL/VAL_1(100), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6

! VALID This is used for model valiadation
      REAL GEP_V,ET_V,GPP_V,NPP_V,FLOW_V,FLOW,RUNOFF_V,RUN_OFF,BASEFLOW_V
        
      COMMON/VALID/ GEP_V(MAX_GRIDS,MAX_YEARS,12), ET_V(MAX_GRIDS,MAX_YEARS,12)&
          ,GPP_V(MAX_GRIDS,MAX_YEARS),NPP_V(MAX_GRIDS,MAX_YEARS), FLOW_V(MAX_YEARS,12),FLOW(MAX_YEARS,12)&
          ,RUNOFF_V(MAX_GRIDS,MAX_YEARS),RUN_OFF(MAX_GRIDS,MAX_YEARS), &
          BASEFLOW_V(MAX_YEARS,12)

! CELLINFO
      REAL LADUSE,LATUDE,LONGI,HUCELE
      INTEGER HUCNO
      COMMON/CELLINFO/LADUSE(MAX_GRIDS),HUCNO(MAX_GRIDS),LATUDE(MAX_GRIDS),LONGI(MAX_GRIDS),HUCELE(MAX_GRIDS)

! VEGINFO
      INTEGER veg
      COMMON/VEGINFO/VEG(MAX_GRIDS,MAX_YEARS)


! OUTPUT1
      REAL PET,PAET,AET,RUNOFF,BASEFLOW,PRIBF,SECBF,INTF,TRUNOFF, &
         RUN_HRU,BASE_HRU
      COMMON/OUTPUT1/ PET(MAX_GRIDS,MAX_YEARS,12),PAET(MAX_GRIDS,MAX_YEARS,12),AET(MAX_GRIDS,MAX_YEARS, 12),&
          RUNOFF(MAX_GRIDS,MAX_YEARS,12) ,BASEFLOW(MAX_GRIDS,MAX_YEARS,12), PRIBF(MAX_GRIDS,MAX_YEARS,12) , &
		  SECBF(MAX_GRIDS,MAX_YEARS,12) , TRUNOFF(MAX_GRIDS,MAX_YEARS,12), &
         INTF(MAX_GRIDS,MAX_YEARS,12), RUN_HRU(MAX_GRIDS,MAX_YEARS,12),BASE_HRU(MAX_GRIDS,MAX_YEARS,12)  

! Monthly RUNOFF

       Real RUNOFF_MON, BASEFLOW_MON
       COMMON/Month_RUN/ RUNOFF_MON(MAX_GRIDS,12),BASEFLOW_MON(MAX_GRIDS,12)
      
! CLIMATE      
      REAL  RAIN,TEMP,AAPPT
      COMMON/CLIMATE/ RAIN(MAX_GRIDS,MAX_YEARS,12), TEMP(MAX_GRIDS,MAX_YEARS,12), AAPPT(MAX_GRIDS)
      
! LAI        
      REAL LAI
      COMMON/LAI/LAI(MAX_GRIDS,MAX_YEARS,12)

! SNOWPACK	
      REAL SP
	  INTEGER NSPM
      COMMON/SNOWPACK/SP(MAX_GRIDS,MAX_YEARS,12), NSPM(MAX_GRIDS,MAX_YEARS)

! SUMMARY1 
      REAL ANURAIN,ANURUN,ANUPET,ANUAET,ANUPAET,RAINALL, AETALL, PETALL, RUNALL, RUNRATIO,ETRATIO_GRD,TRATIO,RALL
	  INTEGER NUM_Year
      COMMON/SUMMARY/ANURAIN(MAX_GRIDS,MAX_YEARS),ANURUN(MAX_GRIDS,MAX_YEARS),ANUPET(MAX_GRIDS,MAX_YEARS),&
	  ANUAET(MAX_GRIDS,MAX_YEARS),ANUPAET(MAX_GRIDS,MAX_YEARS),&
	  RAINALL(MAX_GRIDS), AETALL(MAX_GRIDS), PETALL(MAX_GRIDS), RUNALL(MAX_GRIDS), RUNRATIO(MAX_GRIDS),&
	  ETRATIO_GRD(MAX_GRIDS),TRATIO(MAX_GRIDS),RALL(MAX_GRIDS),NUM_Year(MAX_GRIDS)

! SOIL parameters input
      REAL LZTWM, LZFPM, LZFSM,LZSK,LZPK, UZTWM, UZFWM, UZK, ZPERC,&
        REXP, PFREE        
      COMMON/SOIL/LZTWM(MAX_GRIDS), LZFPM(MAX_GRIDS), LZFSM(MAX_GRIDS), LZSK(MAX_GRIDS),LZPK(MAX_GRIDS), UZTWM(MAX_GRIDS), &
        &UZFWM(MAX_GRIDS),UZK(MAX_GRIDS), ZPERC(MAX_GRIDS),REXP(MAX_GRIDS), PFREE(MAX_GRIDS)

! Soil Mositure (AV-average,EM-end of month)
       REAL AVSMC,AVUZTWC,AVUZFWC,AVLZTWC,AVLZFPC,AVLZFSC,EMSMC,EMUZTWC,EMUZFWC,EMLZTWC,EMLZFPC,EMLZFSC
	   COMMON/SMC/AVSMC(MAX_GRIDS,MAX_YEARS,12),AVUZTWC(MAX_GRIDS,MAX_YEARS,12), AVUZFWC(MAX_GRIDS,MAX_YEARS,12), &
	   AVLZTWC(MAX_GRIDS,MAX_YEARS,12), AVLZFPC(MAX_GRIDS,MAX_YEARS,12),AVLZFSC(MAX_GRIDS,MAX_YEARS,12), &
	   EMSMC(MAX_GRIDS,MAX_YEARS,12),EMUZTWC(MAX_GRIDS,MAX_YEARS,12),EMUZFWC(MAX_GRIDS,MAX_YEARS,12),&
	   EMLZTWC(MAX_GRIDS,MAX_YEARS,12), EMLZFPC(MAX_GRIDS,MAX_YEARS,12),EMLZFSC(MAX_GRIDS,MAX_YEARS,12)
	   
! FLOW 
      REAL STRFLOW,STRET,STRGEP     
      COMMON/FLOW/STRFLOW(MAX_GRIDS,MAX_YEARS,12),STRET(MAX_GRIDS,MAX_YEARS,12)&
     ,STRGEP(MAX_GRIDS,MAX_YEARS,12)

! CARBON
        REAL GEPM,RECOM,NEEM,GEPA,NEEA,AHUCGEP, AHUCNEE,AHUCRE
		INTEGER NUM_YEAR_C
      COMMON/CARBON/ GEPM(MAX_GRIDS,MAX_YEARS,12),RECOM(MAX_GRIDS,MAX_YEARS,12), &
      NEEM(MAX_GRIDS,MAX_YEARS,12),GEPA(MAX_GRIDS,MAX_YEARS),NEEA(MAX_GRIDS,MAX_YEARS)&
	  ,AHUCGEP(MAX_GRIDS), AHUCNEE(MAX_GRIDS),AHUCRE(MAX_GRIDS),NUM_YEAR_C(MAX_GRIDS)
      
! BYLAND   
!      REAL,POINTER:: RUNLAND(:,:,:,:),ETLAND(:,:,:,:),GEPLAND(:,:,:,:)
!	   COMMON/BYLAND/RUNLAND,ETLAND,GEPLAND
!      REAL RUNLAND,ETLAND,GEPLAND    
!      COMMON/BYLAND/ RUNLAND(MAX_GRIDS,MAX_YEARS,12,31), &
!        ETLAND(MAX_GRIDS,MAX_YEARS,12,31), GEPLAND(MAX_GRIDS,MAX_YEARS,12,31)

! HUCPETAET
      REAL HUCAET, HUCPET,HUCPAET
      COMMON/HUCPETAET/HUCAET(MAX_GRIDS,MAX_YEARS), HUCPET(MAX_GRIDS,MAX_YEARS),&
      HUCPAET(MAX_GRIDS,MAX_YEARS)

! LANDCHANGE     
      REAL FPERD, FPERDLAI  
      COMMON/LANDCHANGE/FPERD,FPERDLAI
        
! R	  
	  REAL RFACTOR,ETRATIO,ARUNRT,AETRT 
      COMMON/R/ RFACTOR(MAX_GRIDS,MAX_YEARS),ETRATIO(MAX_GRIDS,MAX_YEARS),&
	  ARUNRT(MAX_GRIDS,MAX_YEARS), AETRT(MAX_GRIDS,MAX_YEARS)
	  
	 	  
      end


!**************---Program Starting--------*************************
     
      
      PROGRAM WaSSICBZB 
         
       use common_var

      implicit none 
    
           
REAL,POINTER:: RUNLAND(:,:,:,:),ETLAND(:,:,:,:),GEPLAND(:,:,:,:)
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
      CHARACTER(len=256),ALLOCATABLE:: ARGS(:) 
      CHARACTER(256) ARCH,INPATH,OUTPATH
      INTEGER (kind=4) iargc,INDX
! --- Write introductory information to screen
      
      WRITE(*,10)
   10 FORMAT(' *************************************************'//,&
            '                   *** Revised Gridded daily WaSSI-CB by Ning Liu  ***'//,&
       '   Water Supply Stress Index Modeling System'//,&
       ' Eastern Forest Environmental Threat Assessment Center'/,&
         ' USDA Forest Service Southern Research Station '/,&
            ' Raleigh, NC'//,&
            ' April 2015 -'//,&
            ' ' //)
      
!      PRESS = "Y"
!	IF (press.eq."y" .or.press.eq."Y") THEN
   20 FORMAT(A1)
    

!Print*, "Please chose LINUX or PC version model."
!Print*, "Input 1 for LINUX and 2 for PC"
  
	ALLOCATE (ARGS(iargc()))
	DO INDX=1,iargc()
		CALL getarg(INDX, ARGS(INDX))
	END DO
	ARCH=ARGS(1)
	INPATH=ARGS(2)
	OUTPATH=ARGS(3)

!10001  READ(*,*) PRESS
!PRESS=1
IF (ARCH == '1') THEN
	PRESS=1
ELSE IF (ARCH == '2') THEN
	PRESS=2
END IF
WRITE(*,*) 'ARCH set to ',TRIM(ARCH)
WRITE(*,*) 'INPUT files will be read from directory ',TRIM(INPATH)
WRITE(*,*) 'OUTPUT files will be written in directory ',TRIM(OUTPATH)
	 IF (PRESS == 1)  then 
	 
	!!!!-----------Open files------------------   
	!!! This is for Linux  
	!--Open Input files----------------------------------------------
	 
		  OPEN(1,FILE=TRIM(INPATH)//'/GENERAL.TXT')
		  OPEN(2,FILE=TRIM(INPATH)//'/CELLINFO.TXT') 
	!      OPEN(3,FILE=TRIM(INPATH)//'/vegINFO.TXT')
		  OPEN(4,FILE=TRIM(INPATH)//'/CLIMATE.TXT')

		  OPEN(7,FILE=TRIM(INPATH)//'/SOILINFO.TXT')
		  OPEN(8,FILE=TRIM(INPATH)//'/LANDLAI.TXT')

	!      OPEN(11,FILE=TRIM(INPATH)//'/HUCAREA.TXT')
	!      OPEN(22,FILE=TRIM(INPATH)//'/V_FLOW.TXT')

	! ---Open Output files---------------------------------------- 

		  OPEN(77,FILE=TRIM(OUTPATH)//'/BASICOUT.TXT')
		  OPEN(78,FILE=TRIM(OUTPATH)//'/MONTHFLOW.TXT')
		  OPEN(79,FILE=TRIM(OUTPATH)//'/ANNUALFLOW.TXT')
		  OPEN(80,FILE=TRIM(OUTPATH)//'/HUCFLOW.TXT')
	!	  OPEN(99,FILE=TRIM(OUTPATH)//'/ceshi.TXT')
		  OPEN(400,FILE=TRIM(OUTPATH)//'/MONTHCARBON.TXT')
		  OPEN(500,FILE=TRIM(OUTPATH)//'/ANNUALCARBON.TXT')
		  OPEN(600,FILE=TRIM(OUTPATH)//'/HUCCARBON.TXT')
	!      OPEN(700,FILE=TRIM(OUTPATH)//'/ANNUALBIO.TXT')
	!      OPEN(800,FILE=TRIM(OUTPATH)//'/HUCBIO.TXT')    
		  OPEN(900,FILE=TRIM(OUTPATH)//'/SOILSTORAGE.TXT')
	!      OPEN(910,FILE=TRIM(OUTPATH)//'/RUNOFFBYLANDUSE.TXT')
	!      OPEN(920,FILE=TRIM(OUTPATH)//'/FLOWVOLBYLANDUSE.TXT')     
	!      OPEN(1000,FILE=TRIM(OUTPATH)//'/RUNLAND.TXT')
	! --- Open Output FILES (WARMUP.FOR)
			OPEN(2002,FILE=TRIM(OUTPATH)//'/DATA_V_F.TXT') 
		   OPEN(2003,FILE=TRIM(OUTPATH)//'/VALIDATION.TXT') 
	 ELSEIF (PRESS == 2) then
	 
	 !!! This is for Windows
	!!--Open Input files------------------ 
		  OPEN(1,FILE='E:\Github\WaSSI\Inputs_01_12\GENERAL.TXT')
		  OPEN(2,FILE='E:\Github\WaSSI\Inputs_01_12\CELLINFO.TXT') 
	!!      OPEN(3,FILE='E:\Github\WaSSI\Inputs_01_12\vegINFO.TXT')
		  OPEN(4,FILE='E:\Github\WaSSI\Inputs_01_12\CLIMATE.TXT')
	!
		  OPEN(7,FILE='E:\Github\WaSSI\Inputs_01_12\SOILINFO.TXT')
		  OPEN(8,FILE='E:\Github\WaSSI\Inputs_01_12\LANDLAI.TXT')
	! ! ---Open Output files---------------------------------------- 
	!
		  OPEN(77,FILE='E:\Github\output\BASICOUT.TXT')
		  OPEN(78,FILE='E:\Github\output\MONTHFLOW.TXT')
		  OPEN(79,FILE='E:\Github\output\ANNUALFLOW.TXT')
		  OPEN(80,FILE='E:\Github\output\HUCFLOW.TXT')
		  OPEN(99,FILE='E:\Github\output\ceshi.TXT')
		  OPEN(400,FILE='E:\Github\output\MONTHCARBON.TXT')
		  OPEN(500,FILE='E:\Github\output\ANNUALCARBON.TXT')
		  OPEN(600,FILE='E:\Github\output\HUCCARBON.TXT')
	!!      OPEN(700,FILE='E:\Github\output\ANNUALBIO.TXT')
	!!      OPEN(800,FILE='E:\Github\output\HUCBIO.TXT')    
		  OPEN(900,FILE='E:\Github\output\SOILSTORAGE.TXT')
	!!      OPEN(910,FILE='E:\Github\output\RUNOFFBYLANDUSE.TXT')
	!!      OPEN(920,FILE='E:\Github\output\FLOWVOLBYLANDUSE.TXT')     
	!!      OPEN(1000,FILE='E:\Github\output\RUNLAND.TXT')
	!! --- Open Output FILES (WARMUP.FOR)
		   OPEN(2002,FILE='E:\Github\output\DATA_V_F.TXT')
		   OPEN(2003,FILE='E:\Github\output\WATERBALANCE.TXT')
	 
	 ELSE
	   
!	   Print*,"Please input 1 for LINUX or 2 for Windows"
!	   goto 10001
	 ENDIF  
 
   WRITE(*,30)
   30 FORMAT('       *** PROGRAM IS RUNNING, PLEASE WAIT ***')
 
!  --------- Read input data -------------------------------
       
      CALL RPSDF       ! Set up column headings for each output files
      
      CALL RPSINT      ! Read Landuse, elevation and Soil parameters
          
!      CALL RPSWATERUSE  ! Read HUC area, elevation, and slope
      
      print*,"finish read Land cover  data"
	  
      CALL RPSLAI     ! Read LAI data
      
      print*,"finish read LAI  data"
	  
      CALL RPSCLIMATE  ! Read calimate data

    !  CALL  RPSVALID   ! Read Runoff validation data

      print*,"finish read Climate data"
	  
!----------------------Calibration part------------------------------------      

! --- START SIMULATION LOOPS
!      VAL_L1=30
!      VAL_L2=1
!      TUN=2
!       PRINT*,"请输入“变量起始值”, “变量增量”, “模拟次数”的值"
!       PRINT*,"以逗号或者回车隔开"
!      READ (*,*) VAL_L1,VAL_L2,TUN
!      PRINT*,VAL_L1,VAL_L2,TUN


 !     DO 1111 TUN1=1, 1
 !       VAL_L1=VAL_L1+VAL_L2
  !      VAL_1(TUN1)=VAL_L1
!      DO 1112 TUN2=1, 1
!        VAL_L2=VAL_L2+0.05
!        VAL_2(TUN2)=VAL_L2
!        TUN=TUN+1 

      WRITE(77,2051)
2051  FORMAT(/'SOIL PARAMETERS FOR EACH SIMULATION CELL'/)
!      READ (7,550) DUMY
!550   FORMAT (30A8)
!  
!      WRITE (77,550) DUMY
!          
!----------------------Modelling for each Cell and year start------------------------------------  

      ALLOCATE (RUNLAND(NGRID,NYEAR,12,31))
      ALLOCATE (ETLAND(NGRID,NYEAR,12,31))
      ALLOCATE (GEPLAND(NGRID,NYEAR,12,31))

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
             
110         CONTINUE 

            DO 400 IM=1, 12
               IF (NDAY .EQ. 365) THEN
                 MNDAY=MONTHD(IM)
               ELSE
                 MNDAY=MONTHL(IM)
               ENDIF
               
                                             
               CALL WARMPET(ICELL, IYEAR, IM, MNDAY)  ! Caculate MONTHLY PET AND POTENTIAL AET 
               CALL WATERBAL(ICELL, IYEAR, IM, MNDAY,RUNLAND,ETLAND,GEPLAND) ! Caculate MONTHLY GPP and ET
               

400         CONTINUE ! END LOOP MONTH

!    WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT            
!     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT             
!     CALCULATE TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP  
!     PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT  

!     CALCULATE R FACTOR AND OUTPUT TO ANNUALFLOW.TXT           

		CALL SUMMARY_MONTH(ICELL,IYEAR)
300      CONTINUE  ! END LOOP YEAR

!     CALCULATE AVERAGE WATER BALANCE COMPONENTS FROM IYSTART TO IYEND
!     WRITE TO SUMMARRUNOFF.TXT   
 
		                               
		CALL SUMMARY_YEAR(ICELL) 
		CALL SUMMARY_CABON(ICELL)

200   CONTINUE  ! END LOOP GRID
!$OMP END PARALLEL DO


! This is for output result

		CALL OUTPUT !(ICELL,IYEAR)  ! Output Annual water and carbon balances

            PRINT *, 'WATER BALANCE SECTION SUCCEEDED!'                      

!     SIMULATE TOTAL FLOW FOR EACH MONTH AND EACH HUC                  
!     WRITE ACCUMULATED FLOW TO MONTHACCFLOW.TXT                       
!     PERFORM WATER SUPPLY/DEMAND AND WASSI CALCULATIONS               
!     WRITE WASSI OUTPUT TO ANNUALWaSSI.TXT                            
!     WRITE WASSI OUTPUT TO HUCWaSSI.TXT                               

!            CALL FLOWROUTING
                    
!            PRINT *, 'FLOW ROUTING DONE'
            
!     Simulate GEP AND NEE for selected HUC                            
!     WRITE GEP AND NEE TO MONTHCARBON.TXT, ANNUALCARBON.TXT, HUCCARBON.TXT        
!     SIMULATE BIODIVERSITY FOR SELECTED HUC                           
!     WRITE BIODIVERSITY TO HUCBIO.TXT                                 


!            CALL CARBONBAL
            
            PRINT *, 'CARBON BALANCE AND BIODIVERSITY SIMULATION ENDS'
                       
            CALL VALIDATION    
           
         WRITE(*,75)
75         FORMAT('  CALCULATING FLOW BY LANDCOVER'/)


DEALLOCATE (RUNLAND,ETLAND,GEPLAND)
!1112   CONTINUE
!1111   CONTINUE
 !           CALL FLOWBYLAND      

  
            PRINT *, '-------------PROGRAM RUN ENDS----------------!'
      Stop
!      ELSE
!    	    	STOP 
!	ENDIF		
      END

