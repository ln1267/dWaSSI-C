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
!		INTEGER HUCNO
		INTEGER,POINTER:: HUCNO(:)
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
		NEEM(:,:,:),GEPA(:,:),NEEA(:,:),&
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
	  
      end


!**************---Program Starting--------*************************
     
      
      PROGRAM WaSSICBZB 
         
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
      CHARACTER(len=52) ARCH,INPATH,OUTPATH
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
		  OPEN(99,FILE=TRIM(OUTPATH)//'/ceshi.TXT')
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
! ---------Array define

 


!  --------- Read input data -------------------------------
       
      CALL RPSDF       ! Set up column headings for each output files
	  
	CALL ARRAY_ALLO  ! allocate all the global pointer arraies      
      
	  CALL RPSINT      ! Read Landuse, elevation and Soil parameters
          
     
      print*,"finish read Land cover  data"
	  
      CALL RPSLAI     ! Read LAI data
      
      print*,"finish read LAI  data"
	  
      CALL RPSCLIMATE  ! Read calimate data

    !  CALL  RPSVALID   ! Read Runoff validation data

      print*,"finish read Climate data"
	 
	Print *,"Size RUNLAND=",size(RUNLAND,1),size(RUNLAND,2),size(RUNLAND,3) 
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
!----------------------Modelling for each Cell and year start------------------------------------  

!      ALLOCATE (RUNLAND(NGRID,NYEAR,12,31))
!      ALLOCATE (ETLAND(NGRID,NYEAR,12,31))
!      ALLOCATE (GEPLAND(NGRID,NYEAR,12,31))

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
               CALL WATERBAL(ICELL, IYEAR, IM, MNDAY) !,RUNLAND,ETLAND,GEPLAND) ! Caculate MONTHLY GPP and ET
               

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
           
	Print *, "Here is ok!" 
         WRITE(*,75)
75         FORMAT('  CALCULATING FLOW BY LANDCOVER'/)

	CALL ARRAY_DEALLO ! Deallocated all global arries

!DEALLOCATE (RUNLAND,ETLAND,GEPLAND)
!1112   CONTINUE
!1111   CONTINUE
 !           CALL FLOWBYLAND      

  
            PRINT *, '-------------PROGRAM RUN ENDS----------------!'
      Stop
!      ELSE
!    	    	STOP 
!	ENDIF		
      END

