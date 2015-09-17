! This is the latest version of Gridded daily WASSIC for parellel simulating

!******************Defining Common Variables for the whole program********

      Module common_var
      implicit none

! Grid numbers
      INTEGER MAX_GRIDS, MAX_YEARS
      PARAMETER (MAX_GRIDS=5200,MAX_YEARS=20)

! BASIC
      INTEGER NGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND
      COMMON/BASIC/NGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND

! VAL This is used for model calibration       
      REAL VAL_1, VAL_2, VAL_3,VAL_4,VAL_5,VAL_6 
      COMMON/VAL/VAL_1(100), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6

! VALID This is used for model valiadation
      REAL GEP_V,ET_V,GPP_V,NPP_V,FLOW_V,FLOW,RUNOFF_V,RUN_OFF,BASEFLOW_V,&
        BASEFLOW
      COMMON/VALID/ GEP_V(MAX_GRIDS,MAX_YEARS,12), ET_V(MAX_GRIDS,MAX_YEARS,12)&
          ,GPP_V(MAX_GRIDS,MAX_YEARS),NPP_V(MAX_GRIDS,MAX_YEARS), FLOW_V(MAX_YEARS,12),FLOW(MAX_YEARS,12)&
          ,RUNOFF_V(MAX_GRIDS,MAX_YEARS),RUN_OFF(MAX_GRIDS,MAX_YEARS), &
          BASEFLOW_V(MAX_YEARS,12),BASEFLOW(MAX_GRIDS,MAX_YEARS,12)

! CELLINFO
      REAL LADUSE,LATUDE,LONGI,HUCELE
      INTEGER HUCNO
      COMMON/CELLINFO/LADUSE(MAX_GRIDS),HUCNO(MAX_GRIDS),LATUDE(MAX_GRIDS),LONGI(MAX_GRIDS),HUCELE(MAX_GRIDS)

! VEGINFO
      INTEGER veg
      COMMON/VEGINFO/VEG(MAX_GRIDS,MAX_YEARS)


! OUTPUT1
      REAL PET,APET,PAET,APAET,AET,RUNOFF,INTER,PRIBF,SECBF,INTF, &
         AVUZTWC,TPAET,AVUZFWC,AVLZTWC,AVLZFPC,AVLZFSC,A_ET,P_ET,Sun_ET,RUN_HRU,BASE_HRU
      COMMON/OUTPUT1/ PET(MAX_YEARS,12),APET(12),PAET(MAX_YEARS,12),APAET(12),&
         AET(12), RUNOFF(12), INTER(12), PRIBF(12), SECBF(12), INTF(12), &
         AVUZTWC(12), AVUZFWC(12), AVLZTWC(12), AVLZFPC(12),AVLZFSC(12),&
         A_ET(MAX_GRIDS,MAX_YEARS, 12),P_ET(MAX_GRIDS,MAX_YEARS,12),Sun_ET(MAX_GRIDS,MAX_YEARS,12),&
         RUN_HRU(MAX_GRIDS,MAX_YEARS,12),BASE_HRU(MAX_GRIDS,MAX_YEARS,12)  

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
      REAL SP,SNOWPACK,NSPM
      COMMON/SNOWPACK/SP(12),SNOWPACK, NSPM(MAX_YEARS)

! SUMMARY1 
      REAL ANURAIN,ANURUN,ANUPET,ANUAET,ANUPAET
      COMMON/SUMMARY1/ANURAIN(MAX_YEARS),ANURUN(MAX_YEARS),ANUPET(MAX_YEARS),ANUAET(MAX_YEARS),&
        ANUPAET(MAX_YEARS)

! SOIL
      REAL LZTWM, LZFPM, LZFSM,LZSK,LZPK, UZTWM, UZFWM, UZK, ZPERC,&
        REXP, PFREE, SMC        
      COMMON/SOIL/LZTWM(MAX_GRIDS), LZFPM(MAX_GRIDS), LZFSM(MAX_GRIDS), LZSK(MAX_GRIDS),&
        LZPK(MAX_GRIDS), UZTWM(MAX_GRIDS), UZFWM(MAX_GRIDS), UZK(MAX_GRIDS), ZPERC(MAX_GRIDS),&
        REXP(MAX_GRIDS), PFREE(MAX_GRIDS), SMC(12)

! Soil Mositure
       REAL UZTWC, UZFWC, LZTWC, LZFSC, LZFPC
       COMMON/SMC/UZTWC, UZFWC, LZTWC, LZFSC, LZFPC
! FLOW 
      REAL STRFLOW,STRET,STRGEP     
      COMMON/FLOW/STRFLOW(MAX_GRIDS,MAX_YEARS,12),STRET(MAX_GRIDS,MAX_YEARS,12)&
     ,STRGEP(MAX_GRIDS,MAX_YEARS,12)

! CARBON
        REAL GEPM,RECOM,NEEM,GEPA,NEEA
      COMMON/CARBON/ GEPM(MAX_GRIDS,MAX_YEARS,12),RECOM(MAX_GRIDS,MAX_YEARS,12), &
      NEEM(MAX_GRIDS,MAX_YEARS,12),GEPA(MAX_GRIDS,MAX_YEARS),NEEA(MAX_GRIDS,MAX_YEARS)
      
! BYLAND   
      REAL,ALLOCATABLE :: RUNLAND(:,:,:,:),ETLAND(:,:,:,:),GEPLAND(:,:,:,:)
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
	  REAL RFACTOR 
      COMMON/R/ RFACTOR(MAX_YEARS)
      end

!-------------------Parallel Lib moduel-----------
    module omp_lib
    implicit none  
    !Defines standard variable precisions of the OpenMP-implementation
     !Gives the explicit interface for each routine of the run-time library
    interface

    function OMP_get_num_threads()
    integer OMP_get_num_threads
    end function OMP_get_num_threads
   
    function OMP_get_thread_num()
    integer(kind=4) :: OMP_get_thread_num
    end function OMP_get_thread_num
    
    end interface
    end module omp_lib


!**************---Program Starting--------*************************
     
      
      PROGRAM WaSSICBZB 
         
       use common_var
      !use omp_lib
      implicit none 
    

      INTEGER NTHREADS, TID, OMP_GET_NUM_THREADS,OMP_GET_THREAD_NUM
      INTEGER  THREAD_NUM
           
      INTEGER ICELL,ICOUNT,IYEAR,MONTHD(12),MONTHL(12)
      INTEGER YEAR,NDAY,IM,MNDAY

      CHARACTER PRESS,input_mulu,output_mulu
      REAL VAL_L1 ,VAL_L2      
      
! --- Number of days for each month during regular year
      DATA MONTHD/31,28,31,30,31,30,31,31,30,31,30,31/

! --- Number of days for each month during leap year
      DATA MONTHL/31,29,31,30,31,30,31,31,30,31,30,31/
      
! --- Write introductory information to screen
      
      WRITE(*,10)
   10 FORMAT(' *************************************************'//,&
            '                   *** Revised Gridded daily WaSSI-CB by Ning Liu  ***'//,&
       '   Water Supply Stress Index Modeling System'//,&
         ' Eastern Forest Environmental Threat Assessment Center'/,&
         ' USDA Forest Service Southern Research Station '/,&
            ' Raleigh, NC'//,&
            ' April 2015 -'//,&
            ' Press Y OR y to continue : ' //)
      READ(*,20) PRESS
!      PRESS = "Y"
!	IF (press.eq."y" .or.press.eq."Y") THEN
   20 FORMAT(A1)
      WRITE(*,30)
   30 FORMAT('       *** PROGRAM IS RUNNING, PLEASE WAIT ***')

!--Open Input files----------------------------------------------
    
	     input_mulu="../Inputs_01_12"
	     output_mulu='../Inputs_01_12\outputs_82_12'

      OPEN(1,FILE='../Inputs_01_12/GENERAL.TXT')
      OPEN(2,FILE='../Inputs_01_12/CELLINFO.TXT') 
!      OPEN(3,FILE='../Inputs_01_12/vegINFO.TXT')
      OPEN(4,FILE='../Inputs_01_12/CLIMATE.TXT')

      OPEN(7,FILE='../Inputs_01_12/SOILINFO.TXT')
      OPEN(8,FILE='../Inputs_01_12/LANDLAI.TXT')

!      OPEN(11,FILE='../Inputs_01_12/HUCAREA.TXT')
!      OPEN(22,FILE='../Inputs_01_12/V_FLOW.TXT')


! ---Open Output files---------------------------------------- 
 
       
      OPEN(77,FILE='../output/BASICOUT.TXT')
      OPEN(78,FILE='../output/MONTHFLOW.TXT')
      OPEN(79,FILE='../output/ANNUALFLOW.TXT')
      OPEN(80,FILE='../output/HUCFLOW.TXT')
      OPEN(99,FILE='../output/ceshi.TXT')
      OPEN(400,FILE='../output/MONTHCARBON.TXT')
      OPEN(500,FILE='../output/ANNUALCARBON.TXT')
      OPEN(600,FILE='../output/HUCCARBON.TXT')
!      OPEN(700,FILE='../output/ANNUALBIO.TXT')
!      OPEN(800,FILE='../output/HUCBIO.TXT')    
      OPEN(900,FILE='../output/SOILSTORAGE.TXT')
!      OPEN(910,FILE='../output/RUNOFFBYLANDUSE.TXT')
!      OPEN(920,FILE='../output/FLOWVOLBYLANDUSE.TXT')     
!      OPEN(1000,FILE='../output/RUNLAND.TXT')
! --- Open Output FILES (WARMUP.FOR)
       OPEN(2002,FILE='../output/DATA_V_F.TXT') 
!       OPEN(2003,FILE='../output/VALIDATION.TXT')  
    
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

    
      DO 200 ICELL=1, NGRID

        THREAD_NUM=OMP_GET_THREAD_NUM()
        PRINT *, THREAD_NUM, ICELL

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
                
               CALL WATERBAL(ICELL, IYEAR, IM, MNDAY) ! Caculate MONTHLY GPP and ET
               

400         CONTINUE 
 

!    WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT            
!     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT             
!     CALCULATE TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP  
!     PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT  

!     CALCULATE R FACTOR AND OUTPUT TO ANNUALFLOW.TXT           

            CALL OUTPUT(ICELL,IYEAR)  ! Output Annual water and carbon balances
            
300      CONTINUE

!     CALCULATE AVERAGE WATER BALANCE COMPONENTS FROM IYSTART TO IYEND
!     WRITE TO SUMMARRUNOFF.TXT   
                                 
    
       CALL SUMMARY(ICELL)
           

200   CONTINUE
       

	     
                
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


            CALL CARBONBAL
            
            PRINT *, 'CARBON BALANCE AND BIODIVERSITY SIMULATION ENDS'
                       
            CALL VALIDATION    
           
         WRITE(*,75)
75         FORMAT('  CALCULATING FLOW BY LANDCOVER'/)

!1112   CONTINUE
!1111   CONTINUE
 !           CALL FLOWBYLAND      

  
            PRINT *, '-------------PROGRAM RUN ENDS----------------!'
      Stop
!      ELSE
!    	    	STOP 
!	ENDIF		
      END

