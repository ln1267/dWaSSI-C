!**********************************************************************C
!                                                                      C
!     *** SUBROUTINE WATERBAL ***                                      C
!     SIMULATES MONTHLY WATER BALANCE USING 2 LAYER SOIL MOISTURE      C
!     ALGORITHM FROM NOAA NATIONAL WEATHER SERVICE SACRAMENTO SOIL     C
!     MOISTURE ACCOUNTING MODEL (SAC-SMA)                              C
!     Line 591 --- Carbon model                                        C
!     Line 941 --- Area for each cell                                  C
!     IF MODEL in dynamic land cover then LADUSE(I) -----> VEG(I,J) total:38   C
!**********************************************************************C
      
      SUBROUTINE WATERBAL_LC(I,J_S,M,MNDAY,RUNLAND,ETLAND,GEPLAND)
        Use Common_var
        implicit none       
! ----------------------------------------------------------------------------     
         
      INTEGER I,J,M,IAM,DAY,MDAY,MNDAY,K,J_S
      
      REAL AETTEMP, RUNOFFTEMP, PBFTEMP, SBFTEMP,IFTEMP, GEPTEMP,&
            RECOTEMP, NEETEMP
      REAL UZTWC, UZFWC, LZTWC, LZFSC, LZFPC ! soil moisture content parameters    
      
	  !this is for catchment scale
	  REAL UZTWC_lc(MAX_HUCS), UZFWC_lc(MAX_HUCS), LZTWC_lc(MAX_HUCS), &
			LZFSC_lc(MAX_HUCS), LZFPC_lc(MAX_HUCS) ! Landuse soil moisture content parameters
	  
      REAL ETUZTW(MAX_YEARS,12), ETLZTW(MAX_YEARS,12), RESIDET(MAX_YEARS,12), ETUZFW(MAX_YEARS,12)
      
	  !this is for catchment scale
	  REAL ETUZTW_lc(MAX_YEARS,12,MAX_HUCS), ETLZTW_lc(MAX_YEARS,12,MAX_HUCS), &
	       RESIDET_lc(MAX_YEARS,12,MAX_HUCS), ETUZFW_lc(MAX_YEARS,12,MAX_HUCS)

      REAL RATLZT, RATLZ
      !this is for catchment scale
	  REAL RATLZT_lc(MAX_HUCS),RATLZ_lc(MAX_HUCS)
	  
      REAL SNOW,SNOWPACK, SNOWW
      
      REAL LTASM, TAREA
      
      REAL ET(MAX_YEARS,12), GEP(MAX_YEARS,12), &
        RECO(MAX_YEARS,12), NEE(MAX_YEARS,12) 
      
	  !this is for catchment scale
	  REAL ET_lc(MAX_YEARS,12,MAX_HUCS), GEP_lc(MAX_YEARS,12,MAX_HUCS),&
        RECO_lc(MAX_YEARS,12,MAX_HUCS), NEE_lc(MAX_YEARS,12,MAX_HUCS)    
		
      REAL DPAET
      
      REAL UZRAT, TWX, PERCM, PERC, DEFR, LZDEF 
      !this is for catchment scale
	  REAL UZRAT_lc(MAX_HUCS),TWX_lc(MAX_HUCS),PERCM_lc(MAX_HUCS), &
	  PERC_lc(MAX_HUCS), DEFR_lc(MAX_HUCS), LZDEF_lc(MAX_HUCS),  PERCT_lc(MAX_HUCS), PERCF_lc(MAX_HUCS)
	  
      REAL PERCT, PERCF
      
      REAL HPL, RATLP, RATLS, FRACP, PERCP,PERCS
      !This is for catchment scale
	  REAL HPL_lc(MAX_HUCS), RATLP_lc(MAX_HUCS), RATLS_lc(MAX_HUCS), FRACP_lc(MAX_HUCS)&
	  , PERCP_lc(MAX_HUCS),PERCS_lc(MAX_HUCS)    
	  
      REAL PBF, SBF, INF, SURFRO,INFIL
	  
	  !this si for catchment scale
	  REAL PBF_lc(MAX_HUCS), SBF_lc(MAX_HUCS), INF_lc(MAX_HUCS),SURFRO_lc(MAX_HUCS),INFIL_lc(MAX_HUCS)
      
      REAL TAUZTWC, TAUZFWC, TALZTWC, TALZFPC, TALZFSC
     
      REAL TASM,AUZTWC,AUZFWC,ALZTWC,ALZFPC,ALZFSC,ASM          
           
      INTEGER GEPFLAG
      
	REAL :: RUNLAND(NGRID,NYEAR_S+NWARMUP,12,31,NLC)
	REAL :: ETLAND(NGRID,NYEAR_S+NWARMUP,12,31,NLC)
	REAL :: GEPLAND(NGRID,NYEAR_S+NWARMUP,12,31,NLC) 
           
! *****************************************************************************************************
!----Set the simulate ID for the start year
	J=J_S+IYSTART-1-NWARMUP

! *****************************************************************************************************
! --- INITIALIZE VARIABLES FOR START OF SIMULATION

             AETTEMP =0.
             RUNOFFTEMP = 0.
             PBFTEMP = 0.0
             SBFTEMP = 0.0
             IFTEMP = 0.0
                          
             GEPTEMP = 0.
             RECOTEMP = 0.
             NEETEMP =0.
			 	 
        IF (J .EQ. IYSTART-NWARMUP .AND. M .EQ. 1) THEN

        IAM =0     
                
        DO 50 K=1, NLC
                        
           UZTWC_lc(K) = UZTWM(I)
           UZFWC_lc(K) = 0.0
           LZTWC_lc(K) = LZTWM(I)
           LZFSC_lc(K) = 0.75*LZFSM(I)
           LZFPC_lc(K) = 0.75*LZFPM(I)
            
50      CONTINUE
        
        ENDIF 
         
! *****************************************************************************************************
! *****************************************************************************************************
!----- SIMULATE SNOWPACK (SEE VAROSMARTY  ET AL., 1989)
      
        IF (TEMP(I,J, M) .LE.  -1.0) THEN
        
           SNOW = RAIN(I,J, M)
    
           SNOWPACK = SNOWPACK + SNOW
           
           IAM = 0
                                
        ELSE 
        
            IAM = IAM +1 
            
            HUCELE(I) = 1000.
            
           IF (HUCELE(I) .LE. 500.0) THEN
          
              SNOWW = SNOWPACK
              SNOWPACK = 0.
                  
           ELSE 
              
              IF (IAM .EQ. 1) THEN 
              
                 SNOWW = 0.5 * SNOWPACK
              
              ELSEIF (IAM .EQ. 2) THEN
              
                 SNOWW = SNOWPACK
                 
              ELSE
              
                 SNOWW = 0.
                 SNOWPACK = 0.
                                   
              ENDIF
              
              SNOWPACK = SNOWPACK - SNOWW
                            
          ENDIF           
        ENDIF  
                            

! *****************************************************************************************************
! *****************************************************************************************************
! *****************************************************************************************************
! -- LOOP THROUGH DAYS OF CURRENT MONTH AND CALCULATE SOIL WATER STORAGE, BASEFLOW, RUNOFF, AND AET
        
        DO 100 DAY= 1, MNDAY    
         
             TAREA = 0.
             TASM = 0.
             TAUZTWC = 0.
             TAUZFWC = 0.
             TALZTWC = 0.
             TALZFPC = 0.
             TALZFSC = 0.


! *****************************************************************************************************
! *****************************************************************************************************
! -- LOOP THROUGH LAND COVERS IN THE HUC AND PERFORM WATER BALANCE COMPUTATIONS
! -- ASSUMES OPEN WATER LAND COVER IS NEG.
         
             K=0
          
             DO 40 K=1, NLC
                  
! *****************************************************************************************************
! -- SET ET, SURFACE RUNOFF, INTERFLOW, GEP TO ZERO IF TEMPERATURE IS LE -1.0
! -- BASEFLOW STILL OCCURS            

             IF (TEMP (I,J, M) .LE. -1.0) THEN
          
                ET_lc(J,M,K) = 0.
                SURFRO_lc(K) = 0.
                INF_lc(K) = 0.
                GEP_lc(J, M, K) = 0.
                
                
! -- COMPUTE PRIMARY BASEFLOW WHEN T <= -1.0

                PBF_lc(K) = LZFPC_lc(K) * LZPK(I)
                LZFPC_lc(K) = LZFPC_lc(K) - PBF_lc(K)
                IF (LZFPC_lc(K) .LE. 0.0001) THEN 
                   PBF_lc(K) = PBF_lc(K) + LZFPC_lc(K)
                   LZFPC_lc(K) = 0.0
                ENDIF
                
! -- COMPUTE SECONDARY BASEFLOW WHEN T <= -1.0

                SBF_lc(K) = LZFSC_lc(K) * LZSK(I)
                LZFSC_lc(K) = LZFSC_lc(K) - SBF_lc(K)
                IF (LZFSC_lc(K) .LE. 0.0001) THEN
                   SBF_lc(K) = SBF_lc(K) + LZFSC_lc(K)
                   LZFSC_lc(K) = 0.0
                ENDIF                

             ELSE
! *****************************************************************************************************
! *****************************************************************************************************
! -- COMPUTE THE DAILY AVERAGE INFILTRATION FOR A GIVEN MONTH FOR EACH LAND USE

                INFIL_lc(K) = RAIN(I,J,M)/MNDAY + SNOWW/MNDAY
                
! *****************************************************************************************************
! --- COMPUTE AET GIVEN TOTAL WATER STORED IN UPPER SOIL LAYER STORAGES AND PAET CALCULATED IN PET.FOR
! --- ASSUME ET IS SUPPLIED ONLY FROM UPPER LAYER NO UPWARD FLUX FROM LOWER LAYER TO UPPER LAYER
! --- NOTE THAT SAC-SMA ALLOWS ET TO ALSO BE SUPPLIED UNRESTRICTED BY LZ TENSION WATER STORAGE

                
                   DPAET = AET_lc(J, M, K)/MNDAY
               
                   ET_lc(J, M, K) = DPAET
                
! --- COMPUTE ET FROM UZ TENSION WATER STORAGE, RECALCULATE UZTWC, CALCULATE RESIDUAL ET DEMAND

                   ETUZTW_lc(J, M, K) = ET_lc(J, M, K) * (UZTWC_lc(K)/UZTWM(I))
                   
                   RESIDET_lc(J, M, K) = ET_lc(J, M, K) - ETUZTW_lc(J, M, K)
                   
                   UZTWC_lc(K) = UZTWC_lc(K) - ETUZTW_lc(J, M, K)
                   
                   ETUZFW_lc(J, M, K) = 0.0
                   
                   IF (UZTWC_lc(K).GE.0.0) GOTO 220
                   
                   ETUZTW_lc(J, M, K) = ETUZTW_lc(J, M, K) + UZTWC_lc(K)
                   
                   UZTWC_lc(K) = 0.0
                   
                   RESIDET_lc(J, M, K) = ET_lc(J, M, K) - ETUZTW_lc(J, M, K)
                   
! --- COMPUTE ET FROM UZ FREE WATER STORAGE, RECALCULATE UZFWC, CALCULATE RESIDUAL ET DEMAND                   
                   
                   IF (UZFWC_lc(K) .GE. RESIDET_lc(J, M, K)) GO TO 221
                   
                   ETUZFW_lc(J, M, K) = UZFWC_lc(K)
                   
                   UZFWC_lc(K) = 0.0
                   
                   RESIDET_lc(J, M, K) = RESIDET_lc(J, M, K) - ETUZFW_lc(J, M, K)
                   
                   GO TO 225
                   
221                ETUZFW_lc(J, M, K) = RESIDET_lc(J, M, K)

                   UZFWC_lc(K) = UZFWC_lc(K) - ETUZFW_lc(J, M, K)
                   
                   RESIDET_lc(J, M, K) = 0.0
                   
! --- REDISTRIBUTE WATER BETWEEN UZ TENSION WATER AND FREE WATER STORAGES

220                IF((UZTWC_lc(K)/UZTWM(I)).GE.(UZFWC_lc(K)/UZFWM(I))) &
                  GO TO 225

                   UZRAT_lc(K)=(UZTWC_lc(K)+UZFWC_lc(K))/(UZTWM(I)+UZFWM(I))
                      
                   UZTWC_lc(K) = UZTWM(I) * UZRAT_lc(K)
                      
                   UZFWC_lc(K) = UZFWM(I) * UZRAT_lc(K)
                        
225                IF (UZTWC_lc(K) .LT. 0.00001) UZTWC_lc(K) = 0.0

                   IF (UZFWC_lc(K) .LT. 0.00001) UZFWC_lc(K) = 0.0
                   
                   
! --- COMPUTE ET FROM LZ TENSION WATER STORAGE, RECALCULATE LZTWC, CALCULATE RESIDUAL ET DEMAND

                   ETLZTW_lc(J, M, K) = RESIDET_lc(J, M, K) * (LZTWC_lc(K) / &
                  (UZTWM(I) + LZTWM(I)))
                   
                   LZTWC_lc(K) = LZTWC_lc(K) - ETLZTW_lc(J, M, K)
                   
                   IF(LZTWC_lc(K) .GE. 0.0) GO TO 226
                   
                   ETLZTW_lc(J, M, K) = ETLZTW_lc(J, M, K) + LZTWC_lc(K)
                   
                   LZTWC_lc(K) = 0.0
                   
226                RATLZT_lc(K) = LZTWC_lc(K) / LZTWM(I)

                   RATLZ_lc(K) = (LZTWC_lc(K) + LZFPC_lc(K) + LZFSC_lc(K)) /&
				   (LZTWM(I) + LZFPM(I) + LZFSM(I))
     
                   IF (RATLZT_lc(K) .GE. RATLZ_lc(K)) GO TO 230
                  
                   LZTWC_lc(K) = LZTWC_lc(K) + (RATLZ_lc(K) - RATLZT_lc(K)) * &
                  LZTWM(I)

                   
                   LZFSC_lc(K) = LZFSC_lc(K) - (RATLZ_lc(K) - RATLZT_lc(K)) * &
                  LZTWM(I)
                   
                   IF(LZFSC_lc(K) .GE. 0.0) GO TO 230
                   
                   LZFPC_lc(K) = LZFPC_lc(K) + LZFSC_lc(K)
                   
                   LZFSC_lc(K) = 0.0
                   
230                IF (LZTWC_lc(K) .LT. 0.00001) LZTWC_lc(K) = 0.0

! --- CALCULATE TOTAL ET SUPPLIED BY UPPER AND LOWER LAYERS

                   ET_lc(J, M, K) = ETUZTW_lc(J, M, K) + ETUZFW_lc(J, M, K) + &
                  ETLZTW_lc(J, M, K)
                   
					IF (ET_lc(J,M,K) .LE. 0)   THEN
						ET_lc(J,M,K)=0.0 
					ENDIF 	
! *****************************************************************************************************
! *****************************************************************************************************
! --- COMPUTE PERCOLATION INTO SOIL WATER STORAGES AND SURFACE RUNOFF

!     --- COMPUTE WATER IN EXCESS OF UZ TENSION WATER CAPACITY (TWX)

                  TWX_lc(K) = INFIL_lc(K) + UZTWC_lc(K) - UZTWM(I)
           
                  IF (TWX_lc(K).GE.0.0) THEN
             	
!     --- IF INFIL EXCEEDS UZ TENSION WATER CAPACITY, SET UZ TENSION WATER STORAGE TO CAPACITY, 
!         REMAINDER OF INFIL GOES TO UZFWC IF EXCEEDS UZFWC EXCESS GOES TO SURFACE RUNOFF

                     UZTWC_lc(K) = UZTWM(I)     
                
                     UZFWC_lc(K) = UZFWC_lc(K) + TWX_lc(K)
                
                     IF (UZFWC_lc(K) .GT. UZFWM(I)) THEN
                
                        SURFRO_lc(K) = UZFWC_lc(K) - UZFWM(I)
                
                        UZFWC_lc(K) = UZFWM(I)
                
                     ELSE
                
                        SURFRO_lc(K) = 0.0
                
                     ENDIF 

                  ELSE        	
           

!     --- IF INFIL DOES NOT EXCEED UZ TENSION WATER CAPACITY, ALL INFIL GOES TO UZ TENSION WATER STORAGE

                     UZTWC_lc(K) = UZTWC_lc(K) + INFIL_lc(K)
             	
                  ENDIF
           
! --- COMPUTE PERCOLATION TO LZ IF FREE WATER IS AVAILABLE IN UZ

		
	          IF (UZFWC_lc(K) .GT. 0.0) THEN
		

!     --- COMPUTE PERCOLATION DEMAND FROM LZ
!? LZFPM
                     PERCM_lc(K) = LZFPM(I) * LZPK(I) + LZFSM(I) * LZSK(I)
                
                     PERC_lc(K) = PERCM_lc(K) * (UZFWC_lc(K)/UZFWM(I))
                
                     DEFR_lc(K)=1.0-((LZTWC_lc(K)+LZFPC_lc(K)+LZFSC_lc(K))/&
     (LZTWM(I)+LZFPM(I)+LZFSM(I)))
                
                     PERC_lc(K) = PERC_lc(K) * (1.0 + ZPERC(I) * (DEFR_lc(K)&
     **REXP(I)))
            

!     --- COMPARE LZ PERCOLATION DEMAND TO UZ FREE WATER AVAILABLE AND COMPUTE ACTUAL PERCOLATION

                    IF (PERC_lc(K) .LT. UZFWC_lc(K)) THEN
                
                       UZFWC_lc(K) = UZFWC_lc(K) - PERC_lc(K)
                
                    ELSE
                
                       PERC_lc(K) = UZFWC_lc(K)
                
                       UZFWC_lc(K) = 0.0
                
                    ENDIF
            
!      --- CHECK TO SEE IF PERC EXCEEDS LZ TENSION AND FREE WATER DEFICIENCY, IF SO SET PERC TO LZ DEFICIENCY


                    LZDEF_lc(K) = (LZTWC_lc(K) + LZFPC_lc(K) + LZFSC_lc(K)) - &
     (LZTWM(I) + LZFPM(I) + LZFSM(I)) + PERC_lc(K)
                    
                    IF (LZDEF_lc(K) .GT. 0.0) THEN
                    
                       PERC_lc(K) = PERC_lc(K) - LZDEF_lc(K)
          
                       UZFWC_lc(K) = UZFWC_lc(K) + LZDEF_lc(K)
                       
                    ENDIF
                
                
! --- DISRIBUTE PERCOLATED WATER INTO THE LZ STORAGES AND COMPUTE THE REMAINDER IN UZ FREE WATER STORAGE AND RESIDUAL AVAIL FOR RUNOFF
   

!     --- COMPUTE PERC WATER GOING INTO LZ TENSION WATER STORAGE AND COMPARE TO AVAILABLE STORAGE

                    PERCT_lc(K) = PERC_lc(K) * (1.0 - PFREE(I))
                
                    IF ((PERCT_lc(K) + LZTWC_lc(K)) .GT. LZTWM(I)) THEN
                
!     --- WHEN PERC IS GREATER THAN AVAILABLE TENSION WATER STORAGE, SET TENSION WATER STORAGE TO MAX, REMAINDER OF PERC GETS EVALUATED AGAINST FREE WATER STORAGE

                       PERCF_lc(K) = PERCT_lc(K) + LZTWC_lc(K) - LZTWM(I)
                
                       LZTWC_lc(K) = LZTWM(I)
                
                    ELSE
                
!     --- WHEN PERC IS LESS THAN AVAILABLE TENSION WATER STORAGE, UPDATE TENSION WATER STORAGE

                       LZTWC_lc(K) = LZTWC_lc(K) + PERCT_lc(K)
                
                       PERCF_lc(K) = 0.0
                
                    ENDIF
                
!     --- COMPUTE TOTAL PERC WATER GOING INTO LZ FREE WATER STORAGE

                    PERCF_lc(K) = PERCF_lc(K) + PERC_lc(K) * PFREE(I)                

                    IF(PERCF_lc(K) .GT. 0.0) THEN
                
!     --- COMPUTE RELATIVE SIZE OF LZ PRIMARY FREE WATER STORAGE COMPARED TO LZ TOTAL FREE WATER STORAGE

                       HPL_lc(K) = LZFPM(I) / (LZFPM(I) + LZFSM(I))
                
!     --- COMPUTE LZ PRIMARY AND SECONDARY FREE WATER CONTENT TO CAPACITY RATIOS

                       RATLP_lc(K) = LZFPC_lc(K) / LZFPM(I)
                
                       RATLS_lc(K) = LZFSC_lc(K) / LZFSM(I)
                
!     --- COMPUTE FRACTIONS AND PERCENTAGES OF FREE WATER PERC TO GO TO LZ PRIMARY STORAGE

                       FRACP_lc(K) = (HPL_lc(K) * 2.0 * (1.0 - RATLP_lc(K))) &
                      / ((1.0 - RATLP_lc(K)) + (1.0 - RATLS_lc(K)))
                
                       IF (FRACP_lc(K) .GT. 1.0) FRACP_lc(K) = 1.0

                          PERCP_lc(K) = PERCF_lc(K) * FRACP_lc(K)
                
                          PERCS_lc(K) = PERCF_lc(K) - PERCP_lc(K)
                
!     --- COMPUTE NEW PRIMARY AND SECONDARY STORAGE

!         --- COMPUTE NEW SECONDARY FREE WATER STORAGE

                          LZFSC_lc(K) = LZFSC_lc(K) + PERCS_lc(K)

                          IF(LZFSC_lc(K) .GT. LZFSM(I)) THEN
                
!         --- IF NEW SECONDARY FREE WATER STORAGE EXCEEDS CAPACITY SET SECONDARY STORAGE TO CAPACITY AND EXCESS GOES TO PRIMARY FREE WATER STORAGE

                             PERCS_lc(K) = PERCS_lc(K) - LZFSC_lc(K) + LZFSM(I)
                
                             LZFSC_lc(K) = LZFSM(I)
                          
                          ENDIF
                
            
!         --- IF NEW LZ SECONDARY FREE WATER STORAGE IS LESS THAN CAPACITY MOVE ON TO COMPUTE NEW PRIMARY FREE WATER STORAGE


                       LZFPC_lc(K) = LZFPC_lc(K) + (PERCF_lc(K) - PERCS_lc(K))

                
                       IF (LZFPC_lc(K) .GT. LZFPM(I)) THEN

!             --- IF LZ FREE PRIMARY WATER STORAGE EXCEEDS CAPACITY SET PRIMARY STORAGE TO CAPACITY AND EVALUATE EXCESS AGAINST LZ TENSION WATER STORAGE

                          LZTWC_lc(K) = LZTWC_lc(K) + LZFPC_lc(K) - LZFPM(I)
                
                          LZFPC_lc(K) = LZFPM(I)
                
                          IF (LZTWC_lc(K) .GT. LZTWM(I)) THEN

!            --- IF LZ TENSION WATER EXCEEDS CAPACITY EVALUATE EXCESS AGAINST UZ FREE WATER CAPACITY AND SET LZ TENSION WATER STORAGE TO CAPACITY

                             UZFWC_lc(K) = UZFWC_lc(K) + LZTWC_lc(K) - LZTWM(I)
                
                             LZTWC_lc(K) = LZTWM(I)
                             
                          ENDIF
                          
                       ENDIF
                       
                    ENDIF
                
		 ENDIF
		 
! ***************************************************************************************************** 
! *****************************************************************************************************                
! --- COMPUTE BASEFLOW AND UPDATE LZ PRIMARY AND SECONDARY FREE WATER STORAGES

                
!      --- COMPUTE PRIMARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

                 PBF_lc(K) = LZFPC_lc(K) * LZPK(I)
                
                 LZFPC_lc(K) = LZFPC_lc(K) - PBF_lc(K)
                
                 IF (LZFPC_lc(K) .LE. 0.0001) THEN 
                
                    PBF_lc(K) = PBF_lc(K) + LZFPC_lc(K)
                
                    LZFPC_lc(K) = 0.0
                
                 ENDIF
                

!      --- COMPUTE SECONDARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

                 SBF_lc(K) = LZFSC_lc(K) * LZSK(I)
                
                 LZFSC_lc(K) = LZFSC_lc(K) - SBF_lc(K)
                
                 IF (LZFSC_lc(K) .LE. 0.0001) THEN
                
                   SBF_lc(K) = SBF_lc(K) + LZFSC_lc(K)
                
                   LZFSC_lc(K) = 0.0
                
                 ENDIF                
                 

! *****************************************************************************************************
! --- COMPUTE INTERFLOW FROM UZ

                 INF_lc(K) = UZFWC_lc(K) * UZK(I)
                
                 IF (UZFWC_lc(K) .LT. INF_lc(K)) THEN
                 
                    INF_lc(K) = UZFWC_lc(K)
                    
                    UZFWC_lc(K) = 0.0
                 
                 ELSE
                    
                    UZFWC_lc(K) = UZFWC_lc(K) - INF_lc(K)
                
                 ENDIF

             ENDIF
             
 

! NOTE  the following is based on Law et al

! *****************************************************************************************************
! *****************************************************************************************************
	! Calculate GEP based on ET and the equation
		GEP_lc(J,M,K) = wue_k(K) * ET_lc(J,M,K) 
        IF (GEP_lc(J,M,K) .LE. 0)   THEN
			GEP_lc(J,M,K)=0.0 
		ENDIF 				
                
		RECO_lc(J,M,K)= (reco_inter(K) + reco_slope(K) * GEP_lc(J,M,K)*MNDAY)/MNDAY
		
        NEE_lc(J,M,K)=  RECO_lc(J,M,K)- GEP_lc(J,M,K)                 
! --- COMPUTE FRACTION OF EACH WATER BALANCE COMPONENT AND GEP FOR EACH LAND COVER

! *****************************************************************************************************
! -- CALCULATE THE FRACTION OF daily GEP

		GEPTEMP = GEPTEMP + GEP_lc(J,M,K)*LADUSE_lc(I,K)   
		RECOTEMP = RECOTEMP + RECO_lc(J,M,K)*LADUSE_lc(I,K)               
		NEETEMP = NEETEMP + NEE_lc(J,M,K)*LADUSE_lc(I,K)


! *****************************************************************************************************
! --- CALCULATE THE FRACTION OF daily AET
               
               AETTEMP = AETTEMP + ET_lc(J,M,K) * LADUSE_lc(I,K)
               
! *****************************************************************************************************
! -- CALCULATE THE FRACTION OF daily SURFACE RUNOFF
              
               RUNOFFTEMP = RUNOFFTEMP + SURFRO_lc(K) * LADUSE_lc(I,K)
               
! *****************************************************************************************************
! -- CALCULATE THE FRACTION OF DAILY PRIMARY BASEFLOW
              
               PBFTEMP = PBFTEMP + PBF_lc(K) * LADUSE_lc(I,K)               
               
! *****************************************************************************************************
! -- CALCULATE THE FRACTION OF DAILY SECONDARY BASEFLOW
              
               SBFTEMP = SBFTEMP + SBF_lc(K) * LADUSE_lc(I,K)               

! *****************************************************************************************************
! -- CALCULATE THE FRACTION OF DAILY INTERFLOW
              
               IFTEMP = IFTEMP + INF_lc(K) * LADUSE_lc(I,K)                                              
              
! *****************************************************************************************************

! ----CALCUALTE TOTAL RUNOFF FOR EACH LANDUSE, K (GE SUN OCT 19, 2010) 
					
            ! RUNLAND(I,J_S,M,DAY,K) = SURFRO_lc(K) + PBF_lc(K) + &
          ! SBF_lc(K) + INF_lc(K)
		  	
			! ETLAND(I,J_S,M,DAY,K) =ET_lc(J,M,K)
			
			! GEPLAND(I,J_S,M,DAY,K) =GEP_lc(J,M,K)
			
	! print*,J_S,NYEAR_S+NWARMUP,K,  GEP_lc(J,M,K)
! -- calculate the fraction of soil moisture 
          
              
              TAUZTWC = TAUZTWC + UZTWC_lc(K) * LADUSE_lc(I,K)
			  
              TAUZFWC = TAUZFWC + UZFWC_lc(K) * LADUSE_lc(I,K)
              
              TALZTWC = TALZTWC + LZTWC_lc(K) * LADUSE_lc(I,K)
              
              TALZFPC = TALZFPC + LZFPC_lc(K) * LADUSE_lc(I,K)
              
              TALZFSC = TALZFSC + LZFSC_lc(K) * LADUSE_lc(I,K)
              
              TASM = TASM + (UZTWC_lc(K)+UZFWC_lc(K)+LZTWC_lc(K)+LZFPC_lc(K) &
             +LZFSC_lc(K)) * LADUSE_lc(I,K)
                   
               TAREA = TAREA + LADUSE_lc(I,K) 

    
40         CONTINUE ! end of land cover

           
! -- CALCULATE AVG SMC

              AUZTWC = TAUZTWC / TAREA
              AUZFWC = TAUZFWC / TAREA
              ALZTWC = TALZTWC / TAREA
              ALZFPC = TALZFPC / TAREA
              ALZFSC = TALZFSC / TAREA
              ASM = TASM/TAREA

100        CONTINUE ! End of daily


           AET(I,J,M) = AETTEMP        
           RUNOFF(I,J,M) = RUNOFFTEMP
           PRIBF(I,J,M) = PBFTEMP
           SECBF(I,J,M) = SBFTEMP
           INTF(I,J,M) = IFTEMP
           SMC(I,J,M) = ASM          
           SP(I,J,M) = SNOWPACK
           AVUZTWC(I,J,M) = AUZTWC
           AVUZFWC(I,J,M) = AUZFWC
           AVLZTWC(I,J,M) = ALZTWC
           AVLZFPC(I,J,M) = ALZFPC
           AVLZFSC(I,J,M) = ALZFSC
                   
           IF (RUNOFF(I,J,M) .LT. 0.) THEN
           
           RUNOFF(I,J,M)=0.
           
           ENDIF            
     

! -- STREAMFLOW IN MILLION M3 FOR EACH HUC FOR MONTH M. HUCAREA IN SQ. METERS

          STRFLOW(I, J, M) = (RUNOFF(I,J,M) + PRIBF(I,J,M) + SECBF(I,J,M) + INTF(I,J,M)) &
      * HUCAREA(I)/1000./1000000.
           
           GEPM(I,J, M) = GEPTEMP
           RECOM(I,J,M)  = RECOTEMP
           NEEM(I,J,M) = NEETEMP
			! print*,I,J,M,GEPTEMP
      RETURN
      END
