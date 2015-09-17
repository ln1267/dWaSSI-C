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
      
      SUBROUTINE WATERBAL(I,J,M,MNDAY)
        Use Common_var
        implicit none       
! ----------------------------------------------------------------------------     
         
      INTEGER I,J,M,IAM,DAY,MDAY,MNDAY
      
      REAL AETTEMP, RUNOFFTEMP, PBFTEMP, SBFTEMP,IFTEMP, GEPTEMP,&
            RECOTEMP, NEETEMP
          
         
      REAL ETUZTW(32,12), RESIDET(32,12), ETUZFW(32,12)
      
      REAL ETLZTW(32,12), RATLZT, RATLZ
      
      REAL SNOW, SNOWW
      
      REAL LTASM, TAREA
      
      REAL ET(32,12), SURFRO, GEP(32,12), INFIL,&
        RECO(32,12), NEE(32,12) 
      
      REAL DPAET
      
      REAL UZRAT, TWX, PERCM, PERC, DEFR, LZDEF 
      
      REAL PERCT, PERCF
      
      REAL HPL, RATLP, RATLS, FRACP, PERCP,PERCS
      
      REAL PBF, SBF, INF
      
      REAL TAUZTWC, TAUZFWC, TALZTWC, TALZFPC, TALZFSC
     
      REAL TASM,AUZTWC,AUZFWC,ALZTWC,ALZFPC,ALZFSC,ASM           
            ! TAREA,TAREA,TAUZTWC,TAUZFWC,TALZTWC,TALZFPC,TALZFSC
      
     
      INTEGER GEPFLAG
           
! *****************************************************************************************************

! ----   Allocates array RUNLAND,

!      ALLOCATE (RUNLAND(60000,32,12,31))
!	  ALLOCATE (ETLAND(60000,32,12,31))
!	  ALLOCATE (GEPLAND(60000,32,12,31))
	  
      ALLOCATE (RUNLAND(NGRID,NYEAR,12,31))
      ALLOCATE (ETLAND(NGRID,NYEAR,12,31))
      ALLOCATE (GEPLAND(NGRID,NYEAR,12,31))

! --- INITIALIZE VARIABLES FOR START OF SIMULATION

             AETTEMP =0.0
             RUNOFFTEMP = 0.0
             PBFTEMP = 0.0
             SBFTEMP = 0.0
             IFTEMP = 0.0
                          
             GEPTEMP = 0.0
             RECOTEMP = 0.0
             NEETEMP =0.0
			 
        IF (J .EQ. 1 .AND. M .EQ. 1) THEN

        IAM =0     
                                          
           UZTWC = 0.1*UZTWM(I)
           UZFWC = 0.0
           LZTWC = 0.1*LZTWM(I)
           LZFSC = 0.75*LZFSM(I)
           LZFPC = 0.75*LZFPM(I)
           
        ENDIF 
         
! *****************************************************************************************************
! *****************************************************************************************************
!----- SIMULATE SNOWPACK (SEE VAROSMARTY  ET AL., 1989)
      
             IF (TEMP(I,J, M) .LE.  -1.0) THEN
        
           SNOW = RAIN(I,J,M)
    
           SNOWPACK = SNOWPACK + SNOW
           
           IAM = 0
                                
        ELSE 
        
            IAM = IAM +1 
            
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
        
             TASM = 0.0
             TAUZTWC = 0.0
             TAUZFWC = 0.0
             TALZTWC = 0.0
             TALZFPC = 0.0
             TALZFSC = 0.0


! *****************************************************************************************************
! *****************************************************************************************************
! -- LOOP THROUGH LAND COVERS IN THE HUC AND PERFORM WATER BALANCE COMPUTATIONS
! -- ASSUMES OPEN WATER LAND COVER IS NEG.

! *****************************************************************************************************
! -- SET ET, SURFACE RUNOFF, INTERFLOW, GEP TO ZERO IF TEMPERATURE IS LE -1.0
! -- BASEFLOW STILL OCCURS

        IF (TEMP (I,J, M) .LE. -1.0) THEN
                ET(J,M) = 0.
                SURFRO = 0.
                INF = 0.
                GEP(J,M) = 0.
                
                
! -- COMPUTE PRIMARY BASEFLOW WHEN T <= -1.0

                PBF = LZFPC * LZPK(I)
                LZFPC = LZFPC - PBF
                IF (LZFPC .LE. 0.0001) THEN 
                   PBF = PBF + LZFPC
                   LZFPC = 0.0
                ENDIF
                
! -- COMPUTE SECONDARY BASEFLOW WHEN T <= -1.0

                SBF = LZFSC * LZSK(I)
                LZFSC = LZFSC - SBF
                IF (LZFSC .LE. 0.0001) THEN
                   SBF = SBF + LZFSC
                   LZFSC = 0.0
                ENDIF  

! Test Output
!
!Write(*,9881) 
!9881 Format("Check the following variables: SNOWPACK; PBF;SBF")
!Write(*,*) I,J,M,SNOWPACK,PBF,SBF

        ELSE
                  
! **************************----Trmperature > -0.1 -------------*******************************************************
! *****************************************************************************************************
! -- COMPUTE THE DAILY AVERAGE INFILTRATION FOR A GIVEN MONTH FOR EACH LAND USE
!---流域植被类型日水分输入量=降水+融雪
                INFIL = RAIN(I,J,M)/MNDAY + SNOWW/MNDAY
          
            
! *****************************************************************************************************
! --- COMPUTE AET GIVEN TOTAL WATER STORED IN UPPER SOIL LAYER STORAGES AND PAET CALCULATED IN PET.FOR
! --- ASSUME ET IS SUPPLIED ONLY FROM UPPER LAYER NO UPWARD FLUX FROM LOWER LAYER TO UPPER LAYER
! --- NOTE THAT SAC-SMA ALLOWS ET TO ALSO BE SUPPLIED UNRESTRICTED BY LZ TENSION WATER STORAGE

                
                   DPAET = PAET(J, M)/MNDAY
               
                   ET(J, M) = DPAET
                
!		开始计算植被类型K 每天的ET、
! --- COMPUTE ET FROM UZ TENSION WATER STORAGE, RECALCULATE UZTWC, CALCULATE RESIDUAL ET DEMAND

                   ETUZTW(J,M) = ET(J,M) * (UZTWC/UZTWM(I))
                   
                   RESIDET(J,M) = ET(J,M) - ETUZTW(J,M)
                   
                   UZTWC = UZTWC - ETUZTW(J,M)
                   
                   ETUZFW(J,M) = 0.0
                   
                   IF (UZTWC.GE.0.0) GOTO 220
                   
                   ETUZTW(J,M) = ETUZTW(J,M) + UZTWC
                   
                   UZTWC = 0.0
                   
                   RESIDET(J,M) = ET(J,M) - ETUZTW(J,M)
                   
! --- COMPUTE ET FROM UZ FREE WATER STORAGE, RECALCULATE UZFWC, CALCULATE RESIDUAL ET DEMAND                   
                   
                   IF (UZFWC .GE. RESIDET(J,M)) GO TO 221
                   
                   ETUZFW(J,M) = UZFWC
                   
                   UZFWC = 0.0
                   
                   RESIDET(J,M) = RESIDET(J,M) - ETUZFW(J,M)
                   
                   GO TO 225
                   
221                ETUZFW(J,M) = RESIDET(J,M)

                   UZFWC = UZFWC - ETUZFW(J,M)
                   
                   RESIDET(J,M) = 0.0
                   
! --- REDISTRIBUTE WATER BETWEEN UZ TENSION WATER AND FREE WATER STORAGES

220                IF((UZTWC/UZTWM(I)).GE.(UZFWC/UZFWM(I))) GO TO 225

                   UZRAT=(UZTWC+UZFWC)/(UZTWM(I)+UZFWM(I))
                      
                   UZTWC = UZTWM(I) * UZRAT
                      
                   UZFWC = UZFWM(I) * UZRAT
                        
225                IF (UZTWC .LT. 0.00001) UZTWC = 0.0

                   IF (UZFWC .LT. 0.00001) UZFWC = 0.0
                   
                   
! --- COMPUTE ET FROM LZ TENSION WATER STORAGE, RECALCULATE LZTWC, CALCULATE RESIDUAL ET DEMAND

                   ETLZTW(J,M) = RESIDET(J,M) * (LZTWC / &
                  (UZTWM(I) + LZTWM(I)))
                   
                   LZTWC = LZTWC - ETLZTW(J,M)
                   
                   IF(LZTWC .GE. 0.0) GO TO 226
                   
                   ETLZTW(J,M) = ETLZTW(J,M) + LZTWC
                   
                   LZTWC = 0.0
                   
226                RATLZT = LZTWC / LZTWM(I)

                   RATLZ = (LZTWC + LZFPC + LZFSC) / &
                  (LZTWM(I) + LZFPM(I) + LZFSM(I))
     
                   IF (RATLZT .GE. RATLZ) GO TO 230
                  
                   LZTWC = LZTWC + (RATLZ - RATLZT)*LZTWM(I)

                   
                   LZFSC = LZFSC - (RATLZ - RATLZT)*LZTWM(I)
                   
                   IF(LZFSC .GE. 0.0) GO TO 230
                   
                   LZFPC = LZFPC + LZFSC
                   
                   LZFSC = 0.0
                   
230                IF (LZTWC .LT. 0.00001) LZTWC = 0.0

! Test Output
!
!Write(*,9882) 
!9882 Format("Check the following variables: LZTWC; LZFSC;LZFPC;UZFWC,ETLZTW,ETUZTW),ETUZFW")
!Write(*,*) I,J,M,LZTWC,LZFSC,LZFPC,UZTWC,UZFWC,ETLZTW(J,M),ETUZTW(J,M),ETUZFW(J,M)


! --- CALCULATE TOTAL ET SUPPLIED BY UPPER AND LOWER LAYERS

                   ET(J,M) = ETUZTW(J,M) + ETUZFW(J,M) + ETLZTW(J,M)

! for check
!WRITE(99,*) 'ET=',ET(J,M),'ETUZTW=',ETUZTW(J,M) ,'ETUZFW=',ETUZFW(J,M),'ETLZTW=', ETLZTW(J,M)


                  IF (ET(J,M) .LT. 0.00001) ET(J,M) = 0.0
! *****************************************************************************************************
! *****************************************************************************************************
! --- COMPUTE PERCOLATION INTO SOIL WATER STORAGES AND SURFACE RUNOFF

!     --- COMPUTE WATER IN EXCESS OF UZ TENSION WATER CAPACITY (TWX)

                  TWX = INFIL + UZTWC - UZTWM(I)
           
                  IF (TWX.GE.0.0) THEN
             	
!     --- IF INFIL EXCEEDS UZ TENSION WATER CAPACITY, SET UZ TENSION WATER STORAGE TO CAPACITY, 
!         REMAINDER OF INFIL GOES TO UZFWC IF EXCEEDS UZFWC EXCESS GOES TO SURFACE RUNOFF

                     UZTWC = UZTWM(I)     
                
                     UZFWC = UZFWC + TWX
                
                     IF (UZFWC .GT. UZFWM(I)) THEN
                
                        SURFRO = UZFWC - UZFWM(I)
                
                        UZFWC = UZFWM(I)
                
                     ELSE
                
                        SURFRO = 0.0
                
                     ENDIF 

                  ELSE        	
           

!     --- IF INFIL DOES NOT EXCEED UZ TENSION WATER CAPACITY, ALL INFIL GOES TO UZ TENSION WATER STORAGE

                    UZTWC = UZTWC + INFIL
					SURFRO = 0.0
                  ENDIF
           
! --- COMPUTE PERCOLATION TO LZ IF FREE WATER IS AVAILABLE IN UZ

		
	          IF (UZFWC .GT. 0.0) THEN
		

!     --- COMPUTE PERCOLATION DEMAND FROM LZ

                     PERCM = LZFPM(I) * LZPK(I) + LZFSM(I) * LZSK(I)
                
                     PERC = PERCM * (UZFWC/UZFWM(I))
                
                     DEFR=1.0-((LZTWC+LZFPC+LZFSC)/ (LZTWM(I)+LZFPM(I)+LZFSM(I)))
      
                
                     PERC = PERC * (1.0 + ZPERC(I) * (DEFR**REXP(I)))
            

!     --- COMPARE LZ PERCOLATION DEMAND TO UZ FREE WATER AVAILABLE AND COMPUTE ACTUAL PERCOLATION

                    IF (PERC .LT. UZFWC) THEN
                
                       UZFWC = UZFWC - PERC
                
                    ELSE
                
                       PERC = UZFWC
                
                       UZFWC = 0.0
                
                    ENDIF
            
!      --- CHECK TO SEE IF PERC EXCEEDS LZ TENSION AND FREE WATER DEFICIENCY, IF SO SET PERC TO LZ DEFICIENCY


                    LZDEF = (LZTWC + LZFPC + LZFSC) - (LZTWM(I) + LZFPM(I) + LZFSM(I)) + PERC
                    
                    IF (LZDEF .GT. 0.0) THEN
                    
                       PERC = PERC - LZDEF
          
                       UZFWC = UZFWC + LZDEF
                       
                    ENDIF
                
                
! --- DISRIBUTE PERCOLATED WATER INTO THE LZ STORAGES AND COMPUTE THE REMAINDER IN UZ FREE WATER STORAGE AND RESIDUAL AVAIL FOR RUNOFF
   

!     --- COMPUTE PERC WATER GOING INTO LZ TENSION WATER STORAGE AND COMPARE TO AVAILABLE STORAGE

                    PERCT = PERC * (1.0 - PFREE(I))
                
                    IF ((PERCT + LZTWC) .GT. LZTWM(I)) THEN
                
!     --- WHEN PERC IS GREATER THAN AVAILABLE TENSION WATER STORAGE, SET TENSION WATER STORAGE TO MAX, REMAINDER OF PERC GETS EVALUATED AGAINST FREE WATER STORAGE

                       PERCF = PERCT + LZTWC - LZTWM(I)
                
                       LZTWC = LZTWM(I)
                
                    ELSE
                
!     --- WHEN PERC IS LESS THAN AVAILABLE TENSION WATER STORAGE, UPDATE TENSION WATER STORAGE

                       LZTWC = LZTWC + PERCT
                
                       PERCF = 0.0
                
                    ENDIF
                
!     --- COMPUTE TOTAL PERC WATER GOING INTO LZ FREE WATER STORAGE

                    PERCF = PERCF + PERC * PFREE(I)            

                    IF(PERCF .GT. 0.0) THEN
                
!     --- COMPUTE RELATIVE SIZE OF LZ PRIMARY FREE WATER STORAGE COMPARED TO LZ TOTAL FREE WATER STORAGE

                       HPL = LZFPM(I) / (LZFPM(I) + LZFSM(I))
                
!     --- COMPUTE LZ PRIMARY AND SECONDARY FREE WATER CONTENT TO CAPACITY RATIOS

                       RATLP = LZFPC / LZFPM(I)
                
                       RATLS = LZFSC / LZFSM(I)
                
!     --- COMPUTE FRACTIONS AND PERCENTAGES OF FREE WATER PERC TO GO TO LZ PRIMARY STORAGE

                       FRACP = (HPL * 2.0 * (1.0 - RATLP)) &
                      / ((1.0 - RATLP) + (1.0 - RATLS))
                
                       IF (FRACP .GT. 1.0) FRACP = 1.0

                          PERCP = PERCF * FRACP
                
                          PERCS = PERCF - PERCP
                
!     --- COMPUTE NEW PRIMARY AND SECONDARY STORAGE

!         --- COMPUTE NEW SECONDARY FREE WATER STORAGE

                          LZFSC = LZFSC + PERCS

                          IF(LZFSC .GT. LZFSM(I)) THEN
                
!         --- IF NEW SECONDARY FREE WATER STORAGE EXCEEDS CAPACITY SET SECONDARY STORAGE TO CAPACITY AND EXCESS GOES TO PRIMARY FREE WATER STORAGE

                             PERCS = PERCS - LZFSC + LZFSM(I)
                
                             LZFSC = LZFSM(I)
                          
                          ENDIF
                
            
!         --- IF NEW LZ SECONDARY FREE WATER STORAGE IS LESS THAN CAPACITY MOVE ON TO COMPUTE NEW PRIMARY FREE WATER STORAGE


                       LZFPC = LZFPC + (PERCF - PERCS)

                
                       IF (LZFPC .GT. LZFPM(I)) THEN

!             --- IF LZ FREE PRIMARY WATER STORAGE EXCEEDS CAPACITY SET PRIMARY STORAGE TO CAPACITY AND EVALUATE EXCESS AGAINST LZ TENSION WATER STORAGE

                          LZTWC = LZTWC + LZFPC - LZFPM(I)
                
                          LZFPC = LZFPM(I)
                
                          IF (LZTWC .GT. LZTWM(I)) THEN

!            --- IF LZ TENSION WATER EXCEEDS CAPACITY EVALUATE EXCESS AGAINST UZ FREE WATER CAPACITY AND SET LZ TENSION WATER STORAGE TO CAPACITY

                             UZFWC = UZFWC + LZTWC - LZTWM(I)
                
                             LZTWC = LZTWM(I)
                             
                          ENDIF
                          
                       ENDIF
                       
                    ENDIF
                
			   ENDIF
		 
! ***************************************************************************************************** 
! *****************************************************************************************************                
! --- COMPUTE BASEFLOW AND UPDATE LZ PRIMARY AND SECONDARY FREE WATER STORAGES

                
!      --- COMPUTE PRIMARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

                 PBF = LZFPC * LZPK(I)
                
                 LZFPC = LZFPC - PBF
                
                 IF (LZFPC .LE. 0.0001) THEN 
                
                    PBF = PBF + LZFPC
                
                    LZFPC = 0.0
                
                 ENDIF
                

!      --- COMPUTE SECONDARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

                 SBF = LZFSC * LZSK(I)
                
                 LZFSC = LZFSC - SBF
                
                 IF (LZFSC .LE. 0.0001) THEN
                
                   SBF = SBF + LZFSC
                
                   LZFSC = 0.0
                
                 ENDIF                
                 

! *****************************************************************************************************
! --- COMPUTE INTERFLOW FROM UZ

                 INF = UZFWC * UZK(I)
                
                 IF (UZFWC .LT. INF) THEN
                 
                    INF = UZFWC
                    
                    UZFWC = 0.0
                 
                 ELSE
                    
                    UZFWC = UZFWC - INF
                
                 ENDIF

        ENDIF
		
!Print *, 'Finish calculate Water balances and Soil Water Content'		
! **************************----Finish calculate Water balances and Soil water--------******************************************************


! for check
!WRITE(99,*) I,J,M,Day,' ET=', ET(J,M),'TEMP=',TEMP (I,J, M),'UZTWC=',UZTWC,'PBF=',PBF,'SBF=',SBF,'LZFPC=',LZFPC ,&
!'LZFSC=',LZFSC,'UZFWC=', UZFWC,'SURFRO=', SURFRO,'INF=', INF


        GEPFLAG = 2
            
        IF (GEPFLAG .EQ. 1) THEN 

! NOTE  the following is based on Law et al

! *****************************************************************************************************
! *****************************************************************************************************
! -- Caculate GEP based on Law et al (2002) paper, GEP =f(monthly ET, ECOSYSTEMS)
! --- LUMPED CROP, GRASSLAND, SHRUB, SAVANNAH, AND WATER/URBAN/BARREN
! --- MIXED FOREST SHOULD BE AVG OF DECID/EVERGREEN?  EVERGREEN?

! --  SHRUB, SAVANNAH, AND WATER/URBAN/BARREN

               IF (LADUSE(I).EQ. 9 .OR. LADUSE(I).EQ. 10 .OR. LADUSE(I) &
			   .GE. 12) THEN 
     
               GEP(J,M) = (3.2 * ET(J,M) * MNDAY - 0.4)/MNDAY

! -- DECIDUOUS FOREST
 
               ELSEIF (LADUSE(I) .EQ. 5  .OR. LADUSE(I) .EQ. 6) THEN 

               GEP(J,M) = (3.2 * ET(J,M)*MNDAY - 0.4)/MNDAY


! -- EVERGREEN FOREST

               ELSEIF (LADUSE(I) .EQ. 4  .OR. LADUSE(I) .EQ. 8) THEN 

               GEP(J,M) = (2.4 * ET(J,M)*MNDAY + 30.4)/MNDAY
               
! -- MIXED FOREST (SAME AS EVERGREEN)
               
               ELSEIF (LADUSE(I) .EQ. 7  .OR. LADUSE(I) .EQ. 11) THEN 
               
               GEP(J,M) = (2.4 * ET(J,M)*MNDAY + 30.4)/MNDAY


! -- crop lands
               ELSEIF (LADUSE(I) .EQ. 1  .OR. LADUSE(I) .EQ. 2) THEN 
               
               GEP(J,M) = (3.06 * ET(J,M)*MNDAY - 31.6)/MNDAY
               

! -- Grasslands

               ELSEIF (LADUSE(I) .EQ. 3) THEN 
               
               GEP(J,M) = (3.39 * ET(J,M)*MNDAY - 67.88)/MNDAY

              
               ENDIF
                     
               IF (GEP(J,M) .LE. 0.) THEN
               
                GEP(J,M) = 0.                  
               
               ENDIF
			   
        ElSEIF (GEPFLAG .eq. 3) then
! IF GEPFLAG= 3
! CALCULATING DAILY GEP G C/M2/DAY
! NOTE  the following is based on New Analysis by Asko (Aug 24, 2010)
!----根据不同的植被类型重编写下面的代码

! -- CROP

               IF (LADUSE(I).EQ.1) THEN 

               GEP(J,M) = 4.5 * ET(J,M)
!---------论文中的公式
               RECO(J,M)= 40.6 + 0.43 * GEP(J,M)*MNDAY
!---------原始计算公式 		
!               RECO(J,M)= VAL_1(TUN1) + VAL_2(TUN2) * GEP(J,M)*MNDAY      !11.14 1.85
			  
			   
! -- Close SHRUBLANDS                   
               
               ELSEIF (LADUSE(I) .EQ. 4) THEN                

               GEP(J,M) = 1.4 * ET(J,M)  
                
               RECO(J,M)= 11.4 + 0.69 * GEP(J,M)*MNDAY
                
! -- DECIDUOUS Broadleaf FOREST
 
               ELSEIF (LADUSE(I) .EQ. 3) THEN 

               GEP(J,M) = 2.4* ET(J,M) 
!-------论文公式				   
               RECO(J,M)= 30.8 + 0.45 * GEP(J,M)*MNDAY	
!-------原始计算公式
!			   RECO(J,M)= 24.12 + 1.49 * ET(J,M)*MNDAY	


! -- Evergreen Broadleaf FOREST
 
               ELSEIF (LADUSE(I) .EQ. 0) THEN 

               GEP(J,M) = 2.6* ET(J,M)              
               RECO(J,M)= 19.6 + 0.61 * GEP(J,M)*MNDAY			   

!---- Evergreen Needleleaf Forest
               ELSEIF (LADUSE(I) .EQ. 5) THEN                

               GEP(J,M) = 2.14* ET(J,M)
               RECO(J,M)= 9.9 + 0.68 * GEP(J,M)*MNDAY			   
               
! -- GRASSLANDS               
                ELSEIF (LADUSE(I) .EQ. 6) THEN                
               
               GEP(J,M) = 2.25 * ET(J,M)
!------ 论文公式
              RECO(J,M)= 18.9 + 0.64*GEP(J,M)*MNDAY
!-------原始计算公式	
!			   RECO(J,M)= 14.2 + 1.42 * ET(J,M)*MNDAY	
               
!---- MIXED FOREST
               
               ELSEIF (LADUSE(I) .EQ. 7) THEN 
               
               GEP(J,M) =2.5 * ET(J,M)
               IF (TEMP(I,J,M) .LE. -1.0) THEN 
                
                GEP(J,M) = 0.0
                
               ENDIF 
                           
               RECO(J,M)= 24.44 + 0.62 * GEP(J,M)*MNDAY

! -- Open Shrublands                   
               
		      ELSEIF (LADUSE(I) .EQ. 8) THEN                

               GEP(J,M) =  1.42* ET(J,M)
               RECO(J,M)= 9.7 + 0.56 * GEP(J,M)*MNDAY
                                           
! -- SAVANNAS                   
               
	          ELSEIF (LADUSE(I).EQ.  9) THEN                

               GEP(J,M) = 1.26* ET(J,M) !
               RECO(J,M)= 25.2 + 0.53 * GEP(J,M)*MNDAY
       
         
! -- Wetlands                     
               
	           ELSEIF (LADUSE(I) .EQ. 10) THEN                

               GEP(J,M) = 1.66* ET(J,M)
               RECO(J,M)= 7.8 + 0.56 * GEP(J,M)*MNDAY     
! -- Wet Savanna                     
               
	           ELSEIF (LADUSE(I) .EQ. 11) THEN                

               GEP(J,M) = 1.49* ET(J,M)
               RECO(J,M)= 14.7 + 0.63 * GEP(J,M)*MNDAY
 			   
! -- URBAN/BARRENS/WATRE BODY (SAME AS OPEN SHRUB)                  
               
               ELSE
               
               GEP(J,M) = 0.
               RECO(J,M) =0.
              
              
               ENDIF

               IF (GEP(J,M) .LE. 0.) THEN
                      GEP(J,M) = 0.                  
               ENDIF
                                     
        ELSE
!*****************************************************************************************************
! *****************************************************************************************************
! IF GEPFLAG= 2
! CALCULATING DAILY GEP G C/M2/DAY
! NOTE  the following is based on New Analysis by Asko (Aug 24, 2010)
!----根据不同的植被类型重编写下面的代码

! -- CROP

               IF (LADUSE(I).EQ.12.0) THEN 

               GEP(J,M) = 4.5 * ET(J,M)
!---------论文中的公式
!              RECO(J,M)= 40.6 + 1.35 * ET(J,M)*MNDAY
!---------原始计算公式 		
               RECO(J,M)= 11.4 + 1.85 * GEP(J,M)*MNDAY      !11.14 1.85
			  
			   
! -- Close SHRUBLANDS                   
               
               ELSEIF (LADUSE(I) .EQ. 6.0) THEN                

               GEP(J,M) = 1.4 * ET(J,M)  
                
               RECO(J,M)= 11.4 + 0.95 * ET(J,M)*MNDAY
                
!-- DECIDUOUS Broadleaf FOREST
 
               ELSEIF (LADUSE(I) .EQ. 4.0) THEN 

               GEP(J,M) = 2.4* ET(J,M) 
!-------论文公式				   
!               RECO(J,M)= 30.8 + 1.44 * ET(J,M)*MNDAY	
!-------原始计算公式
			   RECO(J,M)= 24.12 + 1.49 * ET(J,M)*MNDAY	


! -- Evergreen Broadleaf FOREST
 
               ELSEIF (LADUSE(I) .EQ. 2) THEN 

               GEP(J,M) = 2.6* ET(J,M)              
               RECO(J,M)= 19.6 + 1.58 * ET(J,M)*MNDAY			   

!---- Evergreen Needleleaf Forest
               ELSEIF (LADUSE(I) .EQ. 1) THEN                

               GEP(J,M) = 2.14* ET(J,M)
               RECO(J,M)= 9.9 + 1.67 * ET(J,M)*MNDAY			   
               
! -- GRASSLANDS               
                ELSEIF (LADUSE(I) .EQ. 8 .or. LADUSE(I) .EQ. 9 .or. LADUSE(I) .EQ. 10 ) THEN    
               
               GEP(J,M) = 2.25 * ET(J,M)
!------ 论文公式
!               RECO(J,M)= 18.9 + 1.36* ET(J,M)*MNDAY
!-------原始计算公式	
			   RECO(J,M)= 14.2 + 1.42 * ET(J,M)*MNDAY	
               
!---- MIXED FOREST
               
               ELSEIF (LADUSE(I) .EQ. 5) THEN 
               
               GEP(J,M) =2.5 * ET(J,M)                         
               RECO(J,M)= 24.44 + 1.70 * GEP(J,M)*MNDAY

! -- Open Shrublands                   
               
			   ELSEIF (LADUSE(I) .EQ. 7) THEN                

               GEP(J,M) =  1.42* ET(J,M)
               RECO(J,M)= 9.7 + 0.74 * ET(J,M)*MNDAY
                                           
! -- SAVANNAS                   
               
			   ELSEIF (LADUSE(I).EQ. 20  ) THEN                

               GEP(J,M) = 1.26* ET(J,M) !
               RECO(J,M)= 25.2 + 0.67 * ET(J,M)*MNDAY
         
! -- Wetlands                     
               
			   ELSEIF (LADUSE(I) .EQ. 11) THEN                

               GEP(J,M) = 1.66* ET(J,M)
               RECO(J,M)= 7.8 + 0.93 * ET(J,M)*MNDAY     
! -- Wet Savanna                     
               
			   ELSEIF (LADUSE(I) .EQ. 20 ) THEN                

               GEP(J,M) = 1.49* ET(J,M)
               RECO(J,M)= 14.7 + 0.94 * ET(J,M)*MNDAY
 			   
! -- URBAN/BARRENS/WATRE BODY (SAME AS OPEN SHRUB)                  
               
			   ELSE
               
               GEP(J,M) = 0.
               RECO(J,M) =0.
              
              
			   ENDIF

               IF (GEP(J,M) .LE. 0.) THEN
                      GEP(J,M) = 0.                  
               ENDIF
                                   
        ENDIF    

! **************************----Finish calculate Carbon balances--------************************************************
		
                RECO(J,M) = RECO(J,M)/MNDAY 
                                                            
                NEE(J,M) = -GEP(J,M) + RECO(J,M)
				
! for check
!print *, I,J,M,' LANDUSE=', LADUSE(I),' ET=', ET(J,M),' GEP=', GEP(J,M),' RECO=', RECO(J,M)
                                    
! --- COMPUTE FRACTION OF EACH WATER BALANCE COMPONENT AND GEP FOR EACH LAND COVER

! *****************************************************************************************************
! -- CALCULATE THE daily GEP

               GEPTEMP = GEPTEMP + GEP(J,M)  
               RECOTEMP = RECOTEMP + RECO(J,M)            
               NEETEMP = NEETEMP + NEE(J,M)
                            

! *****************************************************************************************************
! --- CALCULATE THE daily AET
               
               AETTEMP = AETTEMP + ET(J,M)
               
! *****************************************************************************************************
!--- CALCULATE THE daily SURFACE RUNOFF
              
               RUNOFFTEMP = RUNOFFTEMP + SURFRO
               
! *****************************************************************************************************
!--- CALCULATE THE DAILY PRIMARY BASEFLOW
              
               PBFTEMP = PBFTEMP + PBF              
               
! *****************************************************************************************************
!--- CALCULATE THE DAILY SECONDARY BASEFLOW
              
               SBFTEMP = SBFTEMP + SBF                

! *****************************************************************************************************
!--- CALCULATE THE DAILY INTERFLOW
              
               IFTEMP = IFTEMP + INF                   
! *****************************************************************************************************

            RUNLAND(I,J,M,DAY) = SURFRO + PBF + SBF + INF
            ETLAND(I,J,M,DAY) = ET(J,M)
            GEPLAND(I,J,M,DAY) = GEP(J,M)
			
!--- calculate the soil moisture 
              
              TAUZTWC = TAUZTWC + UZTWC
              
              TAUZFWC = TAUZFWC + UZFWC
              
              TALZTWC = TALZTWC + LZTWC
              
              TALZFPC = TALZFPC + LZFPC
              
              TALZFSC = TALZFSC + LZFSC
              
              TASM = TASM + (UZTWC+UZFWC+LZTWC+LZFPC+LZFSC)
                      
!40         CONTINUE
           
!! -- CALCULATE AVG SMC
!
!              AUZTWC = TAUZTWC / TAREA
!              AUZFWC = TAUZFWC / TAREA
!              ALZTWC = TALZTWC / TAREA
!              ALZFPC = TALZFPC / TAREA
!              ALZFSC = TALZFSC / TAREA
!              ASM = TASM/TAREA

100        CONTINUE


           AET(M) = AETTEMP        
           RUNOFF(M) = RUNOFFTEMP
           PRIBF(M) = PBFTEMP
           SECBF(M) = SBFTEMP
           INTF(M) = IFTEMP
           SMC (M) = TASM          
           SP(M) = SNOWPACK
           AVUZTWC(M) = TAUZTWC
           AVUZFWC(M) = TAUZFWC
           AVLZTWC(M) = TALZTWC
           AVLZFPC(M) = TALZFPC
           AVLZFSC(M) = TALZFSC
                   
           IF (RUNOFF(M) .LT. 0.) THEN
           
           RUNOFF(M)=0.
           
           ENDIF            
       
           GEPM(I,J, M) = GEPTEMP
           RECOM(I,J,M)  = RECOTEMP
           NEEM(I,J,M) = NEETEMP

! -- STREAMFLOW IN MILLION M3 FOR EACH HUC FOR MONTH M. HUCAREA IN SQ. METERS 
        STRFLOW(I, J, M) = (RUNOFF(M) + PRIBF(M) + SECBF(M) + INTF(M))*25/1000. 
        ! 64 is the area of each cell (KM2)


! TEST OUTPUT

!WRITE(99,*) 'ICELL=',I,'Year=',J,'Month=',M,'TEMP=',TEMP(I,J,M),'RAINFALL=',RAIN(I,J,M),'AET=',AET(M),'RUNOFF=',RUNOFF(M),&
!'INTF=',INTF(M),'SMC=',SMC (M),'SP=',SP(M), 'PRIBF=',PRIBF(M),'SECBF=',SECBF(M),&
!    'AVUZTWC=',AVUZTWC(M),'AVUZFWC=',AVLZTWC(M),'AET=',AVLZTWC(M),&
!    'AVLZFPC=',AVLZFPC(M),'AVLZFSC=',AVLZFSC(M),'GEPM=',GEPM(I,J, M) 
12133 format(I6,I6,I4,13F10.3)
! --- Return

! Deallocates array RUNLAND,ETLAND,GEPLAND
      
      DEALLOCATE (RUNLAND,ETLAND,GEPLAND)

      RETURN
      END
