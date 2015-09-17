!**********************************************************************C
!                                                                      C
!     *** SUBROUTINE RPSDF ***                                         C
!     Read basic input data from GENERAL.TXT and write to              !
!     BASICOUT.TXT, set up column headings in output files             C
!                                                                      !
!**********************************************************************!
      SUBROUTINE RPSDF
 
	  USE Common_var
      implicit none 
 
      CHARACTER*4 HEADNG(20)
      INTEGER ISCENARIOS
  
!      COMMON/LAND/ SPRING,SUMMER,FALL,WINTER,SI(366)

!      COMMON/GROUNDWATER/ GROUNDWATER(4000,9), ALAFA
      
!      COMMON/NODE/NONODE

!      REAL ALAFA

! --- Read in data from GENERAL.TXT and write to BASICOUT.TXT

           
      READ(1,1000) HEADNG
 1000 FORMAT(20A4)
      WRITE(77,2010) HEADNG
 2010 FORMAT(' ',20A4/)

      READ(1,*) ISCENARIOS
      
      WRITE(77,*) ISCENARIOS
2015  FORMAT('Scenario#', I10)


      READ(1,*) NGRID, NYEAR, BYEAR, NLC

      WRITE(77,2020) NGRID
 2020 FORMAT(I5,'ACTIVE GRIDS'/)
      
      WRITE(77,2030) NYEAR, BYEAR
      
 2030 FORMAT(I10,'YEARS TO BE SIMULATED AND',' FIRST YEAR =', I10)
      
      WRITE(77,2040) NLC
      
 2040 FORMAT('NUMBER OF LAND COVER CATEGORIES: ',I10)      
           
      READ (1,*) IYSTART, IYEND

      WRITE(77,2050) IYSTART, IYEND
2050  FORMAT('FOR SIMULATION SUMMARY, YEAR TO START',I10, ' , END ',I10)


      READ (1,*) FPERD

!--  reduction fraction of Leaf Area index for scenario analysis

      READ (1, *) FPERDLAI
!1033  FORMAT (F10.2) 

      WRITE(77,2058) FPERD, FPERDLAI
2058  FORMAT('DEFOREST RATE%',F10.2, /'LAI REDUCTION%=', F10.2)


       READ (1,*) SNOWPACK
!1046   FORMAT (F10.2) 
              
      WRITE(77,1047) SNOWPACK

1047  FORMAT('INTIAL SNOWPACK (MM) = :', F10.2)


!-----PRINT TITLE FOR MONTHLY OUTPUT FILE MONTHRUNOFF.TXT 
    
      WRITE (78, 1050)
1050  FORMAT('CELL,YEAR,MONTH,PRECIP,TEMP,', &
      'SMC,SNWPK,PET,AET,Sun_ET,',&
      'RUNOFF,BASEFLOW,FLOWMCMMon')
!-----PRINT TITLE FOR MONTHLY OUTPUT FILE Soil Storage.TXT          
      WRITE (900, 1055)
1055  FORMAT('CELL,YEAR,MONTH,UZTWC,UZFWC,',&
      'LZTWC,LZFPC,LZFSC')
     
!-----PRINT TITLE FOR ANNUAL OUTPUT ANNUALFLOW.TXT    
     
       WRITE (79, 1060)
1060  FORMAT ( 'CELL,YEAR,RAIN,PET,',&
      'AET,Sun_ET,RUNOFF,RUN_Pratio,ET_Pratio,RUN_ETRatio,', &
       'SNWPCKMON,RFACTOR')
                
!-----PRINT TITLE FOR SUMMARY OUTPUT SUMMARRUNOFF.TXT  

      WRITE (80, 1070)
1070  FORMAT ('CELL,RAIN,PET,',&
      'AET,RUNOFF,RUNOFF/P,ET/P,(RUN+ET)/P',&
       'RFACTOR,Y_n')

       
         WRITE (910,1080) 
         
1080    FORMAT ('WATERSHEDID,YEAR,LADUSEID,',&
      'HUCRUNOFF,FLOWVOL,LAND%,HUCAREA')        
       
       
       
         WRITE (920,1090) 
         
1090    FORMAT ('WATERSHEDID,YEAR,CROPFLOW,',&
       'FORESTFLOW,GRASSFLOW,SHRUBSAVAFLOW,URBANWATERFLOW,TFLOW') 

!	   WRITE (2003, 204) 
!            
!204      FORMAT ('VAL_1,RMES_RUNOFF,RMSE_BASEFLOW,
!     > NS_RUNOFF,NS_BASEFLOW,R_RUNOFF,R_BASEFLOW')      
      RETURN
      END
!**********************************************************************!
!                                                                      !
!     *** SUBROUTINE RPSINT ***                                        !
!     Read in landuse data from CELLINFO.TXT, calcuate change in       !
!     landuse based on percent forest decrease (if desired), write     !
!     to BASICOUT.TXT                                                  !
!     基于设定的采伐值重计算土地利用情况。                             !
!**********************************************************************!
      SUBROUTINE RPSINT 

	  Use Common_var
       implicit none

      REAL CROP

      INteger year, I , J,ID


      CHARACTER*1000 DUMY(30)
      
! --- Read and print land use data for each active cell IN THE BASIC.OUT FILE
      WRITE(77,2000)
2000  FORMAT(/'LANDUSE INFO FOR EACH SIMULATION CELL'/)
      READ (2,500) DUMY
500   FORMAT (1000A30)
      

! ----LANC = raw Landcover types    
      
      WRITE (77,500) DUMY
             
      DO 10 I=1, NGRID

      READ(2,*) ID, HUCNO(I), LATUDE(I), LONGI(I),LADUSE(I)!,HUCELE(I)
      
             
!      WRITE(77,1100) ID, HUCNO(I),LATUDE(I), LONGI(I), 
!     > (LADUSE(I,K),K=1, NLC)

1100  FORMAT(2I10, 2F10.4, I4)    
     
10    CONTINUE


! --- Read and print SOIL PARAMETERS for each active cell IN THE BASIC.OUT FILE
!
      WRITE(77,2051)
2051  FORMAT(/'SOIL PARAMETERS FOR EACH SIMULATION CELL'/)
      READ (7,550) DUMY
550   FORMAT (30A8)
      
      
      WRITE (77,550) DUMY
          
      DO 15 I=1, NGRID

      READ(7,*) ID, HUCNO(I), UZTWM(I), UZFWM(I), UZK(I), ZPERC(I),&
     REXP(I), LZTWM(I), LZFSM(I), LZFPM(I), LZSK(I),&
     LZPK(I), PFREE(I)
            
!      WRITE(*,1150) HUCNO(I), UZTWM(I), UZFWM(I), UZK(I), ZPERC(I),&
!     REXP(I), LZTWM(I), LZFSM(I), LZFPM(I), LZSK(I),&
!     LZPK(I), PFREE(I)
!    print*,I 

!! this part is used for soil data calibration
           
   !    UZTWM(I)=UZTWM(I)*1
   !    UZFWM(I)=UZFWM(I)*1
   !    UZK(I)=UZK(I)*1 
 !      ZPERC(I)=ZPERC(I)*1
     !  REXP(I)=REXP(I)*1
      LZTWM(I)=LZTWM(I)*1.3 !*(1+VAL_1(TUN1)) 
    !  LZFSM(I)=LZFSM(I) !*
       LZFPM(I)=LZFPM(I)*2.4
       LZSK(I)= LZSK(I)*0.9 !*(1+VAL_2(TUN2)) 
       LZPK(I)=LZPK(I)*1.2
!      PFREE(I)=PFREE(I) !


1150  FORMAT(I12, 11F10.4)    
     
15    CONTINUE


! ---- Converting forest to two types of croplands in the same watersheds
!------根据设定的采伐率重计算植被类型中的某些变化植被类型
 
!      DO 20 I=1, NGRID
!      
!           
!      CROP = LADUSE(I,1) + LADUSE(I,2)
!     
!      IF (CROP .NE. 0.) THEN
! 
!      LADUSE(I,1)=(LADUSE(I,1)/(LADUSE(I,1) + LADUSE(I,2))) 
!     & * FPERD * (LADUSE(I,4) + LADUSE(I,5)+ LADUSE(I,6)+   
!     &LADUSE(I,7) + LADUSE(I,8)+ LADUSE(I,11) ) 
!     &+LADUSE(I,1)
!             
!      
!      LADUSE(I,2)=(LADUSE(I,2)/(LADUSE(I,1) + LADUSE(I,2)))
!     & * FPERD * (LADUSE(I,4) + LADUSE(I,5)+ LADUSE(I,6) +   
!     & LADUSE(I,7) + LADUSE(I,8)+ LADUSE(I,11) ) 
!     & + LADUSE(I,2)
! 
!      ELSEIF (CROP .EQ. 0.) THEN
!     
!         LADUSE(I,1) = 0.5*FPERD
!     & *FPERD * (LADUSE(I,4) + LADUSE(I,5)+ LADUSE(I,6)   
!     & +LADUSE(I,7)+LADUSE(I,8)+ LADUSE(I,11) ) 
!     & +LADUSE(I,1)
!      
!        LADUSE(I,2) = 0.5
!     & * FPERD * (LADUSE(I,4) + LADUSE(I,5)+ LADUSE(I,6)+   
!     & LADUSE(I,7) + LADUSE(I,8)+ LADUSE(I,11))
!     & + LADUSE(I,2)
!     
!         ENDIF 
!                          
!      LADUSE(I,4) = LADUSE(I,4)*(1-FPERD)
!      LADUSE(I,5) = LADUSE(I,5)*(1-FPERD)
!      LADUSE(I,6) = LADUSE(I,6)*(1-FPERD)
!      LADUSE(I,7) = LADUSE(I,7)*(1-FPERD)
!      LADUSE(I,8) = LADUSE(I,8)*(1-FPERD)
!      LADUSE(I,11) = LADUSE(I,11)*(1-FPERD)
!  20    CONTINUE

      RETURN
      END

      
!**********************************************************************C
!                                                                      C
!C     *** SUBROUTINE RPSWATERUSE ***                                   C
!C     Read in HUC area, elevation, and slope from HUCAREA.TXT          C
!C     Read in return flow rate from RETURNFLOW.TXT                     C
!C     Read in water use by water resource region from HUCREGION.TXT    C
!C     Read in population info from POPULATION.TXT                      C
!C     READ IN GROUNDWATER USE BY SECTOR from GROUNDWATER.TXT           C
!C     READ IN SURFACE WATER USE BY SECTOR from WATERUSE.TXT            C
!C     calculate change in irrigation water use if desired              C
!C     Read in huc node info from NODEHUC.TXT                           C
!C     Read in average monthly flows from AVGMONFLOW.TXT                C
!C                                                                      C
!C**********************************************************************C
      SUBROUTINE RPSWATERUSE


     Use Common_var
      
!c      COMMON/POPULATION/POPULATION(4000, 150)      
!c      COMMON/HUCREGION/DMC(18,4),IRRC(18,4), PPC(18,4) 
!c      COMMON/RETURNFLOW/ RETURNFLOW(4000,8)
!c      COMMON/GROUNDWATER/ GROUNDWATER(4000,9), ALAFA      
!c      COMMON/WATERUSE/ WATERUSE(4000,8), HUCWUSE(4000)
!
! 

!      
!c      COMMON/FLOWNETWORK/IDFHUC(4000), FHUC(4000), IDTHUC(4000), 
!c     &THUC(4000)
!     
!c      COMMON/NODE/NONODE
!          
!c      COMMON/ROUTING/RECHUC(4000,50),NORECHUC(4000)
!      
!c      COMMON/IRRIGATION/DRIRR
!      
!c      REAL  POPULATION(4000,150)                   
!c      REAL HUCAREA(4000), RETURNFLOW(4000,8), WATERUSE(4000, 8) 
!c      REAL GROUNDWATER (4000, 9), HUCWUSE(4000)


      
!c      REAL DMC(18,4), IRRC(18,4), PPC(18,4)
!c      REAL DRIRR     
!      
!c      INTEGER IDFHUC(4000), FHUC(4000), IDTHUC(4000), THUC(4000)
!      
!c      INTEGER K,RECHUC(4000, 50)
!      
!c      INTEGER NORECHUC(4000), POP_FLAG 
      
      
 !     CHARACTER*10 DUMY7(30)
          

      
       
!---READ IN HUC AREA, ELEV, and slope from HUCAREA.TXT
      
           
!       READ (11, 50) DUMY7
!50     FORMAT (30A10)
! 
!       WRITE (77, 50) DUMY7
!             
!       DO 60 I = 1, NGRID
!       
!       READ (11, *) ID, IDHUC, HUCAREA(I)   !,HUCELE(I)
!       
!       print *, ID, IDHUC, HUCAREA(I)    !,HUCELE(I)
!      
!60     CONTINUE


!C---READ IN RETURN FLOW RATE from RETURNFLOW.TXT
!
!c       WRITE (77, 20)
!c20    FORMAT (/'RETURNFLOW RATES BY SECTOR IN DECIMAL POINT'/)
! 
!c       READ (15, 50) DUMY7
!c       WRITE (77, 50) DUMY7
!               
!c       DO 65 I=1, NGRID
!       
!c       READ (15, *) IDHUC, HUCN,(RETURNFLOW(I,J), J=1, 8)
!       
!C       WRITE (77,86) IDHUC, HUCN,(RETURNFLOW(I,J), J=1, 8)
!       
!c86     FORMAT(2I12, 8F10.3)
!
!c65     CONTINUE 
!     
!C-- READ IN water resource region water use info from HUCREGION.TXT
!        
!c       READ (12,50) DUMY7
!c       WRITE (77, 50) DUMY7
!            
!c       DO 55 I = 1, 18
!       
!c       READ (12, *) IREGION, (DMC(I,J), J=1,4), 
!c     >        (IRRC(I,J), J=1,4), (PPC(I,J), J=1,4)   
!     
!C       WRITE (77, 98) IREGION, (DMC(I,J), J=1,4), 
!C     >        (IRRC(I,J), J=1,4), (PPC(I,J), J=1,4)       
!          
!c55    CONTINUE
!
!c98    FORMAT (I10, 12F10.5)
!
!C-- READ IN population info from POPULATION.TXT
!    
!
!c      READ (13, 50) DUMY2
!     
!c      WRITE (77, 800)
!       
!c800   FORMAT (/'POPULATION DATA 10^3 People'/)
!
!c      WRITE (77,900) DUMY2
!       
!c900   FORMAT (30A10)
!        
!        
!c      DO 300 I = 1, NGRID
! 
!C ---J=35 => YEAR = 1994
!C ---J=91 => YEAR = 2050
!
!c         DO 400 J=35, 91
!      
!c            READ (13, *) HUCN, JYEAR, POPULATION(I, J) 
!            
!c1000        FORMAT(2I10, F12.3)
!
!c400       CONTINUE 
!
!c300   CONTINUE   
!
!
!C ----POP_FLAG=3, TIME VARIABLE POPULATION 1994-2050
!
!c      IF (POP_FLAG .EQ. 3) THEN
!
!c      DO 211 I=1, NGRID
!                
!C --- ASSIGN YEAR 1994 POPULATION DATA TO YEARS BEFORE 1994
!
!c         DO 311 J=1, 34
!
!c            POPULATION(I,J) = POPULATION(I,35)
!            
!c311      CONTINUE
!
!C --- ASSIGN YEAR 2050 POPULATION DATA TO YEARS AFTER 2050 UP TO 2100
!
!c         DO 312 J=92, 141
!
!c            POPULATION(I,J) = POPULATION(I,91)
!            
!c312      CONTINUE
!
!c211   CONTINUE   
!
!
!C ---POP_FLAG = 1, CONSTANT BASELINE POPULATION USING YEAR 2000 POPULATION
!
!c      ELSEIF (POP_FLAG .EQ. 1) THEN
!      
!c      DO 214 I=1,NGRID
!      
!c         DO 314 J=1,40
!      
!c            POPULATION(I,J) = POPULATION(I,41)
!         
!c314      CONTINUE
!
!c         DO 315 J=42,141
!      
!c            POPULATION(I,J) = POPULATION(I,41) 
!           
!c315      CONTINUE
!
!c214   CONTINUE
!
!C ---POP_FLAG = 2, CONSTANT FUTURE POPULATION USING YEAR 2050 POPULATION
!
!c      ELSEIF (POP_FLAG .EQ. 2) THEN
!      
!c      DO 216 I=1,NGRID
!      
!c         DO 316 J=1,90
!      
!c            POPULATION(I,J) = POPULATION(I,91)
!         
!c316      CONTINUE
!
!c         DO 317 J=92,141
!      
!c            POPULATION(I,J) = POPULATION(I,91) 
!            
!c317      CONTINUE
!
!c216   CONTINUE     
!
!C ---POP_FLAG = OTHER- POPULATION FLAG ERROR
!
!c      ELSE
!      
!c      WRITE (*,23)
!      
!c23    FORMAT(/'POPULATION FLAG ERROR'/) 
!
!c      ENDIF
!      
!C ----WRITE POPULATION DATA TO BASICOUT.TXT FOR VERIFICATION
!
!c      DO 218 I=1,NGRID
!      
!c         DO 318 J=1,141
!      
!C            WRITE(77,1069) I,J+1959,POPULATION(I,J)
!            
!c1069        FORMAT (2I10, F12.3)
!
!            
!c318      CONTINUE
!
!c218   CONTINUE     
!
!        
!C--READ IN GROUNDWATER WITHDRAWAL DATA BY SECTOR (8 SECTORS) IN MILLION GALON/day
!c
!c       WRITE (77, 820)
!c820    FORMAT (/'GROUNDWATER WITHDRAWAL DATA'/)
!       
!c       READ (14, 50) DUMY4
!       
!c       WRITE (77, 50) DUMY4
!       
!c       DO 80 I = 1, NGRID
!       
!c       READ (14, *) IDHUC, HUCN, (GROUNDWATER(I, J), J=1, 9)     
!
!C       WRITE (77, 1100) IDHUC, HUCN, (GROUNDWATER(I, J), J=1, 9)
!       
!c1100   FORMAT (2I12, 9F12.3)                         
!
!c80    CONTINUE      
!
!C--READ IN SURFACE WATER USE BY SECTOR (8 SECTORS)
!
!c       WRITE (77, 830)
!c830    FORMAT (/'WATERUSE DATA'/)
!       
!c       READ (16, 50) DUMY6
!c       WRITE (77,50) DUMY6
!      
!c       DO 90 I = 1, NGRID
!       
!c       READ (16, *) IDHUC, HUCN, (WATERUSE(I,J), J=1, 9)
!              
!C       WRITE (77,1200)  IDHUC,HUCN, (WATERUSE (I,J), J=1,9)       
!c1200   FORMAT(2I15, 9F12.3)
!             
!c90     CONTINUE
!
!C------WATER USE CHANGE IRRIGATION SECTOR REDUCED BY DRIRR
!c       WRITE (77, 835)
!c835    FORMAT (/'ALTERED IRRIGATION WATERUSE DATA'/)
!
!c       DO 99 I = 1, NGRID
!       
!c       WATERUSE(I,3) = WATERUSE(I,3) * (1-DRIRR)               
!       
!C       WRITE (77,1250) I, (WATERUSE (I,J), J=1,8), DRIRR
!       
!c1250   FORMAT(I10, 8F12.3, F5.2)
!             
!c99     CONTINUE
!
!
!
!C --- READ IN HUC NODE INFO FROM NODEHUC.TXT
!
!c      WRITE (77, 840)
!c840   FORMAT (/'Stream Network Info'/)
!       
!c       READ (5, 50) DUMY7
!c       WRITE (77,50) DUMY7
!             
!c       K=1
!
!C --  SET FIRST NODE = 99
!
!c       IDTHUC(0) = 99
!       
!c       DO 100 I = 1, NONODE
!       
!c          READ (5, *) IDFHUC(I), FHUC(I), IDTHUC(I), THUC(I)
!       
!C       WRITE(77,841) IDFHUC(I), FHUC(I), IDTHUC(I), THUC(I)
!       
!c841       FORMAT(4I10)
!       
!C ---  ESTABLISH RELATIONS AMONG NODE AND HUC       
!       
!c          IDHUCT = IDTHUC(I)
!c          IDHUCF = IDFHUC(I)
!
!C -- IF THERE IS A DUPLICATED HUC THEN ESTIMATE NUMBER OF HUC RECEIVING FLOWS
!       
!c          IF (IDHUCT .EQ. IDTHUC(I-1) ) THEN  
!       
!c             K = K + 1
!       
!c             RECHUC(IDHUCT, K) = IDFHUC(I) 
!             
!c          ELSE
!       
!c             K=1
!       
!c             RECHUC(IDHUCT, K) = IDFHUC(I) 
!       
!c          ENDIF       
!     
!c          NORECHUC(IDHUCT) = K    
!          
!       
!c100    CONTINUE
!
!C -----------------------------------------------------------

      RETURN
      END
      
!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE RPSLAI ***                                        C
!C     Input MONTHLY LAI, FILL IN GAPS FOR PERIODS WITH NO LAI DATA     C
!C                                                                      C
!C**********************************************************************C
      SUBROUTINE RPSLAI
      
      USE Common_var
                   
      INTEGER YEAR
      
      
      INTEGER I, J, M,Mon

      INTEGER Y_2001 ,Y_LAI_END ,Y_2012,Y_LAI_START     
     
      CHARACTER*100 TEMPHEAD3 (11)
            
   
 
!   Set default LAI for the year without LAI input-----


      IF (BYEAR .LT. 2001 ) then
           Y_LAI_START=2001-BYEAR+1
        ELSE
         Y_LAI_START=1
       ENDIF
      If (IYEND .GT. 2012) then 
        Y_LAI_END=2012-BYEAR+1
       Else
        Y_LAI_END=IYEND-BYEAR+1
      Endif

      Y_2001=2001-BYEAR+1
      Y_2012=2012-BYEAR+1

! --- READ IN LAI DATA FROM LANDLAI.TXT


      DO 201 I=1, NGRID
                
         DO 301 J= Y_LAI_START,Y_LAI_END 
         
            DO 401 M=1, 12

            IF (I .EQ. 1 .AND. J .EQ. Y_LAI_START .AND. M .EQ. 1) THEN 

               READ (8, 902) TEMPHEAD3
 
 902           FORMAT (100A11)
 
            ENDIF
                      
 
! --- LAI_* IS THE LAI FOR LANDUSE * (8 TOTAL IN LANDLAI.TXT)

         READ(8,*) HUCNO(I),YEAR,Mon,LAI(I,J,M)  
            
 !        WRITE(*,*),HUCNO(I),YEAR,Mon,LAI(I,J,M)
    
!1011        FORMAT(3I10, 8F10.2)               


401         CONTINUE 

301      CONTINUE

201   CONTINUE

! --- ASSIGN YEAR 2001 LAI DATA TO YEARS BEFORE 2001
! -----将2001年的数据赋给以前的年份
        IF  ( BYEAR .LT. 2001)  then
          DO 202 I=1, NGRID
                
             DO 302 J=1, Y_2001-1

                DO 402 M=1, 12

                LAI(I,J,M) = LAI(I,Y_2001,M)
                      
     
402             CONTINUE 

302          CONTINUE

202        CONTINUE
!
        ENDIF
!          
!C--- ASSIGN YEAR 2012 LAI DATA TO YEARS AFTER 2012
!C--- 将2012年的数据赋给以后的年份
      IF (IYEND .GT. 2012) then
          DO 203 I=1, NGRID
                
             DO 303 J=Y_2012+1, NYEAR

                DO 403 M=1, 12

                LAI(I,J,M) = LAI(I,Y_2012,M)
       

403             CONTINUE 

303          CONTINUE

203        CONTINUE
!
      ENDIF
	  
	  
	  	  
	  
! --- WRITE LAI DATA TO BASICOUT.TXT FOR VALIDATION
            

        WRITE (77, 801)
       
801    FORMAT (/'LAI DATA for each Cell'/)

 
      WRITE (77, 902) TEMPHEAD3
      

	  !************* -----------Read annual land cover data------------ ***************

!      READ (3,5001) DUMY
!5001   FORMAT (1000A30)

! ----LANC = raw Landcover types    
      
           
 !     DO 105 I=1, NGRID   ! start and end year of land cover data
        
!        DO 106 J=Y_2001,Y_2012

!      READ(3,*) HUCNO(I),YEAR,veg(I,J)  
             
     ! WRITE(*,*) HUCNO(I),YEAR,veg(I,J) 
 
!106    CONTINUE  
!105    CONTINUE
	  
	  
	  
	  
      RETURN
      END

!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE RPSCLIMATE ***                                    C
!C     Input MONTHLY CLIMATE DATA, CALCULATE ANNUAL PPT                 C
!C                                                                      C
!C**********************************************************************C
      SUBROUTINE RPSCLIMATE
      
       USE Common_var

      INTEGER YEAR
            
      INTEGER I, J, M,Mon
     
      REAL ANNPPT(50000,32)
      
      REAL SUMANPPT(50000)
      
      CHARACTER*10 TEMPHEAD (10)


      ANNPPT =0.
      
      SUMANPPT = 0.

      AAPPT = 0.
      

      DO 5000 I=1,NGRID
      
         DO 5001 J=1,NYEAR
         
            DO 5002 M=1,12
            
               IF (I .EQ. 1 .AND. J .EQ. 1 .AND. M .EQ. 1) THEN 
       
                  READ (4, 900) TEMPHEAD

                 WRITE (77, 900) TEMPHEAD
      
!                 WRITE (77, 905)
!905              FORMAT ('end of input data' )

                             
               ENDIF
        
900            FORMAT (10A10)
!910            FORMAT  (/'CLIMATE DATA', 10A10)
               
              
               READ(4,*) HUCNO(I), YEAR, Mon, RAIN(I,J,M), TEMP(I,J,M)
            
                
!1015        FORMAT(3I10, 2F10.2) 
                       
            
               ANNPPT(I, J) = ANNPPT(I, J) + RAIN(I,J,M)
               

5002        CONTINUE
       Write(99,*), I,J
            SUMANPPT(I) = SUMANPPT(I) + ANNPPT(I, J)

5001     CONTINUE


         AAPPT(I) = SUMANPPT(I)/NYEAR
                
         
!         WRITE(77,5004) HUCNO(I), AAPPT(I)
      
!5004     FORMAT(I10,F10.2)

5000  CONTINUE

      RETURN
      END

!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE RPSVALID ***                                      C
!C     Input MONTHLY YEAR FLOW  VALIDATION DATA,                        C
!C                                                                      C
!C**********************************************************************C
      SUBROUTINE RPSVALID
      
      USE Common_var

      INTEGER  YEAR
            
      INTEGER I, J, M,Mon
     
      REAL ANNPPT(10000,32)
            
      CHARACTER*10 TEMPHEAD (10)

      
      DO 20001 J=19,25

      DO 20002 M=1,12

          IF (J .eq. 1 .and. M .eq. 1) then
          
          READ (22, 29001) TEMPHEAD

          ENDIF

29001     FORMAT (10A10)

!         READ(22,*) YEAR,Mon,RUNOFF_V(J,M)!,FLOW_V(J,M),BASEFLOW_V(J,M)
!      READ (22, *) YEAR,Mon,RUNOFF_V(J,M), FLOW_V(J,M), BASEFLOW_V(J,M)

20002  CONTINUE
20001  CONTINUE


      RETURN
      END