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
	  REAL SNOWPACK
  
!      COMMON/LAND/ SPRING,SUMMER,FALL,WINTER,SI(366)

!      COMMON/GROUNDWATER/ GROUNDWATER(4000,9), ALAFA
      
!      COMMON/NODE/NONODE

!      REAL ALAFA
!!!--------------------------------------------------------------------
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

!!!--------------------------------------------------------------------
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

	   WRITE (2003, 204) 
!            
204      FORMAT ('WATERSHEDID Year Month RAIN SP  PET &
			&AET PAET RUNOFF PRIBF SECBF INTF & 
			&AVSMC EMUZTWC  EMUZFWC EMLZTWC  EMLZFPC  EMLZFSC')     


 WRITE (400, 500) 
500    FORMAT ('CELL,YEAR,MONTH,GEP(gC/m2/Month),Reco,NEE')


        WRITE (500, 600) 
600    FORMAT ('CELL,YEAR,GEP(gC/m2/yr),Reco,NEE(gC/m2/yr),',& 
              'AET(MM),PET(MM)')


        WRITE (600, 650) 
650    FORMAT ('CELL,NO_YR,GEP(gC/m2/yr),Reco,NEE')


        WRITE (700, 700)
       
700     FORMAT ('CELL,YEAR,TREE,MAMMALS,BIRD, ', &
       'AMPHIB, REPTILES, VERTEB, AET, PET')

            WRITE (800,800) 
            
800      FORMAT ('CELL,NO_YR, TREE, MAMMALS, BIRD,',&
        'AMPHIB, REPTILES, AHUCVERTEB') 

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
   !   REXP(I)=REXP(I)*1
      ! LZTWM(I)=LZTWM(I)*1.3 !*(1+VAL_1(TUN1)) 
    ! LZFSM(I)=LZFSM(I) !*
       ! LZFPM(I)=LZFPM(I)*2.4
       ! LZSK(I)= LZSK(I)*0.9 !*(1+VAL_2(TUN2)) 
       ! LZPK(I)=LZPK(I)*1.2
    ! PFREE(I)=PFREE(I) !


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

  
      
!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE RPSLAI ***                                        C
!C     Input MONTHLY LAI, FILL IN GAPS FOR PERIODS WITH NO LAI DATA     C
!C                                                                      C
!C**********************************************************************C
      SUBROUTINE RPSLAI
      
      USE Common_var
      implicit none             
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
	  implicit none
	  
      INTEGER YEAR
            
      INTEGER I, J, M,Mon
     
      REAL,POINTER :: ANNPPT(:,:),SUMANPPT(:)
      
      CHARACTER*10 TEMPHEAD (10)

      ALLOCATE ( ANNPPT(NGRID,NYEAR), SUMANPPT(NGRID))
      
      ANNPPT =0.
      
      SUMANPPT = 0.

      WHERE(AAPPT /= 0.) AAPPT=0.0
      

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

            SUMANPPT(I) = SUMANPPT(I) + ANNPPT(I, J)

5001     CONTINUE


         AAPPT(I) = SUMANPPT(I)/NYEAR
                
         
         WRITE(77,5004) HUCNO(I), AAPPT(I)
      
5004     FORMAT(I10,F10.2)

!	Print *,"I=",I
!	WRITE(*,*) HUCNO(I), AAPPT(I)

5000  CONTINUE
		
	DEALLOCATE (ANNPPT, SUMANPPT)
	
      RETURN
      END
