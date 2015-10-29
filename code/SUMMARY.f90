!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE SUMMARY_MONTH ***                                        C
!C     WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT            C
!C     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT             C
!C     CALCULATE TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP     C
!C     PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT          C
!C                                                                      C
!C                                                                      C
!C**********************************************************************C
      SUBROUTINE SUMMARY_MONTH (I, J)
      
	     USE Common_var
         implicit none 
! -----------------------------------------------------------------------------     
      INTEGER I,J,IM
      REAL TANURAIN, TANUPET,TANUAET,TANUPAET,TANURUN
      REAL ISM,TSP,TDM

      REAL TSNOWP
     
      INTEGER IDY, ISNOWP
            
      REAL RAINSQ, F
      
!-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
         
      IDY = J + BYEAR - 1
         
      TANURAIN =0.
      TANUPET= 0.
      TANUAET= 0.
	  TANUPAET= 0.
      TANURUN= 0.
      TSNOWP = 0.
         
      ISM = 0
      TSP = 0.
      TDM = 0.
         
      ISNOWP = 0
      
      RAINSQ =0. 
       
  
        IF (IDY .GE. IYSTART .AND. IDY .LE. IYEND) THEN        

         
		    DO 100 IM = 1, 12     
	
				RUN_HRU(I,J,IM)=RUNOFF(I,J,IM) + PRIBF(I,J,IM) + SECBF(I,J,IM) + INTF(I,J,IM)
				BASE_HRU(I,J,IM)=PRIBF(I,J,IM) + SECBF(I,J,IM)
				TRUNOFF(I,J,IM)= RUNOFF(I,J,IM) + PRIBF(I,J,IM) + SECBF(I,J,IM) + INTF(I,J,IM)
				BASEFLOW(I,J,IM)=PRIBF(I,J,IM) + SECBF(I,J,IM)
!------PRINT MONTHLY WATER BALANCE DATA TO MONTHFLOW.TXT

! For Water ballance output and Check
				! WRITE(2003,2325) I,IDY,IM,RAIN(I,J,IM),SP(I,J,IM), PET(I,J,IM), &
				! AET(I,J,IM),PAET(I,J,IM),RUNOFF(I,J,IM),PRIBF(I,J,IM),SECBF(I,J,IM),&
				! INTF(I,J,IM),AVSMC(I,J,IM),EMUZTWC(I,J,IM), EMUZFWC(I,J,IM),&
					! EMLZTWC(I,J,IM), EMLZFPC(I,J,IM), EMLZFSC(I,J,IM)
					
2325            FORMAT (I10, I6, I4,15F10.1)        
				   
			    ! WRITE(78,2025) I,IDY, IM, RAIN(I,J,IM),TEMP(I,J,IM),&
				! AVSMC(I,J,IM), SP(I,J,IM), PET(I,J,IM), AET(I,J,IM),PAET(I,J,IM), TRUNOFF(I,J,IM), &
				! BASEFLOW(I,J,IM),STRFLOW(I,J,IM)

2025      		FORMAT (I10, ',',I6, ',', I6, ',', F10.1, ',', F10.1,',',&  
                    F10.1, ',', F10.1,',', F8.1, ',', F8.1, ',',F8.1,',',F10.1,',',&
                    F10.1,',', F10.1)         


!------PRINT MONTHLY SOIL STORAGE DATA TO SOILSTORAGE.TXT

			   ! WRITE(900,2035)  HUCNO(I), IDY, IM, AVUZTWC(I,J,IM), AVUZFWC(I,J,IM),&
					! AVLZTWC(I,J,IM), AVLZFPC(I,J,IM), AVLZFSC(I,J,IM)
        
! TEST OUTPUT        
!			    WRITE(*,2035)  HUCNO(I), IDY, IM, AVUZTWC(I,J,IM), AVUZFWC(I,J,IM),&
!				   AVLZTWC(I,J,IM), AVLZFPC(I,J,IM)  
				  
2035    		FORMAT(I10, ',', I6, ',',I6, ',',F8.1, ',', F8.1,',' F8.1, ',',F8.1, ',',F8.1)

!------SUM THE TOTAL RAIN, PET, AET, DISCHARGE, INT, SNOWP FOR YEAR



			    IF (RAIN(I,J,IM) < -50.0 .or. TEMP(I,J,IM) < -50.0) then
					
					
					 TANURAIN = -999.0
					 TANUPET = -999.0
					 TANUAET = -999.0
					 TANUPAET= -999.0
					 TANURUN = -999.0
					 TSNOWP  = -999.0

					 GOTO 22200


			    ELSE
					 TANURAIN = TANURAIN + RAIN(I,J,IM)
					 TANUPET = TANUPET + PET(I,J,IM)
					 TANUAET = TANUAET + AET(I,J,IM)
					 TANUPAET= TANUPAET + PAET(I,J,IM)
					 TANURUN = TANURUN + (RUNOFF(I,J,IM)+PRIBF(I,J,IM)+SECBF(I,J,IM)+INTF(I,J,IM))
					 TSNOWP    = TSNOWP + SP(I,J,IM)

			    ENDIF       



			    IF (SP(I,J,IM) .GT. 0.) THEN 
				 
					ISNOWP = ISNOWP + 1    
					   
			    ENDIF
            
         
			    RAINSQ= RAINSQ + RAIN(I,J,IM)**2
                                                    
100         CONTINUE
         
!------ASSIGN TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP
! -----TANURUN 为流域的总出流量(RUNOFF(I,J,IM)+PRIBF(I,J,IM)+SECBF(I,J,IM)+INTF(I,J,IM))
         
22200       ANURAIN(I,J) = TANURAIN
		    ANUPET(I,J) = TANUPET
		    ANUAET(I,J) = TANUAET
		    ANUPAET(I,J)=TANUPAET
		    ANURUN(I,J) = TANURUN
      
		    IF (TANURAIN .GE. 1.0) THEN        
				ARUNRT(I,J) = TANURUN/TANURAIN
				AETRT(I,J)  = TANUAET/TANURAIN
			
		    ELSE
		  
			   ARUNRT (I,J) = 0.0
			   AETRT (I,J) = 0.0
		  
		    ENDIF 
      
      
			  ETRATIO(I,J) = AETRT(I,J) + ARUNRT(I,J)
			  NSPM(I,J) = ISNOWP 
				 
			  HUCAET(I,J) = ANUAET(I,J)
			  HUCPET(I,J) = ANUPET(I,J)     
			  HUCPAET(I,J)=ANUPAET(I,J)
!------PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT

                               

! ---- CALCULATING R FACTOR

       
		    IF (TANURAIN .GE. 0.) THEN
			 
			  F = RAINSQ/TANURAIN
		   
		  
			   IF (F .LT. 55) THEN
				   
				
			 RFACTOR(I,J) = (0.07397 * F**1.847 )/17.2
		   
			   ELSE
		   
			   RFACTOR(I,J) = (95.77-6.081*F+0.4770*F**2) /17.2
				   
			   ENDIF
				
		    ELSE
		   
			 RFACTOR(I,J) =0.
		   
		    ENDIF  


		    ! WRITE (79,2100) HUCNO(I), IDY, ANURAIN(I,J),&
			 ! ANUPET(I,J), ANUAET(I,J),ANUPAET(I,J), ANURUN(I,J), ARUNRT(I,J),  &
			 ! AETRT(I,J),ETRATIO(I,J), NSPM(I,J), RFACTOR (I,J)

! TEST OUTPUT
!            WRITE (*,2100) HUCNO(I), IDY, ANURAIN(J),&
!			 ANUPET(J), ANUAET(J),ANUPAET(J), ANURUN(J), ARUNRT(J),  &
!			 AETRT(J),ETRATIO, NSPM(I,J), RFACTOR (J)



2100        FORMAT(I10, ',', I10, ',',F10.1, ',', F8.1, ',', F8.1,',' F8.1, &
',',F8.1, ',', F8.2, ',',F8.2,',',F8.2,',', I8, ',', F8.1)


        ENDIF 
              
      RETURN
      END
      

!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE SUMMARY ***                                       C
!C     CALCULATE AVERAGE WATER BALANCE COMPONENTS FROM IYSTART TO IYEND C
!C     WRITE TO SUMMARRUNOFF.TXT by WATERSHED                                       C
!C                                                                      C
!C                                                                      C
!C**********************************************************************C
     
      SUBROUTINE SUMMARY_YEAR(I)
	  
	  USE Common_var
      implicit none 

!-----------------------------------------------------------------------      
        
      INTEGER I,J,ISTEP
            
      RAINALL(I) =0.
      AETALL(I) =0.   
      PETALL(I)=0.
      RUNALL(I)=0.
      RALL(I) = 0.
      
      NUM_year(I)=0
      
      ISTEP = IYEND - IYSTART + 1
          
      DO 100 J = 1, ISTEP
!------ 排除异常的气候值，异常值，气候和温度设置为-99999
       
       IF  (ANURAIN(I,J+IYSTART-BYEAR) < -50.0 .or. &
      ANUAET(I,J+IYSTART-BYEAR) < -50.0 .or. ANUPET(I,J+IYSTART-BYEAR) &
        < -50.0 .or. ANURUN(I,J+IYSTART-BYEAR) < -50.0 ) then 
      
      ELSE
            RAINALL(I) = RAINALL(I) + ANURAIN(I,J+IYSTART-BYEAR)    
            AETALL(I) = AETALL(I) + ANUAET(I,J+IYSTART-BYEAR)  
            PETALL(I) = PETALL(I) + ANUPET(I,J+IYSTART-BYEAR)
            RUNALL(I) = RUNALL(I) + ANURUN(I,J+IYSTART-BYEAR)
            
            RALL(I) = RALL(I) + RFACTOR(I,J+IYSTART-BYEAR)
                 
            NUM_year(I) = NUM_year(I) + 1
      ENDIF
!------ ---------------------------------------------------------------------


100   CONTINUE
     
      RAINALL(I) = RAINALL(I) /NUM_year(I)  
      AETALL(I) = AETALL(I) / NUM_year(I)
      PETALL(I) = PETALL(I) / NUM_year(I)
      RUNALL(I) = RUNALL(I) / NUM_year(I)
      
      RALL(I) =RALL(I) /NUM_year(I)


      IF (RAINALL(I) .GE. 0.) THEN  
      RUNRATIO(I) = RUNALL(I)/RAINALL(I)
      ETRATIO_GRD(I) = AETALL(I)/RAINALL(I)   
      ELSE
      
      RUNRATIO(I) = 0.0
      ETRATIO_GRD(I) = 0.0
      
      
      ENDIF

      TRATIO(I) = RUNRATIO(I) + ETRATIO_GRD(I)
   
!--WRITE TO FILE SUMMARRUNOFF.TXT

         ! WRITE (80,250) HUCNO(I), RAINALL(I), PETALL(I), AETALL(I), RUNALL(I), &
         ! RUNRATIO(I), ETRATIO_GRD(I), TRATIO(I), NUM_year(I)

     
250      FORMAT (I10, ',', F10.1, ',', F10.1,',',  &
               F10.1, ',', F10.1,',', F8.3, ',', F8.3,',', F8.3,&
          ',', I3)    
    
         RUNRATIO(I) = RUNRATIO(I) *100.
          
!         WRITE(*,300) HUCNO(I), RAINALL(I), RUNALL(I), RUNRATIO(I)
         
    
300    FORMAT ('GRID=',I5,' PRECIP (MM)=',F6.0, '   RUNOFF(MM)=', F5.0, &
         '  RUNOFF/PRECIP=', F4.1, '%')    

      RETURN
      END

	

!**********************************************************************C
!                                                                      C
!     *** SUBROUTINE Cabon balance ***                                 C
!     Simulate GEP AND NEE for selected HUC                            C
!     WRITE GEP AND NEE TO MONTHCARBON.TXT, ANNUALCARBON.TXT, HUCCARBON.TXT        C
!     SIMULATE BIODIVERSITY FOR SELECTED HUC                           C
!     WRITE BIODIVERSITY TO HUCBIO.TXT                                 C
!                                                                      C
!**********************************************************************C
!        I=HUC; J= YEAR, M =MONTH, MNDAY= NO OF DAYS IN A MONTH

      SUBROUTINE SUMMARY_CABON(I)
      
		USE Common_var
	    Implicit none 
! --------------------------------------------------------------
      INTEGER I,J,M,IDY
           
! ---------------------------------------------------------------      
      
      REAL ANGEP, ANRECO, ANNEE
      REAL HUCGEP, HUCNEE, HUCRE

!----------------------------------------------------------------      
             
         HUCGEP =0.
         HUCNEE =0.
         HUCRE  = 0.

         NUM_YEAR_C(I)=0

                          
         DO 200 J=1, NYEAR
   
            IDY = J + BYEAR - 1 
                  
            IF (IDY .GE. IYSTART .AND. IDY .LE. IYEND) THEN        
         
            
            ANGEP = 0.
            ANRECO = 0.
            ANNEE = 0.
                    
            DO 100 M=1, 12
                 
       
!--- ACCUMULATE ANNUAL GEP FORM MONTHLY VALUES (g C/m2/mon)
			  IF (RAIN(I,J,M) < -50.0 .or. TEMP(I,J,M) < -50.0) then

					ANGEP = -999.0
					ANRECO = -999.0
					ANNEE = -999.0
				   

			  ELSE

					ANGEP = ANGEP + GEPM(I,J,M)  
					ANRECO = ANRECO + RECOM(I,J,M) 
					ANNEE = ANNEE + NEEM(I,J,M)     
							 
			  ENDIF 
		  			
100         CONTINUE 

			ENDIF
  
			IF (ANGEP < -50.0 .or. ANRECO < -50.0 .or. ANRECO < -50.0 ) then

			 
			ELSE
            HUCGEP = HUCGEP + ANGEP
              
            HUCRE = HUCRE + ANRECO
              
            HUCNEE = HUCNEE + ANNEE
            
            NUM_YEAR_C(I) = NUM_YEAR_C(I) + 1
			ENDIF

         
200      CONTINUE

         AHUCGEP(I) = HUCGEP/NUM_YEAR_C(I)
         
         AHUCRE(I) = HUCRE/NUM_YEAR_C(I)
         
         AHUCNEE(I) = HUCNEE/NUM_YEAR_C(I)
            
      

            ! WRITE (600, 4000) HUCNO(I),NUM_YEAR_C(I), AHUCGEP(I), AHUCRE(I), AHUCNEE(I)
                                              
! 4000        FORMAT (I12, ',', I12, ',',F14.2, ',', F14.2, ',', F14.2)       



!            WRITE (*, 4100) HUCNO(I),AHUCGEP(I),AHUCRE(I), AHUCNEE(I)
                                              
4100        FORMAT ('CELL=',I12, '  GEP(gC/m2/yr.)=', F10.0, &
            '  Reco=', F10.0,' NEE(gC/m2/yr.)=', F10.0)       

 



      RETURN
      END

	
	  
