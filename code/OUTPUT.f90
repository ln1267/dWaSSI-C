!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE OUTPUT ***                                        C
!C     WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT            C
!C     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT             C
!C     CALCULATE TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP     C
!C     PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT          C
!C                                                                      C
!C                                                                      C
!C**********************************************************************C
      SUBROUTINE OUTPUT (I, J)
      
	     USE Common_var
         implicit none 
! -----------------------------------------------------------------------------     
      INTEGER I,J,IM
      REAL TANURAIN, TANUPET,TANUAET,TANUPAET,TANURUN
      REAL ISM,TSP,TDM,TRUNOFF

!      REAL MWASSI(12),DEMAND(12),SUPPLY(12),
!     >  ANUDM(MAX_YEARS), ANUSP(MAX_YEARS)

!      REAL TDM, TSP
      
      REAL TSNOWP
     
      INTEGER IDY, ISNOWP
      REAL TRUNOFFS
      
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
				TRUNOFF = RUNOFF(I,J,IM) + PRIBF(I,J,IM) + SECBF(I,J,IM) + INTF(I,J,IM)
				BASEFLOW(I,J,IM)=PRIBF(I,J,IM) + SECBF(I,J,IM)
!------PRINT MONTHLY WATER BALANCE DATA TO MONTHFLOW.TXT

! For Water ballance output and Check
				WRITE(2003,2325) I,IDY,IM,RAIN(I,J,IM),SP(I,J,IM), PET(I,J,IM), &
				AET(I,J,IM),PAET(I,J,IM),RUNOFF(I,J,IM),PRIBF(I,J,IM),SECBF(I,J,IM),&
				INTF(I,J,IM),AVSMC(I,J,IM),EMUZTWC(I,J,IM), EMUZFWC(I,J,IM),&
					EMLZTWC(I,J,IM), EMLZFPC(I,J,IM), EMLZFSC(I,J,IM)
					
2325            FORMAT (I10, I6, I4,15F10.1)        
				   
			    WRITE(78,2025) I,IDY, IM, RAIN(I,J,IM),TEMP(I,J,IM),&
				AVSMC(I,J,IM), SP(I,J,IM), PET(I,J,IM), AET(I,J,IM),PAET(I,J,IM), TRUNOFF, &
				BASEFLOW(I,J,IM),STRFLOW(I,J,IM)

2025      		FORMAT (I10, ',',I6, ',', I6, ',', F10.1, ',', F10.1,',',&  
                    F10.1, ',', F10.1,',', F8.1, ',', F8.1, ',',F8.1,',',F10.1,',',&
                    F10.1,',', F10.1)         


!------PRINT MONTHLY SOIL STORAGE DATA TO SOILSTORAGE.TXT

			   WRITE(900,2035)  HUCNO(I), IDY, IM, AVUZTWC(I,J,IM), AVUZFWC(I,J,IM),&
					AVLZTWC(I,J,IM), AVLZFPC(I,J,IM), AVLZFSC(I,J,IM)
        
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


		    WRITE (79,2100) HUCNO(I), IDY, ANURAIN(I,J),&
			 ANUPET(I,J), ANUAET(I,J),ANUPAET(I,J), ANURUN(I,J), ARUNRT(I,J),  &
			 AETRT(I,J),ETRATIO(I,J), NSPM(I,J), RFACTOR (I,J)

! TEST OUTPUT
!            WRITE (*,2100) HUCNO(I), IDY, ANURAIN(J),&
!			 ANUPET(J), ANUAET(J),ANUPAET(J), ANURUN(J), ARUNRT(J),  &
!			 AETRT(J),ETRATIO, NSPM(I,J), RFACTOR (J)



2100        FORMAT(I10, ',', I10, ',',F10.1, ',', F8.1, ',', F8.1,',' F8.1, &
',',F8.1, ',', F8.2, ',',F8.2,',',F8.2,',', I8, ',', F8.1)


        ENDIF 
        
              
      RETURN
      END
      

