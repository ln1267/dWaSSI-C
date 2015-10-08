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
      SUBROUTINE OUTPUT
	  
	  
	USE Common_var
	IMPLICiT NONE 
! -----------------------------------------------------------------------------     
	 INTEGER I,J,IM,IDY

     DO I= 1, NGRID
	 
		DO J=1, NYEAR

	 
!-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
         
		IDY = J + BYEAR - 1
    
  
        IF (IDY .GE. IYSTART .AND. IDY .LE. IYEND) THEN        

         
		    DO  IM = 1, 12     
	
!------PRINT MONTHLY WATER BALANCE DATA TO MONTHFLOW.TXT

! For Water ballance output and Check
				WRITE(2003,2325) I,IDY,IM,RAIN(I,J,IM),SP(I,J,IM), PET(I,J,IM), &
				AET(I,J,IM),PAET(I,J,IM),RUNOFF(I,J,IM),PRIBF(I,J,IM),SECBF(I,J,IM),&
				INTF(I,J,IM),AVSMC(I,J,IM),EMUZTWC(I,J,IM), EMUZFWC(I,J,IM),&
					EMLZTWC(I,J,IM), EMLZFPC(I,J,IM), EMLZFSC(I,J,IM)
					
2325            FORMAT (I10, I6, I4,15F10.1)        
				   
			    WRITE(78,2025) I,IDY, IM, RAIN(I,J,IM),TEMP(I,J,IM),&
				AVSMC(I,J,IM), SP(I,J,IM), PET(I,J,IM), AET(I,J,IM),PAET(I,J,IM), TRUNOFF(I,J,IM), &
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
  
!------PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT

				WRITE (79,2100) HUCNO(I), IDY, ANURAIN(I,J),&
				ANUPET(I,J), ANUAET(I,J),ANUPAET(I,J), ANURUN(I,J), ARUNRT(I,J),  &
				AETRT(I,J),ETRATIO(I,J), NSPM(I,J), RFACTOR (I,J)

! TEST OUTPUT
!            WRITE (*,2100) HUCNO(I), IDY, ANURAIN(J),&
!			 ANUPET(J), ANUAET(J),ANUPAET(J), ANURUN(J), ARUNRT(J),  &
!			 AETRT(J),ETRATIO, NSPM(I,J), RFACTOR (J)

2100        	FORMAT(I10, ',', I10, ',',F10.1, ',', F8.1, ',', F8.1,',' F8.1, &
				',',F8.1, ',', F8.2, ',',F8.2,',',F8.2,',', I8, ',', F8.1)

				
				WRITE (400, 2000) HUCNO(I), IDY, IM, GEPM(I,J, IM), &
                RECOM(I,J,IM), NEEM(I,J,IM)         
     
2000          	FORMAT (I10, ',',I6, ',', I6, ',', F10.2, ',',  &
				F10.2,',',F10.2)
				
							
			END DO ! END LOOP MONTH
			
		ENDIF 
		
		END DO ! END LOOP YEAR
	
	WRITE (80,250) HUCNO(I), RAINALL(I), PETALL(I), AETALL(I), RUNALL(I), &
         RUNRATIO(I), ETRATIO_GRD(I), TRATIO(I), NUM_year(I)
		
250 FORMAT (I10, ',', F10.1, ',', F10.1,',',  &
    F10.1, ',', F10.1,',', F8.3, ',', F8.3,',', F8.3,&
    ',', I3)    	 
	
	WRITE (600, 4000) HUCNO(I),NUM_YEAR_C(I), AHUCGEP(I), AHUCRE(I), AHUCNEE(I)
                                              
4000        FORMAT (I12, ',', I12, ',',F14.2, ',', F14.2, ',', F14.2)       
                                         
	
	END DO  ! END LOOP GRID	 
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  CONTINUE
	  
	  END