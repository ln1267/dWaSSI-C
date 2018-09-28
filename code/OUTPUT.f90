!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE SUMMARY_MONTH ***                                        C
!C     WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT            C
!C     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT             C
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
	 
		DO J=IYSTART, IYEND

	 
!-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
         
		IDY = J + BYEAR - 1
    

        IF (IDY .GE. YSTART .AND. IDY .LE. YEND) THEN        

         
		    DO  IM = 1, 12     
	
				
	!VALIDATION.TXT
				WRITE(2003,2325) I,IDY,IM,RAIN(I,J,IM),SP(I,J,IM), PET(I,J,IM), &
				AET(I,J,IM),PAET(I,J,IM),RUNOFF(I,J,IM),PRIBF(I,J,IM),SECBF(I,J,IM),&
				INTF(I,J,IM),AVSMC(I,J,IM),EMUZTWC(I,J,IM), EMUZFWC(I,J,IM),&
					EMLZTWC(I,J,IM), EMLZFPC(I,J,IM), EMLZFSC(I,J,IM)
					
2325            FORMAT (I10, I6, I4,15F10.1)  
      
	!MONTHFLOW.TXT				   
			    WRITE(78,2025) I,IDY, IM, RAIN(I,J,IM),TEMP(I,J,IM),&
				AVSMC(I,J,IM), SP(I,J,IM), PET(I,J,IM), AET(I,J,IM),PAET(I,J,IM), TRUNOFF(I,J,IM), &
				BASEFLOW(I,J,IM),STRFLOW(I,J,IM)

2025      		FORMAT (I10, ',',I6, ',', I6, ',', F10.1, ',', F10.1,',',&  
                    F10.1, ',', F10.1,',', F8.1, ',', F8.1, ',',F8.1,',',F10.1,',',&
                    F10.1,',', F10.1)         

	!SOILSTORAGE.TXT

			   WRITE(900,2035)  HUCNO(I), IDY, IM, AVUZTWC(I,J,IM), AVUZFWC(I,J,IM),&
					AVLZTWC(I,J,IM), AVLZFPC(I,J,IM), AVLZFSC(I,J,IM)
        
		! TEST OUTPUT        
!			    WRITE(*,2035)  HUCNO(I), IDY, IM, AVUZTWC(I,J,IM), AVUZFWC(I,J,IM),&
!				   AVLZTWC(I,J,IM), AVLZFPC(I,J,IM)  
				  
2035    		FORMAT(I10, ',', I6, ',',I6, ',',F8.1, ',', F8.1,',' F8.1, ',',F8.1, ',',F8.1)
  
	!MONTHCARBON.TXT
				WRITE (400, 2000) HUCNO(I), IDY, IM, GEPM(I,J, IM), &
                RECOM(I,J,IM), NEEM(I,J,IM)         
				
2000          	FORMAT (I10, ',',I6, ',', I6, ',', 3F10.2)
				
							
			END DO ! END LOOP MONTH
			
	!ANNUALFLOW.TXT

				WRITE (79,2100) HUCNO(I), IDY, ANURAIN(I,J),&
				ANUPET(I,J), ANUAET(I,J),ANUPAET(I,J), ANURUN(I,J), ARUNRT(I,J),  &
				AETRT(I,J),ETRATIO(I,J), NSPM(I,J), RFACTOR (I,J)
				
2100        	FORMAT(I10, ',', I10, ',',F10.1, ',', F8.1, ',', F8.1,',' F8.1, &
				',',F8.1, ',', F8.2, ',',F8.2,',',F8.2,',', I8, ',', F8.1)
				IF (I==1) print*,I,IDY,ANUAET(I,J)
				WRITE (500, 20001) HUCNO(I), IDY, ANGEP(I,J),ANRECO(I,J), ANNEE(I,J),ANUAET(I,J),ANUPAET(I,J)       
20001          	FORMAT (I10, ',',I6, ',', 5F10.2)

		
		
		ENDIF 
				
		END DO ! END LOOP YEAR
		
	!HUCFLOW.TXT
		
		WRITE (80,250) HUCNO(I), RAINALL(I), PETALL(I), AETALL(I), RUNALL(I), &
         RUNRATIO(I), ETRATIO_GRD(I), TRATIO(I), NUM_year(I)
		
250 	FORMAT (I10, ',', F10.1, ',', F10.1,',',  &
		F10.1, ',', F10.1,',', F8.3, ',', F8.3,',', F8.3,&
		',', I3)    	 
	!HUCCARBON.TXT
		WRITE (600, 4000) HUCNO(I),NUM_YEAR_C(I), AHUCGEP(I), AHUCRE(I), AHUCNEE(I)
                                              
4000    FORMAT (I12, ',', I12, ',',F14.2, ',', F14.2, ',', F14.2)       
                                         
	
	END DO  ! END LOOP GRID	 
	   
	  
	  CONTINUE
	  
	  END