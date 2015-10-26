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
	
				IF (RAIN(I,J,IM)== NaN .or. RAIN(I,J,IM)==Infinity) then RAIN(I,J,IM)=0
				IF (SP(I,J,IM)== NaN .or. SP(I,J,IM)==Infinity) then SP(I,J,IM)=0
				IF (PET(I,J,IM)== NaN .or. PET(I,J,IM)==Infinity) then PET(I,J,IM)=0
				IF (AET(I,J,IM)== NaN .or. AET(I,J,IM)==Infinity) then AET(I,J,IM)=0
				IF (PAET(I,J,IM)== NaN .or. PAET(I,J,IM)==Infinity) then PAET(I,J,IM)=0
				IF (RUNOFF(I,J,IM)== NaN .or. RUNOFF(I,J,IM)==Infinity) then RUNOFF(I,J,IM)=0
				IF (PRIBF(I,J,IM)== NaN .or. PRIBF(I,J,IM)==Infinity) then PRIBF(I,J,IM)=0
				IF (SECBF(I,J,IM)== NaN .or. SECBF(I,J,IM)==Infinity) then SECBF(I,J,IM)=0
				IF (INTF(I,J,IM)== NaN .or. INTF(I,J,IM)==Infinity) then INTF(I,J,IM)=0
				IF (AVSMC(I,J,IM)== NaN .or. AVSMC(I,J,IM)==Infinity) then AVSMC(I,J,IM)=0
				IF (EMUZTWC(I,J,IM)== NaN .or. EMUZTWC(I,J,IM)==Infinity) then EMUZTWC(I,J,IM)=0
				IF (EMUZFWC(I,J,IM)== NaN .or. EMUZFWC(I,J,IM)==Infinity) then EMUZFWC(I,J,IM)=0
				IF (EMLZTWC(I,J,IM)== NaN .or. EMLZTWC(I,J,IM)==Infinity) then EMLZTWC(I,J,IM)=0
				IF (EMLZFPC(I,J,IM)== NaN .or. EMLZFPC(I,J,IM)==Infinity) then EMLZFPC(I,J,IM)=0
				IF (EMLZFSC(I,J,IM)== NaN .or. EMLZFSC(I,J,IM)==Infinity) then EMLZFSC(I,J,IM)=0
! For Water ballance output and Check VALIDATION.TXT
				WRITE(2003,2325) I,IDY,IM,RAIN(I,J,IM),SP(I,J,IM), PET(I,J,IM), &
				AET(I,J,IM),PAET(I,J,IM),RUNOFF(I,J,IM),PRIBF(I,J,IM),SECBF(I,J,IM),&
				INTF(I,J,IM),AVSMC(I,J,IM),EMUZTWC(I,J,IM), EMUZFWC(I,J,IM),&
					EMLZTWC(I,J,IM), EMLZFPC(I,J,IM), EMLZFSC(I,J,IM)
					
2325            FORMAT (I10, I6, I4,15F10.1)  
      
!------PRINT MONTHLY WATER BALANCE DATA TO MONTHFLOW.TXT				   
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

				!MONTHCARBON.TXT
				WRITE (400, 2000) HUCNO(I), IDY, IM, GEPM(I,J, IM), &
                RECOM(I,J,IM), NEEM(I,J,IM)         
     
2000          	FORMAT (I10, ',',I6, ',', I6, ',', F10.2, ',',  &
				F10.2,',',F10.2)
				
							
			END DO ! END LOOP MONTH
			
		ENDIF 
		
		END DO ! END LOOP YEAR
	
	
	!PRINT ANNUAL WATER BALANCE COMPONENTS TO HUCFLOW.TXT
		IF (RAINALL(I,J,IM)== NaN .or. RAINALL(I,J,IM)==Infinity) then RAINALL(I,J,IM)=0
		IF (PETALL(I,J,IM)== NaN .or. PETALL(I,J,IM)==Infinity) then PETALL(I,J,IM)=0
		IF (AETALL(I,J,IM)== NaN .or. AETALL(I,J,IM)==Infinity) then AETALL(I,J,IM)=0
		IF (RUNALL(I,J,IM)== NaN .or. RUNALL(I,J,IM)==Infinity) then RUNALL(I,J,IM)=0
		IF (RUNRATIO(I,J,IM)== NaN .or. RUNRATIO(I,J,IM)==Infinity) then RUNRATIO(I,J,IM)=0
		IF (ETRATIO_GRD(I,J,IM)== NaN .or. ETRATIO_GRD(I,J,IM)==Infinity) then ETRATIO_GRD(I,J,IM)=0
		IF (TRATIO(I,J,IM)== NaN .or. TRATIO(I,J,IM)==Infinity) then TRATIO(I,J,IM)=0
		IF (NUM_year(I,J,IM)== NaN .or. NUM_year(I,J,IM)==Infinity) then NUM_year(I,J,IM)=0
	
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