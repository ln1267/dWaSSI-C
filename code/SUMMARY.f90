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
      SUBROUTINE SUMMARY_MONTH (I, J_S)
      
	     USE Common_var
         implicit none 
! -----------------------------------------------------------------------------     
      INTEGER I,J,IM,J_S
      REAL TANURAIN, TANUPET,TANUAET,TANUPAET,TANURUN
      REAL ISM,TSP,TDM

      REAL TSNOWP
     
      INTEGER IDY, ISNOWP
            
      REAL RAINSQ, F

!----Set the simulate ID for the start year
	  J=J_S+IYSTART-1-NWARMUP
			!print*,J     
!-----IDY = THE ID of the start YEAR, BYEAR = YEAR TO SATRT
         
      IDY = J
         
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
      
          
      DO 100 J = IYSTART, IYEND

       
       IF  (ANURAIN(I,J) < -50.0 .or. &
      ANUAET(I,J) < -50.0 .or. ANUPET(I,J) &
        < -50.0 .or. ANURUN(I,J) < -50.0 ) then 
      
      ELSE
            RAINALL(I) = RAINALL(I) + ANURAIN(I,J)    
            AETALL(I) = AETALL(I) + ANUAET(I,J)  
            PETALL(I) = PETALL(I) + ANUPET(I,J)
            RUNALL(I) = RUNALL(I) + ANURUN(I,J)
            
            RALL(I) = RALL(I) + RFACTOR(I,J)
                 
            NUM_year(I) = NUM_year(I) + 1
      ENDIF
!------ ---------------------------------------------------------------------


100   CONTINUE
!print*,I,NUM_year(I) ,NYEAR_S
     
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
   
      RUNRATIO(I) = RUNRATIO(I) *100.

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
      
      REAL HUCGEP, HUCNEE, HUCRE

!----------------------------------------------------------------      

		 
         HUCGEP =0.
         HUCNEE =0.
         HUCRE  = 0.

         NUM_YEAR_C(I)=0

                          
         DO 200 J=IYSTART, IYEND
   
            IDY = J 
			ANGEP(I,J) = 0.
            ANRECO(I,J) = 0.
            ANNEE(I,J) = 0.       
            IF (IDY .GE. IYSTART .AND. IDY .LE. IYEND) THEN        
                    
            DO 100 M=1, 12
                 
       
!--- ACCUMULATE ANNUAL GEP FORM MONTHLY VALUES (g C/m2/mon)
			  IF (RAIN(I,J,M) < -50.0 .or. TEMP(I,J,M) < -50.0) then

					ANGEP(I,J) = -999.0
					ANRECO(I,J) = -999.0
					ANNEE(I,J) = -999.0
				   goto 1001

			  ELSE

					ANGEP(I,J) = ANGEP(I,J) + GEPM(I,J,M)  
					ANRECO(I,J) = ANRECO(I,J) + RECOM(I,J,M) 
					ANNEE(I,J) = ANNEE(I,J) + NEEM(I,J,M)     
							 
			  ENDIF 
		  			
100         CONTINUE 

1001		ENDIF
  
			IF (ANGEP(I,J) < -50.0) then
			
			 
			ELSE
            HUCGEP = HUCGEP + ANGEP(I,J)
              
            HUCRE = HUCRE + ANRECO(I,J)
              
            HUCNEE = HUCNEE + ANNEE(I,J)
            
            NUM_YEAR_C(I) = NUM_YEAR_C(I) + 1
			ENDIF

         
200      CONTINUE

         AHUCGEP(I) = HUCGEP/NUM_YEAR_C(I)
         
         AHUCRE(I) = HUCRE/NUM_YEAR_C(I)
         
         AHUCNEE(I) = HUCNEE/NUM_YEAR_C(I)
            
      

             WRITE (*, 4000) HUCNO(I),NUM_YEAR_C(I), AHUCGEP(I), AHUCRE(I), AHUCNEE(I)
                                              
 4000        FORMAT (I12, ',', I12, ',',F14.2, ',', F14.2, ',', F14.2)       
   

      RETURN
      END

	
	  