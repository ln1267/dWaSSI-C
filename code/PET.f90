!**********************************************************************C
!                                                                      C
!     *** SUBROUTINE WARMPET ***                                       C
!     INPUT  MONTHLY PRECIPITATION AND TEMPERATURE, AND CALCULATE      C
!     MONTHLY PET AND POTENTIAL AET                                    C
!                                                                      C
!**********************************************************************C
      SUBROUTINE WARMPET(I, J_S, M, MNDAY)
      
      USE common_var
      implicit none
               
      INTEGER I,J,M,MNDAY,K,J_S  
                 
      INTEGER MONTHD(12),MONTHL(12), MJD(12), MMD
      
      REAL PE,DEGLAT,DTEMP,AETtemp,TPAET
             
! --- Number of days for each month during regular year
      DATA MONTHD/31,28,31,30,31,30,31,31,30,31,30,31/
! 
! --- Number of days for each month during leap year
      DATA MONTHL/31,29,31,30,31,30,31,31,30,31,30,31/
!-----JULIAN DAY FOR MID-DAY OF EACH MONTH
      DATA MJD/15,46,76,107,137,168,198,229,259,290,321,351/      

!----Set the simulate ID for the start year
			J=J_S+IYSTART-1-NWARMUP
			!print*,J
! --- Calculate Monthly potential evapotranspiration
                         
            DTEMP=TEMP(I,J,M)
! -- MMD = JULIAN DATE FOR MONTH M
                               
            MMD=MJD(M)
                     
            DEGLAT = LATUDE(I)   
! - DTEMP = AIR TEMP, HPEC = CORRECTION PARAMETER, PE =CALCUALTED PET (MM)--DAY
           
            CALL HAMON(DTEMP,M,MMD,DEGLAT,PE)	
			
! ---- convert daily PET to monthly
			PET(I,J,M) = PE * MNDAY 		
			
	IF (modelscale == 0) THEN !catchment scale
		
		TPAET=0
	!PET for each LANDUSE 		
		DO 40 K=1, NLC                   
	
	! Latest model By Yuan Fang Sep 10,2015
	!          R2=0.68, p<0.0001,RMSE=18.1 mm

			AET_lc(J,M,K) = -4.79 + 0.75*PET(I,J,M) + 3.92*LAI_lc(I,J,M,K)

	!C ----CALCULATE TOTAL PET AND PAET FOR THE HUC FOR A GIVEN YEAR AND MONTH
				 
			TPAET = TPAET + AET_lc(J,M,K) * LADUSE_lc(I,K)
				 	 
	   40      CONTINUE                   
				  
	! ------APAET =AVERAGE PAET FOR CURRENT CELL, FOR ALL YEAR, MONTH

				  PAET(I,J,M) = TPAET
			if (I.eq.1 .and. J.lt.IYSTART+2) then	  
			Print*,I,J,M,TPAET
			endif

	ELSE
           
! Latest model By Yuan Fang Sep 10,2015
!          R2=0.68, p<0.0001,RMSE=18.1 mm

            PAET(I,J,M) = -4.79 + 0.75*PET(I,J,M) + 3.92*LAI(I,J,M)
   
! ------TEST OUTPUT


!       WRITE(*, 5050) HUCNO(I),J,M, PET(I,J,M), PAET(I,J,M) 
!      
!5050  FORMAT (3I10, 2F10.5)

    ENDIF                          
     
	RETURN
	
    END


!**********************************************************************C
!                                                                      C
!     *** SUBROUTINE HAMON ***                                         C
!     Calculate potential evapotranspiration by using Hamon's          C
!     equation                                                         C
!                                                                      C
!**********************************************************************C
      SUBROUTINE HAMON(TEMP,MONTH,J,DEGLAT,PE)
       
      REAL PE, TEMP, DEGLAT
      
      REAL SOLDEC, SSANG, DAY
      
      REAL ESAT, RHOSAT, PI
      
      INTEGER J 
      
      PI = 3.14159265

! --- ESAT = saturated vapor pressure at TEMP
! --- RHOSAT = saturated vapor density at TEMP
      
      ESAT = 6.108*EXP(17.2693882*TEMP/(TEMP+237.3))
      RHOSAT = 216.7*ESAT/(TEMP+273.3)
      
! --- CALCULATE MEAN MONTHLY DAY LENGTH (DAY) BASED ON LATITUDE AND MID-MONTH JULIAN DATE
! --- SHUTTLEWORTH, W.J. 1993. Evaporation, in Handbook of Hydrology, 
! --- MAIDMENT, D.R. ED., MCGRAW HILL, NY, PP. 4, 1-53

! ---    CALCULATE THE ANGLE OF SOLAR DECLINATION IN RADIANS     
 
         SOLDEC = 0.4093*SIN(2*PI*J/365.-1.405)
      
! ---    CALCULATE THE SUNSET HOUR ANGLE IN RADIANS

         SSANG = ACOS(-1*TAN(DEGLAT*.0174529)*TAN(SOLDEC))
      
! ---    CALCULATE THE ADJUSTMENT IN LENGTH OF DAY FOR 12 HR PERIOD
      
         DAY = 2*SSANG/PI
         
         
! --- Calculate Daily PE
     
      PE = 0.1651*DAY*RHOSAT*1.2
      
!      print *, 'PET', MONTH, J, TEMP, PE, K
          
      RETURN

      END