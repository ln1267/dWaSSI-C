!**********************************************************************C
!                                                                      C
!     *** SUBROUTINE WARMPET ***                                       C
!     INPUT  MONTHLY PRECIPITATION AND TEMPERATURE, AND CALCULATE      C
!     MONTHLY PET AND POTENTIAL AET                                    C
!                                                                      C
!**********************************************************************C
      SUBROUTINE WARMPET(I, J, M, MNDAY)
      
      USE common_var
      implicit none
               
      INTEGER I,J,M,MNDAY  
                 
      INTEGER MONTHD(12),MONTHL(12), MJD(12), MMD
      
      REAL TPET,PE,DEGLAT,LAI_temp,DTEMP
             
! --- Number of days for each month during regular year
      DATA MONTHD/31,28,31,30,31,30,31,31,30,31,30,31/
! 
! --- Number of days for each month during leap year
      DATA MONTHL/31,29,31,30,31,30,31,31,30,31,30,31/
!-----JULIAN DAY FOR MID-DAY OF EACH MONTH
      DATA MJD/15,46,76,107,137,168,198,229,259,290,321,351/      

! --- Calculate Monthly potential evapotranspiration
                 
        
            DTEMP=TEMP(I,J,M)
            
            TPET=0. 
            
            TPAET=0. 
!			   PET=0.
                         
              
! -- MMD = JULIAN DATE FOR MONTH M
                               
            MMD=MJD(M)
                     
            DEGLAT = LATUDE(I)
            
! - DTEMP = AIR TEMP, HPEC = CORRECTION PARAMETER, PE =CALCUALTED PET (MM)--DAY
           
            CALL HAMON(DTEMP,M,MMD,DEGLAT,PE)
            
            
! ----PET (YEAR, MONTH, LANDUSE) FOR THE CURRENT CELL)
           
             PET(J,M) = PE  
                  
             PET(J,M) = PET(J,M) * MNDAY 
             
             
! ----ASSIGN LAI FOR EACH LAND USE
            
               

! ----CALCULATE PAET 
! ----PAET (YEAR, MONTH, LANDUSE) FOR THE CURRENT CELL)
! ----PAET IS THE POTENTIAL AET, ASSUMING SOIL MOISTURE NOT LIMITING

!             IF ((LAI.GE.1.0).OR.(AAPPT(I).GE.600.0)) THEN
             
!     CALCULATE PAET USING MODIFIED SUN HAMON EQUATION FOR HIGHER LAI
!     NO COWEETA
!     NO -LAI*RAIN TERM (MAKES PAET INVERSELY PROPORTIONAL TO LAI DURING WINTER
!     R2=0.87, N=90, MODEL ET= 9.37+0.87*MEASURED ET

!             PAET(J,M) = 9.95+(PET(J,M))*LAI*0.205+0.153*
!     &RAIN(I,J,M)+0.246*(PET(J,M))

!  new model with coweeta Hamon ET Sep 10,2010
! 对纬度>40的采用别的公式
!       if (deglat .gt. 40) then

!          paet(j,m) = 0.00169*pet(j,m)*lai(i,j,m)+ &
!             0.4*pet(j,m) + 7.78*lai(i,j,m)

!       else
!              paet(j,m) = 0.0222*pet(j,m)*lai(i,j,m)+0.174*  &
!             rain(i,j,m)+0.502*pet(j,m) + 5.31*lai(i,j,m)

!       endif
!           PRINT *, 'pet', LAI(I,J,M),RAIN(I,J,M), PET(J,M), PAET(J,M)
     
     
!             ELSE
             
!     CALCULATE PAET FOR GRASSLAND AND LOW LAI
!     R2=0.696
             
!             PAET(J,M) = 1.49 + 0.325*(PET(J,M))+0.353*(RAIN(I,J,M))
             
!             ENDIF
          
!             WRITE(910,5039)  HUCNO(I), J, M, K, PAET(J,M)
!5039         FORMAT(4I10, F20.1)


! Latest model By Yuan Fang Sep 10,2015
!          R2=0.68, p<0.0001,RMSE=18.1 mm

            PAET(J,M) = -4.79 + 0.75*PET(J,M) + 3.92*LAI(I,J,M)



! ----CALCULATE TOTAL PET AND PAET FOR THE HUC FOR A GIVEN YEAR AND MONTH
        
             
             TPET = TPET + PET(J,M)
             
             TPAET = TPAET + PAET(J,M)
             
             
        
!          WRITE(77, 5040) HUCNO(I), J, M, LADUSE(I), LAI          
       
!5040  FORMAT (3I10, 2F10.5)
     
 

!-------流域单元月APET、APAET（各植被类型的加权平均值）
! ------APET =AVERAGE PET FOR CURRENT CELL, FOR ALL YEAR, MONTH


              APET(M) = TPET    
              
! ------APAET =AVERAGE PAET FOR CURRENT CELL, FOR ALL YEAR, MONTH

              APAET(M) = TPAET
              

! ------TEST OUTPUT


!       WRITE(*, 5050) HUCNO(I),J,M, APET(M), APAET(M) 
!      
!5050  FORMAT (3I10, 2F10.5)

                              
! --- Return
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