
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

         WRITE (80,250) HUCNO(I), RAINALL(I), PETALL(I), AETALL(I), RUNALL(I), &
         RUNRATIO(I), ETRATIO_GRD(I), TRATIO(I), NUM_year(I)

     
250      FORMAT (I10, ',', F10.1, ',', F10.1,',',  &
               F10.1, ',', F10.1,',', F8.3, ',', F8.3,',', F8.3,&
          ',', I3)    
    
         RUNRATIO(I) = RUNRATIO(I) *100.
          
!         WRITE(*,300) HUCNO(I), RAINALL(I), RUNALL(I), RUNRATIO(I)
         
    
300    FORMAT ('GRID=',I5,' PRECIP (MM)=',F6.0, '   RUNOFF(MM)=', F5.0, &
         '  RUNOFF/PRECIP=', F4.1, '%')    

      RETURN
      END
