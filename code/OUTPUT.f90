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
#ifdef MPI
SUBROUTINE OUTPUT

    USE Common_var
    use mpi
    implicit none
     ! -----------------------------------------------------------------------------
     INTEGER :: I
     INTEGER :: J
     INTEGER :: IM
     INTEGER :: IDY


    !MPI specific varaibles
    real*4, allocatable:: buffer(:)
    integer:: nelement,indx

!VALIDATION.TXT
    nelement = NGRID * NYEAR * 12 * validation_columns
    allocate(buffer(nelement))
    indx=1
    do I = 1,NGRID
        do J = 1,NYEAR
            !-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
        IDY = J + BYEAR - 1
        if (IDY .ge. IYSTART .and. IDY .le. IYEND) then
            do IM = 1,12
                buffer(indx)    =   real((my_grid_start - 1) + I)
                buffer(indx+1)  =   real(IDY)
                buffer(indx+2)  =   real(IM)
                buffer(indx+3)  =   RAIN(I,J,IM)
                buffer(indx+4)  =   SP(I,J,IM)
                buffer(indx+5)  =   PET(I,J,IM)
                buffer(indx+6)  =   AET(I,J,IM)
                buffer(indx+7)  =   PAET(I,J,IM)
                buffer(indx+8)  =   RUNOFF(I,J,IM)
                buffer(indx+9)  =   PRIBF(I,J,IM)
                buffer(indx+10) =   SECBF(I,J,IM)
                buffer(indx+11) =   INTF(I,J,IM)
                buffer(indx+12) =   AVSMC(I,J,IM)
                buffer(indx+13) =   EMUZTWC(I,J,IM)
                buffer(indx+14) =   EMUZFWC(I,J,IM)
                buffer(indx+15) =   EMLZTWC(I,J,IM)
                buffer(indx+16) =   EMLZFPC(I,J,IM)
                buffer(indx+17) =   EMLZFSC(I,J,IM)
                indx=indx+validation_columns
            enddo
        endif
        enddo
    enddo
    call writeData(validation_fh,nelement,buffer)


!MONTHFLOW.TXT
    nelement = NGRID * NYEAR * 12 * monthflow_columns

    if (allocated(buffer) .eqv. .true.) deallocate(buffer)
    allocate(buffer(nelement))

    indx=1
    do I = 1,NGRID
        do J = 1,NYEAR
            !-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
        IDY = J + BYEAR - 1
        if (IDY .ge. IYSTART .and. IDY .le. IYEND) then
            do IM = 1,12
                buffer(indx)    =   real(I)
                buffer(indx+1)  =   real(IDY)
                buffer(indx+2)  =   real(IM)
                buffer(indx+3)  =   RAIN(I,J,IM)
                buffer(indx+4)  =   TEMP(I,J,IM)
                buffer(indx+5)  =   AVSMC(I,J,IM)
                buffer(indx+6)  =   SP(I,J,IM)
                buffer(indx+7)  =   PET(I,J,IM)
                buffer(indx+8)  =   AET(I,J,IM)
                buffer(indx+9)  =   PAET(I,J,IM)
                buffer(indx+10) =   TRUNOFF(I,J,IM)
                buffer(indx+11) =   BASEFLOW(I,J,IM)
                buffer(indx+12) =   STRFLOW(I,J,IM)
                indx=indx+monthflow_columns
            enddo
        endif
        enddo
    enddo
    call writeData(monthflow_fh,nelement,buffer)


!SOILSTORAGE.TXT
    nelement = NGRID * NYEAR * 12 * soilstorage_columns

    if (allocated(buffer) .eqv. .true.) deallocate(buffer)
    allocate(buffer(nelement))

    indx=1
    do I = 1,NGRID
        do J = 1,NYEAR
            !-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
        IDY = J + BYEAR - 1
        if (IDY .ge. IYSTART .and. IDY .le. IYEND) then
            do IM = 1,12
                buffer(indx)    =   real(HUCNO(I))
                buffer(indx+1)  =   real(IDY)
                buffer(indx+2)  =   real(IM)
                buffer(indx+3)  =   AVUZTWC(I,J,IM)
                buffer(indx+4)  =   AVUZFWC(I,J,IM)
                buffer(indx+5)  =   AVLZTWC(I,J,IM)
                buffer(indx+6)  =   AVLZFPC(I,J,IM)
                buffer(indx+7)  =   AVLZFSC(I,J,IM)
                indx=indx+soilstorage_columns
            enddo
        endif
        enddo
    enddo
    call writeData(soilstorage_fh,nelement,buffer)


!MONTHCARBON.TXT
    nelement = NGRID * NYEAR * 12 * monthcarbon_columns

    if (allocated(buffer) .eqv. .true.) deallocate(buffer)
    allocate(buffer(nelement))

    indx=1
    do I = 1,NGRID
        do J = 1,NYEAR
            !-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
        IDY = J + BYEAR - 1
        if (IDY .ge. IYSTART .and. IDY .le. IYEND) then
            do IM = 1,12
                buffer(indx)    =   real(HUCNO(I))
                buffer(indx+1)  =   real(IDY)
                buffer(indx+2)  =   real(IM)
                buffer(indx+3)  =   GEPM(I,J, IM)
                buffer(indx+4)  =   RECOM(I,J,IM)
                buffer(indx+5)  =   NEEM(I,J,IM)
                indx=indx+monthcarbon_columns
            enddo
        endif
        enddo
    enddo
    call writeData(monthcarbon_fh,nelement,buffer)

!ANNUALFLOW.DAT
    nelement = NGRID * NYEAR * annualflow_columns
    if (allocated(buffer) .eqv. .true.) deallocate(buffer)
    allocate(buffer(nelement))
    indx=1
    do I = 1,NGRID
        do J = 1,NYEAR
            !-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
                IDY = J + BYEAR - 1
                buffer(indx)    =   real(HUCNO(I))
                buffer(indx+1)  =   real(IDY)
                buffer(indx+2)  =   ANURAIN(I,J)
                buffer(indx+3)  =   ANUPET(I,J)
                buffer(indx+4)  =   ANUAET(I,J)
                buffer(indx+5)  =   ANUPAET(I,J)
                buffer(indx+6)  =   ANURUN(I,J)
                buffer(indx+7)  =   ARUNRT(I,J)
                buffer(indx+8)  =   AETRT(I,J)
                buffer(indx+9)  =   ETRATIO(I,J)
                buffer(indx+10) =   real(NSPM(I,J))
                buffer(indx+11) =   RFACTOR (I,J)
                indx=indx+annualflow_columns
        enddo
    enddo
    call print_buffer_files(nelement,buffer,annualflow_columns)
    call writeData(annualflow_fh,nelement,buffer)

!ANNUALCARBON.TXT

   nelement = NGRID * NYEAR * annualcarbon_columns
    if (allocated(buffer) .eqv. .true.) deallocate(buffer)
    allocate(buffer(nelement))
    indx=1
    do I = 1,NGRID
        do J = 1,NYEAR
        !-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
                IDY = J + BYEAR - 1
                buffer(indx)    =   real(HUCNO(I))
                buffer(indx+1)  =   real(IDY)
                buffer(indx+2)  =   GEPA(I,J)
                buffer(indx+3)  =   REOA(I,J)
                buffer(indx+4)  =   NEEA(I,J)
                indx=indx+annualcarbon_columns
        enddo
    enddo
    call writeData(annualcarbon_fh,nelement,buffer)

   !HUCFLOW.TXT
   nelement = NGRID * hucflow_columns
    if (allocated(buffer) .eqv. .true.) deallocate(buffer)
    allocate(buffer(nelement))
    indx=1
    do I = 1,NGRID
                buffer(indx)    =   real(HUCNO(I))
                buffer(indx+1)  =   RAINALL(I)
                buffer(indx+2)  =   PETALL(I)
                buffer(indx+3)  =   AETALL(I)
                buffer(indx+4)  =   RUNALL(I)
                buffer(indx+5)  =   RUNRATIO(I)
                buffer(indx+6)  =   ETRATIO_GRD(I)
                buffer(indx+7)  =   TRATIO(I)
                buffer(indx+8)  =   real(NUM_year(I))
                indx=indx+hucflow_columns
    enddo
    call writeData(hucflow_fh,nelement,buffer)

    !HUCCARBON.TXT
    nelement = NGRID * huccarbon_columns
    if (allocated(buffer) .eqv. .true.) deallocate(buffer)
    allocate(buffer(nelement))
    indx=1
    do I = 1,NGRID
                buffer(indx)    =   real(HUCNO(I))
                buffer(indx+1)  =   real(NUM_YEAR_C(I))
                buffer(indx+2)  =   AHUCGEP(I)
                buffer(indx+3)  =   AHUCRE(I)
                buffer(indx+4)  =   AHUCNEE(I)
                indx=indx+huccarbon_columns
    enddo
    call writeData(huccarbon_fh,nelement,buffer)
END

#else

SUBROUTINE OUTPUT
	  
	  
	USE Common_var
	IMPLICIT NONE 
	 ! -----------------------------------------------------------------------------     
	 INTEGER :: I
	 INTEGER :: J
	 INTEGER :: IM
	 INTEGER :: IDY

	 write(*,*) " In serial output routine"

     DO I= 1, NGRID
	 
		DO J=1, NYEAR

	 
!-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
         
		IDY = J + BYEAR - 1
    
  
        IF (IDY .GE. IYSTART .AND. IDY .LE. IYEND) THEN        

         
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
     
2000          	FORMAT (I10, ',',I6, ',', I6, ',', F10.5, ',',  F10.5,',',F10.5)
				
							
			END DO ! END LOOP MONTH
			
	!ANNUALFLOW.TXT

				WRITE (79,2100) HUCNO(I), IDY, ANURAIN(I,J),&
				ANUPET(I,J), ANUAET(I,J),ANUPAET(I,J), ANURUN(I,J), ARUNRT(I,J),  &
				AETRT(I,J),ETRATIO(I,J), NSPM(I,J), RFACTOR (I,J)

! TEST OUTPUT
!            WRITE (*,2100) HUCNO(I), IDY, ANURAIN(I,J),&
!			 ANUPET(I,J), ANUAET(I,J),ANUPAET(I,J), ANURUN(I,J), ARUNRT(I,J),  &
!			 AETRT(I,J),ETRATIO(I,J), NSPM(I,J), RFACTOR (I,J)

2100        	FORMAT(I10, ',', I10, ',',F10.1, ',', F8.1, ',', F8.1,',' F8.1, &
		& ',',F8.1, ',', F8.2, ',',F8.2,',',F8.2,',', I8, ',', F8.1)			
		
	!ANNUALCARBON.TXT
				WRITE (500, 3000) HUCNO(I), IDY, GEPA(I,J),REOA(I,J),NEEA(I,J)
                                              
3000          FORMAT (I12, ',', I12, ',',F16.2, ',', F16.2, ',', F16.2)
		
		ENDIF 
				
		END DO ! END LOOP YEAR
		
	!HUCFLOW.TXT
		
		WRITE (80,250) HUCNO(I), RAINALL(I), PETALL(I), AETALL(I), RUNALL(I), &
         RUNRATIO(I), ETRATIO_GRD(I), TRATIO(I), NUM_year(I)
		
250 	FORMAT (I10, ',', F10.1, ',', F10.1,',',  &
		& F10.1, ',', F10.1,',', F8.3, ',', F8.3,',', F8.3,',', I3)    	 
	!HUCCARBON.TXT
		WRITE (600, 4000) HUCNO(I),NUM_YEAR_C(I), AHUCGEP(I), AHUCRE(I), AHUCNEE(I)
                                              
4000    FORMAT (I12, ',', I12, ',',F14.2, ',', F14.2, ',', F14.2)       
                                         
	
	END DO  ! END LOOP GRID	 
	CONTINUE
END
#endif
