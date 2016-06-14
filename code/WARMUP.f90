#ifdef MPI
!**********************************************************************C
!                                                                      C
!     *** SUBROUTINE RPSDF_MPI ***                                     C
!     MPI implementation of RPSDF                                      !
!     Read basic input data from GENERAL.TXT and write to              !
!     BASICOUT.TXT, set up column headings in output files             C
!     Master reads in the GENERAL.TXT and then broadcasts the 8        !
!     element array of information to allparticipating MPI processes   !                                                                 !
!**********************************************************************!
      SUBROUTINE RPSDF

      USE Common_var
      USE mpi
      implicit none

      CHARACTER*4 HEADNG(20)
      INTEGER ISCENARIOS
      REAL SNOWPACK
      real,allocatable :: inputDATA(:)
      allocate(inputDATA(12))

!!!--------------------------------------------------------------------
! --- Read in data from GENERAL.TXT and write to BASICOUT.TXT


if (rank==0) then
write (*,*) "[",rank,"] --- Calling MPI routine"
      READ(general_fh,1000) HEADNG
 1000 FORMAT(20A4)
      WRITE(basicout_fh,2010) HEADNG
 2010 FORMAT(' ',20A4/)

      READ(general_fh,*) ISCENARIOS

      WRITE(basicout_fh,*) ISCENARIOS
2015  FORMAT('Scenario#', I10)


      READ(general_fh,*) NGRID, NYEAR, BYEAR, NLC

      WRITE(basicout_fh,2020) NGRID
 2020 FORMAT(I5,'ACTIVE GRIDS'/)

      WRITE(basicout_fh,2030) NYEAR, BYEAR

 2030 FORMAT(I10,'YEARS TO BE SIMULATED AND',' FIRST YEAR =', I10)

      WRITE(basicout_fh,2040) NLC

 2040 FORMAT('NUMBER OF LAND COVER CATEGORIES: ',I10)

      READ (general_fh,*) IYSTART, IYEND

      WRITE(basicout_fh,2050) IYSTART, IYEND
2050  FORMAT('FOR SIMULATION SUMMARY, YEAR TO START',I10, ' , END ',I10)

      READ (general_fh,*) LAI_S_Y,LAI_E_Y

      WRITE(basicout_fh,2051) LAI_S_Y,LAI_E_Y
2051  FORMAT('FOR LAI input data, the first year',I10, ' , END ',I10)

      READ (general_fh,*) FPERD

!--  reduction fraction of Leaf Area index for scenario analysis
      READ (general_fh, *) FPERDLAI

      WRITE(basicout_fh,2058) FPERD, FPERDLAI
2058  FORMAT('DEFOREST RATE%',F10.2, /'LAI REDUCTION%=', F10.2)

      READ (general_fh,*) SNOWPACK
      WRITE(basicout_fh,1047) SNOWPACK
1047  FORMAT('INTIAL SNOWPACK (MM) = :', F10.2)

! Populate the broadcast array on rank 0 or Master
       inputDATA(1)=NGRID
       inputDATA(2)=NYEAR
       inputDATA(3)=BYEAR
       inputDATA(4)=NLC
       inputDATA(5)=IYSTART
       inputDATA(6)=IYEND
       inputDATA(7)=FPERD
       inputDATA(8)=FPERDLAI
       inputDATA(9)=SNOWPACK
       inputDATA(10)=LAI_S_Y
       inputDATA(11)=LAI_E_Y
endif

! Broadcasting the data to all participating processors
call mpi_bcast(inputDATA, 11, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
! Now that we have broadcasted the data, lets copy the received array content to the corresponding local variables

       NGRID    =   inputDATA(1)
       NYEAR    =   inputDATA(2)
       BYEAR    =   inputDATA(3)
       NLC      =   inputDATA(4)
       IYSTART  =   inputDATA(5)
       IYEND    =   inputDATA(6)
       FPERD    =   inputDATA(7)
       FPERDLAI =   inputDATA(8)
       SNOWPACK =   inputDATA(9)
       LAI_S_Y  =   inputDATA(10) 
       LAI_E_Y  =   inputDATA(11)
      RETURN
END

!**********************************************************************!
!                                                                      !
!     *** SUBROUTINE RPSINT ***                                        !
!     Read in landuse data from CELLINFO.DAT, calcuate change in       !
!     landuse based on percent forest decrease (if desired), write     !
!     to BASICOUT.TXT                                                  !
!     �����趨�Ĳɷ�ֵ�ؼ����������������                             !
!**********************************************************************!
      SUBROUTINE RPSINT

      Use Common_var
      use mpi
      implicit none

      REAL CROP

    INTEGER(kind=2) year,J
    INTEGER(kind=4) I , ID


      CHARACTER*1000 DUMY(30)


    !MPI specific varaibles
      real*4, allocatable:: buffer(:)
      integer nelement,indx


! --- Read and print land use data for each active cell IN THE BASIC.OUT FILE
if (rank .eq. 0) then
      WRITE(basicout_fh,2000)
endif

2000  FORMAT(/'LANDUSE INFO FOR EACH SIMULATION CELL'/)

500   FORMAT (1000A30)


!! Open File CELLINFO.DAT in parallel and read corresponding chunk of data in a buffer and then update
!! corresponding arrays.
    nelement = NGRID * cellinfo_columns
    allocate(buffer(nelement))
    call readData(cellinfo_fh,nelement,buffer)

    ! Copying data from buffer into the relevant arrays.
    indx=1
        do I=1,nelement,cellinfo_columns
             ID            =   int(buffer(I))
             HUCNO(indx)   =   int(buffer(I))
             LATUDE(indx)  =   buffer(I+1)
             LONGI(indx)   =   buffer(I+2)
             LADUSE(indx)  =   buffer(I+3)
             HUCELE(indx)  =   buffer(I+4)
             indx=indx+1
        enddo


! --- Read and print SOIL PARAMETERS for each active cell IN THE BASIC.OUT FILE
!
    nelement= NGRID * soilinfo_columns
     if (allocated(buffer) .eqv. .true.) deallocate(buffer)
    allocate(buffer(nelement))
    call readData(soilinfo_fh,nelement,buffer)
    indx=1
    do I=1,nelement,soilinfo_columns
        ID             =    int(buffer(I))
        HUCNO(indx)    =    int(buffer(I))
        UZTWM(indx)    =    buffer(I+1)
        UZFWM(indx)    =    buffer(I+2)
        UZK(indx)      =    buffer(I+3)
        ZPERC(indx)    =    buffer(I+4)
        REXP(indx)     =    buffer(I+5)
        LZTWM(indx)    =    buffer(I+6)
        LZFSM(indx)    =    buffer(I+7)
        LZFPM(indx)    =    buffer(I+8)
        LZSK(indx)     =    buffer(I+9)
        LZPK(indx)     =    buffer(I+10)
        PFREE(indx)    =    buffer(I+11)
        indx=indx+1
    enddo

      RETURN
      END


!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE RPSLAI ***                                        C
!C     Input MONTHLY LAI, FILL IN GAPS FOR PERIODS WITH NO LAI DATA     C
!C                                                                      C
!C**********************************************************************C
      SUBROUTINE RPSLAI

      use Common_var
      use mpi
      implicit none
      INTEGER(kind=4) I
      INTEGER(kind=8) NUM_DATA
      INTEGER(kind=2) YEAR, J, M,Mon

      INTEGER(kind=2) Y_LAI_END,Y_LAI_START !,LAI_S_Y,LAI_E_Y

      CHARACTER*100 TEMPHEAD3 (11)

      !MPI specific varaibles
      real*4, allocatable:: buffer(:)
      integer nelement,indx
      
! Define the start and end year of LAI data	
	
	!Print*, "Please input the start and End year of LAI data (Eg: 2000,2012)"
!	Read(*,*) LAI_S_Y,LAI_E_Y
	!LAI_S_Y=1982
  	!LAI_E_Y=2013
    Print*, "Start Year=",LAI_S_Y,"END Year=",LAI_E_Y
!   Set default LAI for the year without LAI input-----
      IF (BYEAR .LT. LAI_S_Y ) then
           Y_LAI_START=LAI_S_Y-BYEAR+1
        ELSE
         Y_LAI_START=1
       ENDIF
      If (IYEND .GT. LAI_E_Y) then 
        Y_LAI_END=LAI_E_Y-BYEAR+1
       Else
        Y_LAI_END=IYEND-BYEAR+1
      Endif
    print*,"reading LAI"
! --- Read and print LAI Data from file in parallel
    !Here total elements for each MPI-task to read are
    !   local_NGRID * NUMBER OF YEARS * NUMBER OF MONTHS * NUMBER OF COLUMNS
    nelement= NGRID * (Y_LAI_END - Y_LAI_START + 1)* 12 * landlai_columns
    if (allocated(buffer) .eqv. .true.) deallocate(buffer)
    allocate(buffer(nelement))
    print*,"infomation for reading LAI",nelement,Y_LAI_END,Y_LAI_START,landlai_columns
    call readData(landlai_fh,nelement,buffer)
    
    
    print*,"finish reading LAI"
    ! Updating the buffered data into relevant arrays.
    indx=1
    do I= 1,NGRID
        do J=Y_LAI_START,Y_LAI_END
            do M=1,12
                HUCNO(I)    =   int(buffer(indx))
                YEAR        =   int(buffer(indx+1))
                Mon         =   int(buffer(indx+2))
                LAI(I,J,M)  =   buffer(indx+3)
                indx=indx+landlai_columns
                !write(*,*) HUCNO(I),YEAR,Mon,LAI(I,J,M)
            enddo
        enddo
    enddo


! --- ASSIGN YEAR LAI_S_Y LAI DATA TO YEARS BEFORE LAI_S_Y
        IF  ( BYEAR .LT. LAI_S_Y)  then
          DO 202 I=1, NGRID

             DO 302 J=1, LAI_S_Y-1

                DO 402 M=1, 12

                LAI(I,J,M) = LAI(I,LAI_S_Y,M)


402             CONTINUE

302          CONTINUE

202        CONTINUE
!
        ENDIF
!
!C--- ASSIGN YEAR Y_LAI_END LAI DATA TO YEARS AFTER Y_LAI_END
      IF (IYEND .GT. Y_LAI_END) then
          DO 203 I=1, NGRID

             DO 303 J=Y_LAI_END+1, NYEAR

                DO 403 M=1, 12

                LAI(I,J,M) = LAI(I,Y_LAI_END,M)


403             CONTINUE

303          CONTINUE

203        CONTINUE

      ENDIF


    RETURN
END

!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE RPSCLIMATE ***                                    C
!C     Input MONTHLY CLIMATE DATA, CALCULATE ANNUAL PPT                 C
!C                                                                      C
!C**********************************************************************C
      SUBROUTINE RPSCLIMATE

      USE Common_var
      implicit none
      INTEGER(kind=8) NUM_DATA
      INTEGER(kind=4) I
      INTEGER(kind=2) YEAR, J, M,Mon

      REAL,POINTER :: ANNPPT(:,:),SUMANPPT(:)

      CHARACTER*10 TEMPHEAD (10)

      !MPI specific varaibles
      real*4, allocatable:: buffer(:)
      integer nelement,indx


      ALLOCATE ( ANNPPT(NGRID,NYEAR), SUMANPPT(NGRID))

      ANNPPT =0.

      SUMANPPT = 0.

      WHERE(AAPPT /= 0.) AAPPT=0.0

! --- Read and print CLIMATE Data from file in parallel
    !   Here total elements for each MPI-task to read are
    !   local_NGRID * NUMBER OF YEARS * NUMBER OF MONTHS * NUMBER OF COLUMNS
    nelement= NGRID * NYEAR * 12 * climate_columns
    if (allocated(buffer) .eqv. .true.) deallocate(buffer)
    allocate(buffer(nelement))
    call readData(climate_fh,nelement,buffer)
    indx=1
    do I = 1,NGRID
        do J = 1,NYEAR
            do M = 1,12
                HUCNO(I)    =   int(buffer(indx))
                YEAR        =   int(buffer(indx+1))
                Mon         =   int(buffer(indx+2))
                RAIN(I,J,M) =   buffer(indx+3)
                TEMP(I,J,M) =   buffer(indx+4)
                ANNPPT(I,J) =   ANNPPT(I,J) + RAIN(I,J,M)
                indx=indx+climate_columns
            enddo
            SUMANPPT(I) = SUMANPPT(I) + ANNPPT(I, J)
        enddo
        AAPPT(I) = SUMANPPT(I)/NYEAR
    enddo

    DEALLOCATE (ANNPPT, SUMANPPT)
    RETURN
END


#else
!SERIAL versions of subroutines

!**********************************************************************C
!                                                                      C
!     *** SUBROUTINE RPSDF ***                                         C
!     Read basic input data from GENERAL.TXT and write to              !
!     BASICOUT.TXT, set up column headings in output files             C
!                                                                      !
!**********************************************************************!
      SUBROUTINE RPSDF

      USE Common_var
      implicit none

      CHARACTER*4 HEADNG(20)
      INTEGER ISCENARIOS
      REAL SNOWPACK

!      COMMON/LAND/ SPRING,SUMMER,FALL,WINTER,SI(366)

!      COMMON/GROUNDWATER/ GROUNDWATER(4000,9), ALAFA

!      COMMON/NODE/NONODE

!      REAL ALAFA
!!!--------------------------------------------------------------------
! --- Read in data from GENERAL.TXT and write to BASICOUT.TXT
      READ(1,1000) HEADNG
 1000 FORMAT(20A4)
      WRITE(77,2010) HEADNG
 2010 FORMAT(' ',20A4/)

      READ(1,*) ISCENARIOS

      WRITE(77,*) ISCENARIOS
2015  FORMAT('Scenario#', I10)


      READ(1,*) NGRID, NYEAR, BYEAR, NLC

      WRITE(77,2020) NGRID
 2020 FORMAT(I5,'ACTIVE GRIDS'/)

      WRITE(77,2030) NYEAR, BYEAR

 2030 FORMAT(I10,'YEARS TO BE SIMULATED AND',' FIRST YEAR =', I10)

      WRITE(77,2040) NLC

 2040 FORMAT('NUMBER OF LAND COVER CATEGORIES: ',I10)

      READ (1,*) IYSTART, IYEND

      WRITE(77,2050) IYSTART, IYEND
2050  FORMAT('FOR SIMULATION SUMMARY, YEAR TO START',I10, ' , END ',I10)


      READ (1,*) FPERD

!--  reduction fraction of Leaf Area index for scenario analysis

      READ (1, *) FPERDLAI
!1033  FORMAT (F10.2)

      WRITE(77,2058) FPERD, FPERDLAI
2058  FORMAT('DEFOREST RATE%',F10.2, /'LAI REDUCTION%=', F10.2)


       READ (1,*) SNOWPACK
!1046   FORMAT (F10.2)

      WRITE(77,1047) SNOWPACK

1047  FORMAT('INTIAL SNOWPACK (MM) = :', F10.2)

!!!--------------------------------------------------------------------
!-----PRINT TITLE FOR MONTHLY OUTPUT FILE MONTHRUNOFF.TXT

      WRITE (78, 1050)
1050  FORMAT('CELL,YEAR,MONTH,PRECIP,TEMP,', &
      'SMC,SNWPK,PET,AET,Sun_ET,',&
      'RUNOFF,BASEFLOW,FLOWMCMMon')
!-----PRINT TITLE FOR MONTHLY OUTPUT FILE Soil Storage.TXT
      WRITE (900, 1055)
1055  FORMAT('CELL,YEAR,MONTH,UZTWC,UZFWC,',&
      'LZTWC,LZFPC,LZFSC')

!-----PRINT TITLE FOR ANNUAL OUTPUT ANNUALFLOW.TXT

       WRITE (79, 1060)
1060  FORMAT ( 'CELL,YEAR,RAIN,PET,',&
      'AET,Sun_ET,RUNOFF,RUN_Pratio,ET_Pratio,RUN_ETRatio,', &
       'SNWPCKMON,RFACTOR')

!-----PRINT TITLE FOR SUMMARY OUTPUT SUMMARRUNOFF.TXT

      WRITE (80, 1070)
1070  FORMAT ('CELL,RAIN,PET,',&
      'AET,RUNOFF,RUNOFF/P,ET/P,(RUN+ET)/P',&
       'RFACTOR,Y_n')


         WRITE (910,1080)

1080    FORMAT ('WATERSHEDID,YEAR,LADUSEID,',&
      'HUCRUNOFF,FLOWVOL,LAND%,HUCAREA')



         WRITE (920,1090)

1090    FORMAT ('WATERSHEDID,YEAR,CROPFLOW,',&
       'FORESTFLOW,GRASSFLOW,SHRUBSAVAFLOW,URBANWATERFLOW,TFLOW')

       WRITE (2003, 204)
!
204      FORMAT ('WATERSHEDID Year Month RAIN SP  PET &
            &AET PAET RUNOFF PRIBF SECBF INTF &
            &AVSMC EMUZTWC  EMUZFWC EMLZTWC  EMLZFPC  EMLZFSC')


 WRITE (400, 500)
500    FORMAT ('CELL,YEAR,MONTH,GEP(gC/m2/Month),Reco,NEE')


        WRITE (500, 600)
600    FORMAT ('CELL,YEAR,GEP(gC/m2/yr),Reco,NEE(gC/m2/yr)')


        WRITE (600, 650)
650    FORMAT ('CELL,NO_YR,GEP(gC/m2/yr),Reco,NEE')


        WRITE (700, 700)

700     FORMAT ('CELL,YEAR,TREE,MAMMALS,BIRD, ', &
       'AMPHIB, REPTILES, VERTEB, AET, PET')

            WRITE (800,800)

800      FORMAT ('CELL,NO_YR, TREE, MAMMALS, BIRD,',&
        'AMPHIB, REPTILES, AHUCVERTEB')

      RETURN
      END

!**********************************************************************!
!                                                                      !
!     *** SUBROUTINE RPSINT ***                                        !
!     Read in landuse data from CELLINFO.TXT, calcuate change in       !
!     landuse based on percent forest decrease (if desired), write     !
!     to BASICOUT.TXT                                                  !
!     �����趨�Ĳɷ�ֵ�ؼ����������������                             !
!**********************************************************************!
      SUBROUTINE RPSINT 

      Use Common_var
       implicit none

      REAL CROP

      INteger(kind=2) year,J
    INTEGER(kind=4) I , ID


      CHARACTER*1000 DUMY(30)
      
! --- Read and print land use data for each active cell IN THE BASIC.OUT FILE
      WRITE(77,2000)
2000  FORMAT(/'LANDUSE INFO FOR EACH SIMULATION CELL'/)
!      READ (2,500) DUMY
500   FORMAT (1000A30)
      

! ----LANC = raw Landcover types    
      
!      WRITE (77,500) DUMY
             
      DO 10 I=1, NGRID

      READ(2,REC=I) ID, HUCNO(I), LATUDE(I), LONGI(I),LADUSE(I),HUCELE(I)
      
             
!      WRITE(*,1100) ID, HUCNO(I),LATUDE(I), LONGI(I) 
!     > (LADUSE(I,K),K=1, NLC)

1100  FORMAT(2I10, 2F10.4, I4)    
     
10    CONTINUE


! --- Read and print SOIL PARAMETERS for each active cell IN THE BASIC.OUT FILE
!
      WRITE(77,2051)
2051  FORMAT(/'SOIL PARAMETERS FOR EACH SIMULATION CELL'/)
!      READ (7,550) DUMY
550   FORMAT (30A8)
      
      
!      WRITE (77,550) DUMY
          
      DO 15 I=1, NGRID

      READ(7,REC=I) ID, HUCNO(I), UZTWM(I), UZFWM(I), UZK(I), ZPERC(I),&
     REXP(I), LZTWM(I), LZFSM(I), LZFPM(I), LZSK(I),&
     LZPK(I), PFREE(I)
            
!      WRITE(*,1150) HUCNO(I), UZTWM(I), UZFWM(I), UZK(I), ZPERC(I),&
!     REXP(I), LZTWM(I), LZFSM(I), LZFPM(I), LZSK(I),&
!     LZPK(I), PFREE(I)
!    print*,I 

!! this part is used for soil data calibration
           
   !    UZTWM(I)=UZTWM(I)*1
   !    UZFWM(I)=UZFWM(I)*1
   !    UZK(I)=UZK(I)*1 
 !      ZPERC(I)=ZPERC(I)*1
   !   REXP(I)=REXP(I)*1
      ! LZTWM(I)=LZTWM(I)*1.3 !*(1+VAL_1(TUN1)) 
    ! LZFSM(I)=LZFSM(I) !*
       ! LZFPM(I)=LZFPM(I)*2.4
       ! LZSK(I)= LZSK(I)*0.9 !*(1+VAL_2(TUN2)) 
       ! LZPK(I)=LZPK(I)*1.2
    ! PFREE(I)=PFREE(I) !


1150  FORMAT(I12, 11F10.4)    
     
15    CONTINUE


! ---- Converting forest to two types of croplands in the same watersheds
!------����趨�Ĳɷ����ؼ���ֲ�������е�ĳЩ�仯ֲ������
 
!      DO 20 I=1, NGRID
!      
!           
!      CROP = LADUSE(I,1) + LADUSE(I,2)
!     
!      IF (CROP .NE. 0.) THEN
! 
!      LADUSE(I,1)=(LADUSE(I,1)/(LADUSE(I,1) + LADUSE(I,2))) 
!     & * FPERD * (LADUSE(I,4) + LADUSE(I,5)+ LADUSE(I,6)+   
!     &LADUSE(I,7) + LADUSE(I,8)+ LADUSE(I,11) ) 
!     &+LADUSE(I,1)
!             
!      
!      LADUSE(I,2)=(LADUSE(I,2)/(LADUSE(I,1) + LADUSE(I,2)))
!     & * FPERD * (LADUSE(I,4) + LADUSE(I,5)+ LADUSE(I,6) +   
!     & LADUSE(I,7) + LADUSE(I,8)+ LADUSE(I,11) ) 
!     & + LADUSE(I,2)
! 
!      ELSEIF (CROP .EQ. 0.) THEN
!     
!         LADUSE(I,1) = 0.5*FPERD
!     & *FPERD * (LADUSE(I,4) + LADUSE(I,5)+ LADUSE(I,6)   
!     & +LADUSE(I,7)+LADUSE(I,8)+ LADUSE(I,11) ) 
!     & +LADUSE(I,1)
!      
!        LADUSE(I,2) = 0.5
!     & * FPERD * (LADUSE(I,4) + LADUSE(I,5)+ LADUSE(I,6)+   
!     & LADUSE(I,7) + LADUSE(I,8)+ LADUSE(I,11))
!     & + LADUSE(I,2)
!     
!         ENDIF 
!                          
!      LADUSE(I,4) = LADUSE(I,4)*(1-FPERD)
!      LADUSE(I,5) = LADUSE(I,5)*(1-FPERD)
!      LADUSE(I,6) = LADUSE(I,6)*(1-FPERD)
!      LADUSE(I,7) = LADUSE(I,7)*(1-FPERD)
!      LADUSE(I,8) = LADUSE(I,8)*(1-FPERD)
!      LADUSE(I,11) = LADUSE(I,11)*(1-FPERD)
!  20    CONTINUE

      RETURN
      END


  
      
!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE RPSLAI ***                                        C
!C     Input MONTHLY LAI, FILL IN GAPS FOR PERIODS WITH NO LAI DATA     C
!C                                                                      C
!C**********************************************************************C
      SUBROUTINE RPSLAI
      
      USE Common_var
      implicit none             
      INTEGER(kind=4) I       
 	INTEGER(kind=8) NUM_DATA     
      INTEGER(kind=2) YEAR, J, M,Mon

      INTEGER Y_2000 ,Y_LAI_END ,Y_2014,Y_LAI_START     
     
      CHARACTER*100 TEMPHEAD3 (11)
            
   
 
!   Set default LAI for the year without LAI input-----


      IF (BYEAR .LT. 2000 ) then
           Y_LAI_START=2000-BYEAR+1
        ELSE
         Y_LAI_START=1
       ENDIF
      If (IYEND .GT. 2014) then 
        Y_LAI_END=2014-BYEAR+1
       Else
        Y_LAI_END=IYEND-BYEAR+1
      Endif

      Y_2000=2000-BYEAR+1
      Y_2014=2014-BYEAR+1

! --- READ IN LAI DATA FROM LANDLAI.TXT


      DO 201 I=1, NGRID
                
         DO 301 J= Y_LAI_START,Y_LAI_END 
         
            DO 401 M=1, 12

            IF (I .EQ. 1 .AND. J .EQ. Y_LAI_START .AND. M .EQ. 1) THEN 

!               READ (8, 902) TEMPHEAD3
 
 902           FORMAT (100A11)
 
            ENDIF
                      
 
! --- LAI_* IS THE LAI FOR LANDUSE * (8 TOTAL IN LANDLAI.TXT)
	NUM_DATA=(I-1)*(Y_LAI_END-Y_LAI_START+1)*12+(J-Y_LAI_START)*12+M
         READ(8,REC=NUM_DATA) HUCNO(I),YEAR,Mon,LAI(I,J,M)  
            
!         WRITE(*,*),I,HUCNO(I),YEAR,Mon,LAI(I,J,M)
    
!1011        FORMAT(3I10, 8F10.2)               


401         CONTINUE 

301      CONTINUE

201   CONTINUE

! --- ASSIGN YEAR 2000 LAI DATA TO YEARS BEFORE 2000
! -----��2000�����ݸ�����ǰ�����
        IF  ( BYEAR .LT. 2000)  then
          DO 202 I=1, NGRID
                
             DO 302 J=1, Y_2000-1

                DO 402 M=1, 12

                LAI(I,J,M) = LAI(I,Y_2000,M)
                      
     
402             CONTINUE 

302          CONTINUE

202        CONTINUE
!
        ENDIF
!          
!C--- ASSIGN YEAR 2014 LAI DATA TO YEARS AFTER 2014
!C--- ��2014�����ݸ����Ժ�����
      IF (IYEND .GT. 2014) then
          DO 203 I=1, NGRID
                
             DO 303 J=Y_2014+1, NYEAR

                DO 403 M=1, 12

                LAI(I,J,M) = LAI(I,Y_2014,M)
       

403             CONTINUE 

303          CONTINUE

203        CONTINUE
!
      ENDIF
	  
	  
	  	  
	  
! --- WRITE LAI DATA TO BASICOUT.TXT FOR VALIDATION
            

        WRITE (77, 801)
       
801    FORMAT (/'LAI DATA for each Cell'/)

 
!      WRITE (77, 902) TEMPHEAD3
      

	  !************* -----------Read annual land cover data------------ ***************

!      READ (3,5001) DUMY
!5001   FORMAT (1000A30)

! ----LANC = raw Landcover types    
      
           
 !     DO 105 I=1, NGRID   ! start and end year of land cover data
        
!        DO 106 J=Y_2000,Y_2014

!      READ(3,*) HUCNO(I),YEAR,veg(I,J)  
             
     ! WRITE(*,*) HUCNO(I),YEAR,veg(I,J) 
 
!106    CONTINUE  
!105    CONTINUE
	  
	  
	  
	  
      RETURN
      END


!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE RPSCLIMATE ***                                    C
!C     Input MONTHLY CLIMATE DATA, CALCULATE ANNUAL PPT                 C
!C                                                                      C
!C**********************************************************************C
      SUBROUTINE RPSCLIMATE
      
      USE Common_var
	  implicit none
	INTEGER(kind=8) NUM_DATA 
      INTEGER(kind=4) I              
      INTEGER(kind=2) YEAR, J, M,Mon
     
      REAL,POINTER :: ANNPPT(:,:),SUMANPPT(:)
      
      CHARACTER*10 TEMPHEAD (10)

      ALLOCATE ( ANNPPT(NGRID,NYEAR), SUMANPPT(NGRID))
      
      ANNPPT =0.
      
      SUMANPPT = 0.

      WHERE(AAPPT /= 0.) AAPPT=0.0
      

      DO 5000 I=1,NGRID
         DO 5001 J=1,NYEAR
            DO 5002 M=1,12
        	NUM_DATA=(I-1)*(NYEAR)*12+(J-1)*12+M
               READ(4,REC=NUM_DATA) HUCNO(I), YEAR, Mon, RAIN(I,J,M), TEMP(I,J,M)
               ANNPPT(I, J) = ANNPPT(I, J) + RAIN(I,J,M)
5002        CONTINUE
            SUMANPPT(I) = SUMANPPT(I) + ANNPPT(I, J)
5001     CONTINUE
         AAPPT(I) = SUMANPPT(I)/NYEAR
         WRITE(77,5004) HUCNO(I), AAPPT(I)
5004     FORMAT(I10,F10.2)

!	Print *,"I=",I
!	WRITE(*,*) HUCNO(I), AAPPT(I)

5000  CONTINUE
		close(1010)
	DEALLOCATE (ANNPPT, SUMANPPT)
	
      RETURN
END
#endif
