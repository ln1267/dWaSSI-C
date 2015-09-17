
!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE VALIDATION ***                                 C
!C     Simulate GEP AND NEE for selected HUC                            C
!C     WRITE GEP AND NEE TO MONTHCARBON.TXT, ANNUALCARBON.TXT, HUCCARBON.TXT        C
!C     SIMULATE BIODIVERSITY FOR SELECTED HUC                           C
!C     WRITE BIODIVERSITY TO HUCBIO.TXT                                 C
!C                                                                      C
!C**********************************************************************C
!C        I=HUC; J= YEAR, M =MONTH, MNDAY= NO OF DAYS IN A MONTH

      SUBROUTINE validation      !(TUN1,TUN2)
      

      use common_var
      implicit none 
! --------------------------------------------------------------
      INTEGER I,J, M,num_m,num_y,num_F,TUN1,TUN2

      REAL SUM_GEP,SUM_GEP_V,SUM_AET,SUM_PET,SUM_GPP,SUM_NPP,SUM_ET_V
      REAL AVE_GEP,AVE_GEP_V,AVE_AET,AVE_PET,AVE_GPP,AVE_NPP,AVE_ET_V
      real R_GEP,R_AET,R_PET,R_GPP,R_NPP,P_GEP,P_AET,P_PET,P_GPP,P_NPP,&
      NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP
      real  SUM_GEP_U,SUM_AET_U,SUM_PET_U,SUM_GPP_U,SUM_NPP_U
      real  SUM_GEP_L,SUM_AET_L,SUM_PET_L,SUM_GPP_L,SUM_NPP_L
      real  SUM_GEP_R,SUM_AET_R,SUM_PET_R,SUM_GPP_R,SUM_NPP_R
      real  SUM_GEP_N,SUM_AET_N,SUM_PET_N,SUM_GPP_N,SUM_NPP_N
      real  SUM_GEP_X,SUM_GEP_Y,SUM_GEP_XY,SUM_GEP_XX,SUM_GEP_YY
      real  SUM_AET_X,SUM_AET_Y,SUM_AET_XY,SUM_AET_XX,SUM_AET_YY
      real  SUM_PET_X,SUM_PET_Y,SUM_PET_XY,SUM_PET_XX,SUM_PET_YY
      real  SUM_GPP_X,SUM_GPP_Y,SUM_GPP_XY,SUM_GPP_XX,SUM_GPP_YY
      real  SUM_NPP_X,SUM_NPP_Y,SUM_NPP_XY,SUM_NPP_XX,SUM_NPP_YY
      
! ---------------------------------------------------------------      
                     
     
      REAL ANGEP, ANRECO, ANNEE,  AHUCGEP, AHUCNEE,AHUCRE
      REAL HUCGEP, HUCNEE, HUCRE

      REAL FLOW_L,RUNOFF_L,BASEFLOW_L, UNIONAREA

      REAL R_RUNOFF,R_FLOW,R_BASEFLOW
      REAL P_RUNOFF,P_FLOW,P_BASEFLOW
      REAL NS_RUNOFF,NS_FLOW,NS_BASEFLOW


      REAL SUM_RUNOFF,SUM_RUNOFF_V,SUM_RUNOFF_X,SUM_RUNOFF_Y,&
           SUM_RUNOFF_XY,SUM_RUNOFF_XX,SUM_RUNOFF_YY,    &
           SUM_RUNOFF_U,SUM_RUNOFF_L,SUM_RUNOFF_R,SUM_RUNOFF_N,&
           SUM_BASEFLOW,SUM_BASEFLOW_V,SUM_BASEFLOW_X,SUM_BASEFLOW_Y,&
           SUM_BASEFLOW_XY,SUM_BASEFLOW_XX,SUM_BASEFLOW_YY,&
           SUM_BASEFLOW_U,SUM_BASEFLOW_L,SUM_BASEFLOW_R,SUM_BASEFLOW_N,&
           AVE_RUNOFF,AVE_RUNOFF_V,AVE_BASEFLOW,AVE_BASEFLOW_V
      

!----------------------------------------------------------------      


!            WRITE (2000, 201) 
!            
!201      FORMAT ('cell,YEAR,Month,GEP,NEE,
!     > GEP_V,AET, PET,S_PET,ET_V') 
!            
!             WRITE (2001, 202) 
!            
!202      FORMAT ('cell,year,GEP,NEE,
!     > AET,PET, GPP_V,NPP_V')  
!        
!           
            WRITE (2002, 203) 
            
203      FORMAT ('Cell,month,RUNOFF,BASEFLOW')
!C     > NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP,
!C    > P_GEP,P_AET,P_PET,P_GPP,P_NPP')    
             
     
      
      FLOW_L=0  !FLOW 总量临时文件
      RUNOFF_L=0
      BASEFLOW_L=0


	  
!         ------- Monthly Runoff and Baseflow data for the whole watershed
      DO 1001 I=1,NGRID
	  
         DO 1002 M=1,12
			
			RUNOFF_L=0
			BASEFLOW_L=0
            DO 1003 J=1,NYEAR
			              
            RUNOFF_L=RUNOFF_L+RUN_HRU(I,J,M)
            BASEFLOW_L=BASEFLOW_L+BASE_HRU(I,J,M)
            
!			print *, I,M,RUN_HRU(I,J,M),BASE_HRU(I,J,M),HUCAREA(I),RUNOFF_L,BASEFLOW_L,UNIONAREA
1003        CONTINUE 
         RUNOFF_MON(I,M)=RUNOFF_L/NYEAR
         BASEFLOW_MON(I,M)=BASEFLOW_L/NYEAR    

         WRITE(2002,20003),I,M,RUNOFF_MON(I,M),BASEFLOW_MON(I,M)
20003    FORMAT (I10,',',I10,',',f10.3, ',', f10.3) 

1002     CONTINUE
1001  CONTINUE
  
    
!C     print*,"输入回车进入下一步操作"
!C     READ(*,*) 
     
      RETURN
      END
