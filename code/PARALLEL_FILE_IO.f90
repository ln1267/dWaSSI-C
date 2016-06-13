!********************************************C
!     *** MPI convinience subroutines  ***   C
!********************************************C
#ifdef MPI
! Open all files for read write in serial or parallel.
subroutine open_io
    use common_var
    use mpi
    implicit none
    character(len=256) filename
! Open serial files only on master
    if (rank .eq. 0) then
        open(general_fh,FILE=TRIM(INPATH)//'/GENERAL.TXT')
        open(basicout_fh,FILE=TRIM(OUTPATH)//'/BASICOUT.TXT')
!        open(annualflow_fh,FILE=TRIM(OUTPATH)//'/ANNUALFLOW.TXT')
    endif

! Open parallel files to read in the data
    filename=TRIM(INPATH)//'/CELLINFO.DAT'
    call mpi_file_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, MPI_INFO_NULL, cellinfo_fh, ierr)
    filename=TRIM(INPATH)//'/CLIMATE.DAT'
    call mpi_file_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, MPI_INFO_NULL, climate_fh, ierr)
    filename=TRIM(INPATH)//'/SOILINFO.DAT'
    call mpi_file_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, MPI_INFO_NULL, soilinfo_fh, ierr)
    filename=TRIM(INPATH)//'/LANDLAI.DAT'
    call mpi_file_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, MPI_INFO_NULL, landlai_fh, ierr)

! Open parallel files to write in the data
    filename=TRIM(OUTPATH)//'/VALIDATION.DAT'
    call mpi_file_open(MPI_COMM_WORLD, filename, MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, validation_fh, ierr)

    filename=TRIM(OUTPATH)//'/MONTHFLOW.DAT'
    call mpi_file_open(MPI_COMM_WORLD, filename, MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, monthflow_fh, ierr)

    filename=TRIM(OUTPATH)//'/SOILSTORAGE.DAT'
    call mpi_file_open(MPI_COMM_WORLD, filename, MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, soilstorage_fh, ierr)

    filename=TRIM(OUTPATH)//'/MONTHCARBON.DAT'
    call mpi_file_open(MPI_COMM_WORLD, filename, MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, monthcarbon_fh, ierr)

    filename=TRIM(OUTPATH)//'/ANNUALFLOW.DAT'
    call mpi_file_open(MPI_COMM_WORLD, filename, MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, annualflow_fh, ierr)

    filename=TRIM(OUTPATH)//'/ANNUALCARBON.DAT'
    call mpi_file_open(MPI_COMM_WORLD, filename, MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, annualcarbon_fh, ierr)

     filename=TRIM(OUTPATH)//'/HUCFLOW.DAT'
    call mpi_file_open(MPI_COMM_WORLD, filename, MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, hucflow_fh, ierr)

     filename=TRIM(OUTPATH)//'/HUCCARBON.DAT'
    call mpi_file_open(MPI_COMM_WORLD, filename, MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, huccarbon_fh, ierr)

    ! Composition information of the input files

    cellinfo_columns=5
    climate_columns=5
    soilinfo_columns=12
    landlai_columns=4

   ! Composition information of the input files
   validation_columns   =   18
   monthflow_columns    =   13
   soilstorage_columns  =   8
   monthcarbon_columns  =   6
   annualflow_columns   =   12
   annualcarbon_columns =   5
   hucflow_columns      =   9
   huccarbon_columns    =   5

end


! Close all the open files
subroutine close_io
    use common_var
    use mpi
    implicit none
    if (rank .eq. 0) then
        close(general_fh)
        close(basicout_fh)
    endif
    call mpi_file_close(cellinfo_fh,ierr)
    call mpi_file_close(climate_fh,ierr)
    call mpi_file_close(soilinfo_fh,ierr)
    call mpi_file_close(landlai_fh,ierr)

    call mpi_file_close(validation_fh,ierr)
    call mpi_file_close(monthflow_fh,ierr)
    call mpi_file_close(soilstorage_fh,ierr)
    call mpi_file_close(monthcarbon_fh,ierr)
    call mpi_file_close(annualflow_fh,ierr)
end

!! Subroutine to read in the data from file unit fname in parallel.
subroutine readData(fname,nelement,buf)
    use common_var
    use mpi
    implicit none

    integer, intent(in) :: nelement,fname
    integer :: i
    integer(kind=MPI_OFFSET_KIND) offset
    real*4 tmp_real
    real*4 , intent(inout):: buf(nelement)
    offset=rank * nelement *sizeof(tmp_real)
    call mpi_file_read_at(fname,offset,buf,nelement,MPI_REAL,MPI_STATUS_IGNORE,ierr)
end subroutine readData


!! Subroutine to read in the data from file unit fname in parallel.
subroutine writeData(fname,nelement,buf)
    use common_var
    use mpi
    implicit none

    integer, intent(in) :: nelement,fname
    integer :: i,rem,meangrid
    integer(kind=MPI_OFFSET_KIND) offset
    real*4 tmp_real
    real*4 , intent(inout):: buf(nelement)
	if (rank .eq. nprocs-1) then
		rem=mod(TGRID,nprocs)
		! If NGIRD cannot be chunked evenly
		meangrid=(TGRID-rem)/(nprocs)
		offset=rank *(nelement/NGRID)*meangrid*sizeof(tmp_real)
	else
		offset=rank * nelement *sizeof(tmp_real)
	endif
	write(*,*)fname, TGRID,rank,NGRID,offset
    call mpi_file_write_at(fname,offset,buf,nelement,MPI_REAL,MPI_STATUS_IGNORE,ierr)


end subroutine writeData


subroutine print_buffer_files(nelement,buffer,tuple)
    use common_var
    use mpi
    implicit none
    integer,intent(in) :: nelement,tuple
    real*4 , intent(inout):: buffer(nelement)

    character(len=32) filename,rank_str
    integer myout,i,j,ind

    write(*,*) "[",rank,"] ", NGRID ," ",tuple, " ",nelement

    write(rank_str,'(I5)') rank
    filename='file'//ADJUSTL(rank_str)
    myout=rank+120
    open(unit=myout,file=filename)
    do i=1,nelement,tuple
       write(myout,*) (buffer(j),j=i,i+(tuple-1))
    enddo

    close(myout)
end subroutine print_buffer_files



#endif
