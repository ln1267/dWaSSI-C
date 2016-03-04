!********************************************C
!     *** MPI convinience subroutines  ***   C
!********************************************C
#ifdef MPI
! Decompose the problem domain of size NGRID into MPI task specific chunks.
! In the case where NGRID is not exactly divisible by the total MPI-Tasks,
! the remainder of the work is given to the last MPI-task.
subroutine decompose_ngrid
    use common_var
    use mpi
    implicit none

    integer rem,mygrid
    rem=mod(NGRID,nprocs)
    ! If NGIRD cannot be chunked evenly
    mygrid=(NGRID-rem)/(nprocs)
    my_grid_start   =   rank * mygrid + 1
    my_grid_end     =   (rank + 1) * mygrid
    if (rank .eq. nprocs-1) then
        mygrid          =       rem + mygrid
        my_grid_end     =       my_grid_end + rem
    endif
	TGRID=NGRID
    NGRID=mygrid

end
#endif
