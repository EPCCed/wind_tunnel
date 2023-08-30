program test_timing
    use mpi
    use timings_mod
    implicit none
    
    integer :: ierr,i
    type(timings) :: dummyTimers
    integer :: rank,nRanks

    call MPI_Init(ierr)
    call MPI_Comm_rank( MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size( MPI_COMM_WORLD, nRanks, ierr)
    

    call dummyTimers%init(1)
    dummyTimers%timers(1)%name="Dummy1"
    call dummyTimers%timers(1)%start
    call sleep(2)
    call dummyTimers%timers(1)%stop

    call dummyTimers%write(0)

    if (rank ==0) then
        do i=1,nRanks
            if ( check_interval(dummyTimers%gathered_times(1) , 1.5d0,2.3d0 ) ) then 
                write(*,*) "Gather time at rank",i,"is wrong"
                call exit(1)
            endif 
        end do



    endif


    call MPI_Finalize(ierr)

    contains

    function check_interval( value, min, max  ) result(passed)
        logical :: passed
        real*8, intent(in) :: value,min,max

        if (value < min .or. value>max) then 
            passed=.true.
        else 
            passed=.false.
        endif
    end function


end program
