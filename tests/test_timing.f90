program test_timing
    use mpi 
    use timing_mod
    implicit none


    integer :: ierr
    type(timing) :: dummyTimer
    real(kind=dp) :: t=0

    call MPI_Init(ierr)

    dummyTimer%name="dummy"

    call dummyTimer%start()

    call sleep(10)

    call dummyTimer%stop()

    t=dummyTimer%time()

    print *, "Timer time elapsed:",t

    if (t<9.5 .or. t>10.5) call exit(1)

    call dummyTimer%start()

    call sleep(5)

    call dummyTimer%stop()

    t=dummyTimer%time()

    print *, "Timer time elapsed:",t

    if (t<14.5 .or. t>15.5) call exit(2)


    call MPI_Finalize(ierr)



end program
