program windtunnel
!simulates an object in a windtunnel

    use vars
    use parallel
    use cuda_kernels
    implicit none

    device = .true.
    call setup_MPI()

    call get_params()

    call setup()
    call setup_gpu()

    call solver()

    call writetofile('output.dat')

    call MPI_Finalize(ierr)

end program
