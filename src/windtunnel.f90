program windtunnel
!simulates an object in a windtunnel

    use vars
    use parallel

#ifdef USE_CUDA
    use cuda_kernels
#endif
    implicit none

    call setup_MPI()

    call get_params()

   


    call setup()
#ifdef USE_CUDA
    call setup_gpu()
#endif

    call solver()

    call writetofile('output.dat')

    call MPI_Finalize(ierr)

end program
