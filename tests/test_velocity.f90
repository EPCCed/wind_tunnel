program tests
!simulates an object in a windtunnel

    use vars
    use parallel
    use poisson_solver_cuda_mod
    use test_tools
    
    call setup_MPI()

    call get_params()

    device = .true.

    call setup()

    call setup_gpu()
    
    call check_velocity()


    call mpi_finalize(ierr)

contains

    subroutine check_velocity()
        use vars
        use cuda_kernels
        implicit none
        real(8) , allocatable, dimension(:,:) :: u2,v2

        u=0.
        v=0.

        u_dev=0
        v_dev=0

        psi_dev=psi
        u_dev=u 
        v_dev=v

        allocate( u2(nx,ny),v2(nx,ny) )

        call getv_cpu()
        call getv_gpu()

        u2=u_dev
        v2=v_dev
        
        call check( "Test bulk velocity u", sum(abs(u2 - u))< 1e-5  )
        call check( "Test bulk velocity v", sum(abs(v2 - v))< 1e-5  )

        print *,sum(abs(u2 - u))
    end subroutine
    





end program
