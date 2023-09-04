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
    
    call check_vorticity()
    
    call mpi_finalize(ierr)

contains

    
    
    subroutine check_vorticity()
            use vars
            use cuda_kernels
            implicit none 
            real(8) , dimension(0:nx+1,0:ny+1) :: vort2
            real(8) , dimension(0:nx+1,0:ny+1) :: vort_backup
            real(8) :: diff
            


            call getvort_cpu()

            vort_backup=vort
            u_dev=u
            v_dev=v
            vort_dev=vort
            mask_dev=mask


            call navier_stokes_cpu( )

            
            call navier_stokes_gpu()


            vort2=vort_dev

            diff = sum( abs(vort2(1:nx,1:ny) - vort(1:nx,1:ny)) )
            call check( "navier_stokes bulk", diff < 1e-7)
            diff = sum( abs(vort2 - vort ) )
            call check( "navier_stokes", diff < 1e-7)

            !print *,diff

            


    end subroutine

    
    





end program
