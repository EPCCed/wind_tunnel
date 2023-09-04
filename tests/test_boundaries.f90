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
    
    call check_vertical_boundary()
    call check_right_boundary()

    
    call mpi_finalize(ierr)

contains

    
    subroutine check_right_boundary()
        implicit none

        real(8), dimension( :, :) , allocatable,device :: psi_dev
        real(8), dimension( :, :) , allocatable :: psi_cpu


        allocate(psi_cpu(0:nx+1,0:ny+1) ,  psi_dev(0:nx+1,0:ny+1) )

        psi_cpu=1
        psi_cpu(nx,1:ny)=0

        psi_dev = psi_cpu

        call fill_right_boundary_gpu(psi_dev)
        psi_cpu=0
        psi_cpu=psi_dev

        call check("Check right boundary", sum( abs(psi_cpu(nx+1,1:ny) - (-1)) ) < 1e-5   )

    end subroutine 
    
    
    subroutine check_vertical_boundary()
        use parallel
        use mpi
        implicit none

        real(8), dimension( :, :) , allocatable,device :: psi_dev
        real(8), dimension( :, :) , allocatable :: psi_cpu

        allocate(psi_cpu(0:nx+1,0:ny+1) ,  psi_dev(0:nx+1,0:ny+1) )

        psi_cpu=1
        psi_cpu(:, ny )=0
        psi_cpu(:,1)=0

        psi_dev = psi_cpu

        call fill_vertical_boundary_gpu(psi_dev)
        psi_cpu=0
        psi_cpu=psi_dev

        if (up .eq. MPI_PROC_NULL) call check("Check top boundary",sum( abs(psi_cpu(:,ny+1) - (-1))  ) < 1e-5 )
        if (down .eq. MPI_PROC_NULL ) call check("Check bottom boundary",sum( abs( psi_cpu(:,0) - (-1)) ) < 1e-5 )


    end subroutine  





end program
