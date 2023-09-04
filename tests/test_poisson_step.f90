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
    
    call check_poisson_step()

    call mpi_finalize(ierr)
    
contains
    
    subroutine check_poisson_step()
        implicit none
        real(8), dimension(:,:) , allocatable :: psi_backup,psi_cpu
        integer :: istat
        real(8) :: diff

        allocate(psi_backup(lbound(psi,1):ubound(psi,1),lbound(psi,2):ubound(psi,2)))
        allocate(psi_cpu(lbound(psi,1):ubound(psi,1),lbound(psi,2):ubound(psi,2)))
        psi_backup=psi
        call poisson_cpu(1)
        psi_cpu=psi


        psi_dev=psi_backup
        mask_dev=mask
        call poisson_gpu(1)
        psi=psi_dev
        call check( "Bulk difference check n=1: " ,  sum( abs( psi_cpu - psi ) )<1e-7 )

        psi=psi_backup

        print *, "running on the cpu..."
        call poisson_cpu(3000)
        psi_cpu=psi


        psi_dev=psi_backup
        print *, "running on the gpu..."
        call poisson_gpu(3000)
        
        istat= cudaDeviceSynchronize()
        psi=psi_dev
        !call check( "Bulk difference check n=5000: " ,  sum( abs( psi_cpu(1:nx,1:ny) - psi(1:nx,1:ny) ) )<1e-1 )

        diff = sum( abs(psi_cpu(1:nx,1:ny) - psi(1:nx,1:ny) ) )/sum(abs(psi_cpu(1:nx,1:ny)))
        
        print *, cudaGetErrorString(cudaGetLastError())
        call check( "Bulk difference check n=3000: " , diff<1e-6 )

        
        print *, diff



    end subroutine
    





end program
