
subroutine setup_local_arrays_dev()
    use vars
    use cuda_kernels
    implicit none
    allocate(psi_dev(0:nx+1,0:ny+1), vort_dev(0:nx+1,0:ny+1), v_dev(nx,ny), u_dev(nx,ny), dw_dev(0:nx+1,0:ny+1) )
    
    allocate(mask_dev(nx,ny) )
    
end subroutine

subroutine setup_local_grid_info_from_cpu()
    use vars
    use cuda_kernels
    use parallel
    implicit none
    dx_dev=dx 
    dy_dev=dy 
    nx_dev=nx 
    ny_dev=ny
    up_dev=up
    down_dev=down
end subroutine


subroutine setup_vorticity_evolution_gpu()
    use vars
    use cuda_kernels
    implicit none
    dt_dev=dt 
    vmax_dev=vmax
    maxvort_dev=maxvort
    r0_dev=r0

end subroutine


subroutine setup_gpu()
    ! Setting up the gpu should be done after setting up the cpu 
    use vars 
    use cuda_kernels
    use mpi
    use cudafor

    implicit none

    integer :: n_devices, current_device
    integer :: rank, n_ranks, ierr


    call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr );
    call MPI_Comm_size (MPI_COMM_WORLD,n_ranks,ierr);

    ierr=cudaGetDeviceCount(n_devices)
    ierr=cudaSetDevice(rank)

    ierr=cudaGetDevice(current_device)



    if (rank == 0) then
        print *,"n. of devices:", n_devices
    endif
    print *, "Device", current_device, "on rank",rank

    call setup_local_arrays_dev()
    call setup_local_grid_info_from_cpu()
    call setup_vorticity_evolution_gpu()
    

    ! copy initial conditions from the cpp
    psi_dev=psi 
    mask_dev=mask
    vort_dev=vort
    
end subroutine


subroutine fieldsFromGpuToCpu()
    use vars
    use cuda_kernels
    if (device == .true.) then 
        psi=psi_dev
        u=u_dev
        v=v_dev
        vort=vort_dev
    endif

end subroutine



