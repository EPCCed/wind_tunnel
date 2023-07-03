
subroutine getv_gpu()
    use vars
    use cuda_kernels
    use mpi
    use cudafor
    implicit none
    integer :: istat,ierr
    
    type(dim3) block_bulk, grid_bulk
    type(dim3) block_h, grid_h
    type(dim3) block_v, grid_v



    block_bulk = dim3(16,16,1)
    grid_bulk = dim3(int( nx/(block_bulk%x ) )+1, int(ny/(block_bulk%y))+1   , 1)

    call getv_gpu_kernel<<<grid_bulk,block_bulk>>>()
    ierr=cudaDeviceSynchronize()

    !print *, cudaGetErrorString(cudaGetLastError())
    block_h = dim3(32,1,1)
    grid_h = dim3(int( nx/(block_h%x ) )+1, 1   , 1)

    call v_horizontal_bc_gpu<<<grid_h,block_h>>>()
    ierr=cudaDeviceSynchronize()

    !print *, cudaGetErrorString(cudaGetLastError())

    block_v = dim3(32,1,1)
    grid_v = dim3(int( ny/(block_v%x ) )+1, 1   , 1)
    call v_vertical_bc_gpu<<<grid_v,block_v>>>()
    ierr=cudaDeviceSynchronize()

    !print *, cudaGetErrorString(cudaGetLastError())

    call maskv_gpu<<<grid_bulk,block_bulk>>>()
    ierr=cudaDeviceSynchronize()

    !print *, cudaGetErrorString(cudaGetLastError())

end subroutine

