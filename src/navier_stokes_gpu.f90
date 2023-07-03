
subroutine navier_stokes_gpu()
    use vars
    use cuda_kernels
    use mpi
    use cudafor
    implicit none

    type(dim3) nThreads,nBlocks
    integer :: iErr

    nThreads = dim3(16,16,1)
    nBlocks = dim3(int( nx/(nThreads%x ) )+1, int(ny/(nThreads%y))+1   , 1)


    call navier_stokes_dw_kernel<<<nBlocks,nThreads>>>()

    !print *, cudaGetErrorString(cudaGetLastError())

    call navier_stokes_vorticity_kernel<<<nBlocks,nThreads>>>()

    !print *, cudaGetErrorString(cudaGetLastError())
    ierr=cudaDeviceSynchronize()
    

    call haloswap_device(vort_dev)


end subroutine