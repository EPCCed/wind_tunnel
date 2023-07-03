module cuda_kernels
    use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
    implicit none

    integer, device :: nx_dev , ny_dev 
    real,device  :: dx_dev, dy_dev
    integer, device :: up_dev,down_dev

    real, device :: dt_dev, maxvort_dev,vmax_dev , r0_dev

    real, allocatable, dimension(:,:), device :: psi_dev, vort_dev, u_dev, v_dev, mask_dev, dw_dev

    contains


    attributes(global) subroutine getv_gpu_kernel()
      integer i , j
      i = (blockIdx%x - 1 ) * blockDim%x + threadIdx%x 
      j = (blockIdx%y - 1) * blockDim%y + threadIdx%y

  

      if ( (i  .gt. nx_dev) .or. (j .gt. ny_dev)) return

      if (j .gt. 1 .or. j .lt. ny_dev) u_dev(i,j) = (psi_dev(i,j+1)-psi_dev(i,j-1))/2./dy_dev
      
      if (i .gt. 1 .or. i .lt. nx_dev) v_dev(i,j) = -(psi_dev(i+1,j)-psi_dev(i-1,j))/2./dx_dev


    end subroutine

    attributes(global) subroutine maskv_gpu()
      integer :: i , j
      i = (blockIdx%x - 1 ) * blockDim%x + threadIdx%x
      j = (blockIdx%y - 1) * blockDim%y + threadIdx%y

      if ( i  .gt. nx_dev .or. j .gt. ny_dev) return 
      
      u_dev(i,j)=u_dev(i,j)*(1-mask_dev(i,j) )
      v_dev(i,j)=v_dev(i,j)*(1-mask_dev(i,j) )

    end subroutine

    attributes(global) subroutine v_horizontal_bc_gpu()
    use mpi
      integer :: i , j
      i = (blockIdx%x - 1 ) * blockDim%x + threadIdx%x
      
      if ( i  .gt. nx_dev ) return 


      if (down_dev .eq. MPI_PROC_NULL) u_dev(i,1) = u_dev(i,2)
      if (up_dev .eq. MPI_PROC_NULL) u_dev(i,ny_dev)=u_dev(i,ny_dev-1)

    end subroutine

    attributes(global) subroutine v_vertical_bc_gpu()
    use mpi
      integer :: j
      j = (blockIdx%x - 1 ) * blockDim%x + threadIdx%x
      if (  j .gt. ny_dev) return 
      
      v_dev(1,j) = v_dev(2,j)
      v_dev(nx_dev,j)=v_dev(nx_dev-1,j)

    end subroutine

    attributes(global) subroutine navier_stokes_dw_kernel()
    implicit none  
    integer :: i , j
    real :: uu,vv,v2,dw=0,dwdx,deltay,dwdy,deltax,r


    i = (blockIdx%x - 1 ) * blockDim%x + threadIdx%x 
    j = (blockIdx%y - 1) * blockDim%y + threadIdx%y

    if ((i .gt. nx_dev) .or. (j .gt. ny_dev)) return 
    if (mask_dev(i,j) .eq. 0) then
      !advection
      

      uu=u_dev(i,j)
      vv=v_dev(i,j)
      v2=uu*uu+vv*vv

      if (v2 .gt. vmax_dev) then
          v2 = sqrt(vmax_dev/v2)
          uu=uu*v2
          vv=vv*v2
      endif

      deltax = uu*dt_dev/dx_dev
      dwdx = (1-deltax)*(vort_dev(i+1,j)-vort_dev(i,j)) + (1+deltax)*(vort_dev(i,j)-vort_dev(i-1,j))


      deltay = vv*dt_dev/dy_dev
      dwdy = (1-deltay)*(vort_dev(i,j+1)-vort_dev(i,j)) + (1+deltay)*(vort_dev(i,j)-vort_dev(i,j-1))

      dw = 0.5*(-uu*dwdx - vv*dwdy)

      r=r0_dev

      dw= dw + 1./r *( (vort_dev(i+1,j) - 2.*vort_dev(i,j) + vort_dev(i-1,j) )/dx_dev/dx_dev &
          + (vort_dev(i,j+1) - 2.*vort_dev(i,j) + vort_dev(i,j-1))/dy_dev/dy_dev )
      !endif

      dw_dev(i,j)=dw

      !vort_dev(i,j)=vort_dev(i,j) + dw*dt_dev

    endif
      


      end subroutine
    

    
    attributes(global) subroutine navier_stokes_vorticity_kernel()
      implicit none
      integer :: i , j


      i = (blockIdx%x - 1 ) * blockDim%x + threadIdx%x 
      j = (blockIdx%y - 1) * blockDim%y + threadIdx%y

      if (i == 0 ) print *, "i", i
      if ((i .gt. nx_dev) .or. (j .gt. ny_dev)) return 

      vort_dev(i,j)=vort_dev(i,j) + dw_dev(i,j)*dt_dev  
      if ( vort_dev(i,j) .gt. maxvort_dev) vort_dev(i,j) = maxvort_dev
      if  (vort_dev(i,j) .lt. -maxvort_dev ) vort_dev(i,j) = -maxvort_dev

      end subroutine





      attributes(global) subroutine poisson_solver_cuda( offset , w )
      implicit none


      integer, value :: offset
      real , value :: w

      real :: dx,dy 

      integer :: i,j,nx,ny

      dx=dx_dev
      dy=dy_dev

      nx=nx_dev
      ny=ny_dev


      i = (blockIdx%x - 1 ) * blockDim%x + threadIdx%x
      j = (blockIdx%y - 1) * blockDim%y + threadIdx%y 

      ! ! ensure that the thread is not out of the vector boundary 
       if (  (i .gt. nx ) .or. ( j .gt.  ny )  ) return

       if (mask_dev(i,j) .eq. 0) then
        if (mod(i+j,2) .eq. offset ) then
           psi_dev(i,j) = (1-w)*psi_dev(i,j) +w*( (psi_dev(i-1,j) + psi_dev(i+1,j))*dy*dy &
           + (psi_dev(i,j-1)+psi_dev(i,j+1))*dx*dx +dx*dx*dy*dy*vort_dev(i,j) ) &
           /2./(dx*dx+dy*dy)
        endif
      endif 

      !endif 
    end subroutine

    attributes(global) subroutine fill_right_boundary_kernel(psi_dev)
      real, dimension( 0:nx_dev+1,0:ny_dev+1) , intent(inout) :: psi_dev

      integer :: j

      j = (blockIdx%x - 1 ) * blockDim%x + threadIdx%x
      
      ! ! ensure that the thread is not out of the vector boundary 
      if (  (j .gt. ny_dev )  ) return

      psi_dev(nx_dev+1,j) = 2.*psi_dev(nx_dev,j) - psi_dev(nx_dev-1,j)

    end subroutine


    attributes(global) subroutine fill_vertical_boundary_kernel(psi_dev)
      USE MPI

      real, dimension( 0:nx_dev+1,0:ny_dev+1) , intent(inout) :: psi_dev

      integer :: i

      i = (blockIdx%x - 1 ) * blockDim%x + threadIdx%x - 1
      
      ! ! ensure that the thread is not out of the vector boundary 
      if ( i  .gt. (nx_dev + 1) )   return


      if ( down_dev .eq. MPI_PROC_NULL ) psi_dev(i,0) = 2* psi_dev(i,1) - psi_dev(i,2)
      if ( up_dev .eq. MPI_PROC_NULL) psi_dev(i,ny_dev+1) = 2*psi_dev(i,ny_dev) - psi_dev(i,ny_dev-1)


      
      
    end subroutine





  
  
  end module




  subroutine haloswap_device(array)
    ! swaps the halo values for 'array'
        use MPI
        use parallel
        use vars
        use cudafor
        implicit none
        real , intent(inout), device:: array(0:nx+1,0:ny+1)


        !send top, recieve bottom
        call MPI_Sendrecv(array(:,ny),nx+2,MPI_REAL,up,1,array(:,0),nx+2,MPI_REAL,down,1,comm,status,ierr)

        !send bottom, receive top
        call MPI_Sendrecv(array(:,1),nx+2,MPI_REAL,down,0,array(:,ny+1),nx+2,MPI_REAL,up,0,comm,status,ierr)

    end subroutine










module poisson_solver_cuda_mod
  use cudafor
  use cuda_kernels
  use mpi
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none



  contains


  subroutine fill_right_boundary_gpu(psi_dev)
    use vars
    real, dimension( 0:nx+1,0:ny+1) , intent(inout), device :: psi_dev
    type(dim3) :: block, grid


    block = dim3(32,1,1)
    grid = dim3(int( (ny)/(block%x ) )+1, 1  , 1)


    call fill_right_boundary_kernel<<<grid, block>>>(psi_dev )

  end subroutine

  subroutine fill_vertical_boundary_gpu(psi_dev)
    use vars
    real, dimension( 0:nx+1,0:ny+1) , intent(inout), device :: psi_dev
    type(dim3) :: block, grid

    block = dim3(32,1,1)
    grid = dim3(int( (nx+2)/(block%x ) )+1, 1  , 1)

    call fill_vertical_boundary_kernel<<<grid, block>>>(psi_dev )

  end subroutine


  subroutine poisson_gpu( n)
    use vars
    use parallel
    implicit none

    integer,intent(in) :: n
    real :: w 
    integer :: it
    integer :: istat
    
    type(dim3) block, grid 

    
    block = dim3(16,16,1)
    grid = dim3(int( nx/(block%x ) )+1, int(ny/(block%y))+1   , 1)
    
    w=1./(1+pi/nx_global) !optimal value of w
        
    do it=1,n 
      call poisson_solver_cuda<<<grid, block>>>(0, w )
      ierr=cudaDeviceSynchronize()
      call haloswap_device(psi_dev)
      !istat=cudaDeviceSynchronize()
      ierr=cudaDeviceSynchronize()

      call poisson_solver_cuda<<<grid, block>>>(1, w )

      ierr=cudaDeviceSynchronize()
      call fill_right_boundary_gpu(psi_dev)
      ierr=cudaDeviceSynchronize()

      call haloswap_device(psi_dev)
      ierr=cudaDeviceSynchronize()

      call fill_vertical_boundary_gpu(psi_dev)

      ierr=cudaDeviceSynchronize()

      
      !istat=cudaDeviceSynchronize()


      
    enddo



  end subroutine 

end module
