subroutine setup()
!Sets up the global arrays, defines the shape of the object, and distributes the global
!arrays to the processes.

    use vars
    use parallel
    
    implicit none
    
    integer :: i,j
    
    real(8) :: ysum, yhat
    integer :: nums
    
    
    call setup_mesh_spacing()
    call set_cartesian_communicator()
    
    ! report if running on CPU or on GPU
    if (irank .eq. 0) then

        if (device .eqv. .true.) then
            print *, "Using the GPU device"
        else
            print *, "Using the CPU device"
            
        endif 
        
        print *, "mesh size: (",nx_global,ny_global,")"

    endif 

    
    

    !main process sets up global arrays
    if (irank .eq. 0) then  
    
        !allocate global arrays
        call setup_global_arrays()
        
        
        
        ! generate x and y coordinates
        
        do i=1,nx_global
            x_global(i) = xrange(1) + (i-1)*dxx
        enddo
        
        do j=1,ny_global
            y_global(j) = yrange(1) + (j-1)*dyy
        enddo
        
        
        
        ! define shape of object. Save it to mask (1=object, 0 = not object)
        
        mask_global(:,:)=0
        
        do j=1,ny_global
            do i=1,nx_global
                if (shape .eq. 1) then
                    mask_global(i,j)=ellipse(x_global(i),y_global(j))
                else
                    mask_global(i,j)=aerofoil(x_global(i),y_global(j))
                endif
            enddo
        enddo
        
        
        !set up boundary array. Points on the edge of the object are set to ±1
        !(depending on whether they are on the top (+1) or bottom (-1) of the object)
        
        boundary_global(:,:)=0
        
        do i=1,nx_global
            !bottom boundary
            do j=1,ny_global
            if (mask_global(i,j) .eq. 1) then
                boundary_global(i,j) = boundary_global(i,j) - 1
                exit
                endif
            enddo
            !top boundary (don't do if the top boundary isn't found)
            if (j .ne. ny_global) then
                do j=ny_global,1,-1
                    if (mask_global(i,j) .eq. 1) then
                        boundary_global(i,j) = boundary_global(i,j) + 1
                        exit
                    endif
                enddo
            endif
        enddo
        
        
        !calculate the value of psi in the object
        
        yhat=0. !yhat is the value of psi in the object
        
        if (nose .eq. 1) then !We want the flow to diverge at the first part of the object it reaches
        
            ysum=0.
            nums=0
            

            do i=1,nx_global !skim along in the x direction looking for the first occurrance of the object
                do j=1,ny_global
                    if (mask_global(i,j) .eq. 1) then !found it! sum up all instances of it crossing this x=const line
                        nums=nums+1
                        ysum=ysum+real(j)
                    endif
                enddo
            
                if (nums .gt. 0) then !if we found the nose
                    yhat=ysum/real(nums) !find the mean location of the nose in the y coordinate
                    exit !we are done here. Leave the loop
                endif
            enddo
        
        else if (nose .eq. 0) then
        
            yhat=real((ny_global-1))/2+1 !set yhat to the centre value of psi
    
        else 
        
            print *, "Error - invalid option (nose)"
            call MPI_Abort(comm,-1,ierr)
            stop
            
        endif
                    
        
        
        
        ! now set the initial condition for psi, psi(x,y) = vx * y (unless in the object where its constant)
        
        do j=1,ny_global
            do i=1,nx_global
                if (mask_global(i,j) .eq. 1) then
                    psi_global(i,j)=yhat
                else
                    psi_global(i,j)=real(j)
                endif
            enddo
        enddo
        

        
        ! initialise vorticity to zero
        vort_global(:,:) = 0
        
    endif
    
    
    
    !allocate local arrays
    call setup_local_arrays()
   
    !farm out global arrays to processes
    call distribute_data()
 
    !fill in halos for psi
    call haloswap(psi)

    !boundary conditions for psi:
    !top/bottom
    if (up .eq. MPI_PROC_NULL) psi(:,ny+1) = psi(:,ny)+1
    if (down .eq. MPI_PROC_NULL) psi(:,0) = psi(:,1)-1
    
    !left/right
    psi(0,:) = psi(1,:)
    psi(nx+1,:) = 2.*psi(nx,:) - psi(nx-1,:)
    
    call set_up_vorticity()

end subroutine

subroutine set_up_vorticity()
    use vars
    implicit none 
    
    if (vorticity) then 
        cfl_r0 = 0.25*dx*dy*R0
        cfl_v = 0.25*dx/5. !assume velocty is 5

        !dt is set according to the most restrictive CFL condition
        dt = minval((/ cfl_r0, cfl_v /))
    endif

end subroutine

subroutine setup_mesh_spacing()
    use vars

    dxx = (xrange(2)-xrange(1))/(real(nx_global)-1)
    dyy = (yrange(2)-yrange(1))/(real(ny_global)-1)

end subroutine