subroutine solver()
!determines the flow profile around the object. In order to do this we have to solve 2
!coupled equations:
! - calculate the streamfunction (psi) by solving Poisson's equation
! - update the vorticity by solving the vorticity transport equation (from the NS equations)
!
!First of all a potential flow is calculated around the object (solving Laplace's equation)
!then (optionally) the vorticity is turned on and alternately Poisson's equation and the
!vorticity transport equation are iterated to determine the fluid flow.

    use vars
    use parallel
#ifdef USE_CUDA
    use cuda_kernels
    use cudafor
#endif

    implicit none

    real :: time
    double precision :: tstart=0, tstop=0

    !$OMP PARALLEL
    !solve Laplace's equation for the irrotational flow profile (vorticity=0)
    call poisson(5000)
    !$OMP END PARALLEL



    !get the vorticity on the surface of the object
    ! ierr=cudaDeviceSynchronize()
    call getvort_cpu()
    
#ifdef USE_CUDA
    if (device == .true.) vort_dev=vort

    call fieldsFromGpuToCpu()

    psi_dev=psi
    vort_dev=vort
    v_dev=v 
    u_dev=u
    dw=0
    dw_dev=dw

#endif

    ! !write out the potential flow to file.

    call writetofile("potential.dat")
    
    
    ! !if the user has chosen to allow vorticity:
     if (vorticity) then


         if (irank .eq. 0) then
             print*,''
             print*,"CFL (Reynolds, velocity)", cfl_r0, cfl_v
             print *, "Timestep=",dt
             print *, ""
         endif

         time=0.

         if (irank .eq. 0) tstart=MPI_Wtime()


    !$OMP PARALLEL
    
     do while (time .lt. real(nx)*crossing_times)
              !if (irank .eq. 0) print*, "t=",time,"of",nx*crossing_times

              call getv() !get the velocity
    ! !         ierr=cudaDeviceSynchronize()

              call navier_stokes() !solve dw/dt=f(w,psi) for a timestep
    ! !         ierr=cudaDeviceSynchronize()

              call poisson(2) !2 poisson 'relaxation' steps
    ! !         ierr=cudaDeviceSynchronize()

              !$OMP SINGLE
              time=time+dt
              !$OMP END SINGLE
              !$OMP BARRIER
    enddo

        !$OMP END PARALLEL

         if (irank .eq. 0) then
             tstop=MPI_Wtime()
             print*,''
             print*, "Time to complete =",tstop-tstart
             print*,''
         endif

#ifdef USE_CUDA
    call fieldsFromGpuToCpu()
    ierr=cudaDeviceSynchronize()
#endif

    call getv_cpu()
    call getvort_cpu()


    endif


end subroutine
