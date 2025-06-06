subroutine get_params()
    use vars
    use parallel

    implicit none
    integer :: io


    if (irank .eq. 0) then
        open(unit=10,file='config.txt',IOSTAT=io,STATUS='old')
        if (io .eq. 0) then

            read(10,nml=shapeparams,IOSTAT=io)
            if (io .eq. 0) then
                print*, "Read in shape parameters"
                !write(*,nml=shapeparams)
            else
                print*, "WARNING: No shape parameters defined. Using defaults"
            endif

            rewind (unit=10)

            read(10,nml=mediumparams,IOSTAT=io)
            if (io .eq. 0) then
                print*, "Read in medium parameters"
            else
                print*, "WARNING: No medium parameters defined. Using defaults"
            endif

            rewind (unit=10)

            read(10,nml=vortparams,IOSTAT=io)
            if (io .eq. 0) then
                print*, "Read in vorticity parameters"
            else
                print*, "WARNING: No vorticity parameters defined. Using defaults"
            endif


            read(10,nml=gpuparams,IOSTAT=io)
#ifdef USE_CUDA
            if (io .eq. 0) then
                print*, "Read in GPU parameters"
            else
                print*, "WARNING: No GPU parameters defined. Using defaults"
            endif
#else
            if (io .ne. 0) then
                print*, "WARNING: GPU support is not enabled. GPU parameters will be ignored."
            endif
#endif

        else
            print*, "WARNING: No parameter file found. Using default parameters."
        endif
        close(10)
    endif

    ! Anything read in from the configuration file needs 
    ! to be broadcast to all the other processes.

    ! Broadcast shape parameters to all other processes
    call MPI_Bcast(shape,1,MPI_INTEGER,0,comm,ierr)
    call MPI_Bcast(alpha,1,MPI_REAL,0,comm,ierr)
    call MPI_Bcast(m,1,MPI_REAL,0,comm,ierr)
    call MPI_Bcast(t,1,MPI_REAL,0,comm,ierr)
    call MPI_Bcast(nx_global,1,MPI_INTEGER,0,comm,ierr)
    call MPI_Bcast(ny_global,1,MPI_INTEGER,0,comm,ierr)

    ! Broadcastvorticity parameters to allother processes
    call MPI_Bcast(vorticity,1,MPI_LOGICAL,0,comm,ierr)
    !call MPI_Bcast(vortcoeff,1,MPI_REAL,0,comm,ierr)
    !call MPI_Bcast(maxvort,1,MPI_REAL,0,comm,ierr)
    !call MPI_Bcast(r0,1,MPI_REAL,0,comm,ierr)

    ! Broadcast GPU params to all other processes
    call MPI_Bcast(device,1,MPI_LOGICAL,0,comm,ierr)

    


    !  call MPI_Finalize(ierr)
    !  stop






end subroutine
