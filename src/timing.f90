module timing_mod
    use mpi
    use iso_fortran_env, only: dp=> real64, sp=> real32
    implicit none

    type :: timing
        private

        real(kind=dp) :: tStart=0,tEnd=0, t=0

        character(len=20), public :: name="Unkown"
    contains
        procedure :: start, stop, time => getTime 
    end type
    private :: start,stop,getTime
    contains

    subroutine start(this)
        class(timing) :: this
        this%tStart=MPI_Wtime()
    end subroutine

    subroutine stop( this )
        class(timing) :: this
        this%tEnd=MPI_Wtime()
        this%t=this%t + this%tEnd - this%tStart
    end subroutine

    function getTime(this) result(t)
        class(timing) :: this
        real(kind=dp) :: t
        
        t = this%t

    end function



end module 

module timings_mod
    use timing_mod
    implicit none

    type :: timings
      type(timing) , allocatable :: timers( :)
      real*8, allocatable :: gathered_times(:)
      integer :: rank,nRanks, n
    contains
        procedure :: init,write,gather_times
    end type

    contains

    subroutine init(this,n)
        class(timings) :: this
        integer , intent(in) :: n 
        integer :: ierr

        this%n=n
        allocate(this%timers(1:n) )
        call MPI_Comm_rank( MPI_COMM_WORLD, this%rank, ierr)
        call MPI_Comm_size( MPI_COMM_WORLD, this%nRanks, ierr)
        
        if (this%rank == 0) then 
            allocate(this%gathered_times(1:this%nRanks*n)  )
        else
            allocate(this%gathered_times(1:this%nRanks) )
        endif

    end subroutine

    subroutine write(this,unit)
        class(timings) :: this
        integer,intent(in) :: unit
        integer:: rank=0,i=0,idx=0
        call this%gather_times()

        if (this%rank == 0) then
            do rank=1,this%nRanks
                do i=1,size(this%timers)
                    idx=idx+1
                    write(unit,*) i, rank, this%timers(i)%name,this%gathered_times(idx)
                end do
            end do
        endif 

    end subroutine


    subroutine gather_times(this)
        class(timings) :: this
        integer :: i,ierr
        do i=1,size(this%timers)
            this%gathered_times(i)=this%timers(i)%time()
        end do
        if (this%rank == 0) then 
            call MPI_Gather(MPI_IN_PLACE, this%n, MPI_DOUBLE_PRECISION, &
            this%gathered_times, this%n, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
        else
            call MPI_Gather(this%gathered_times, this%n, MPI_DOUBLE_PRECISION, &
                this%gathered_times, this%n, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)

        endif

    end subroutine





end module
