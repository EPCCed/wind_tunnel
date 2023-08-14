module timing_mod
    use mpi
    implicit none

    type :: timing
        private

        real :: tStart=0,tEnd=0, t=0

        character(len=20), public :: name
    contains
        procedure :: start, stop, time 
    end type
    private :: start,stop,time
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

    function time(this) result(t)
        class(timing) :: this
        real :: t
        
        t = this%t

    end function



end module 
