module timing_cfd
    use mpi
    use timings_mod
    implicit none
    
    type(timings) :: CFDTimers

    contains

    subroutine initCFDTimers()
    
        call CFDTimers%init(4)
        CFDTimers%timers(1)%name="poisson_kernel"
        CFDTimers%timers(2)%name="halo_swap"
        CFDTimers%timers(3)%name="fill_boundary"
        CFDTimers%timers(4)%name="full"

    end subroutine

end module