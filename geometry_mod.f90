module geometry_mod
    use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
    implicit none 

    type geometry
        integer :: nx 
        integer :: ny
        integer :: nx_global
        integer :: ny_global
        real(dp) :: dx
        real(dp) :: dy
        integer :: up 
        integer :: down

    end type

    contains

    function create_geometry() result(geo)
        use parallel
        implicit none
        type(geometry) :: geo

        geo%nx=nx
        geo%ny=ny 
        geo%up=up
        geo%down=down
        geo%dx=dx
        geo%dy=dy
        

    end function        



end module
