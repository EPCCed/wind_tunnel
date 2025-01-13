
module hello_c
    use:: ISO_C_BINDING, only : c_int,c_double,c_ptr

    interface

    subroutine say_hello( ) bind(C,name='say_hello')
    
      end subroutine

      subroutine get_array( psi, nx,ny ) bind(C,name="get_array")
        import c_int
        import c_double
        import c_ptr

        integer(C_INT) , intent(in), value :: nx,ny
        real(c_double), intent(inout) :: psi(nx,ny)
        

    end subroutine
      end interface
end module 


program test_c_fortran
    use iso_c_binding
    use hello_c
    real(8), allocatable, dimension(:,:) :: psi
    integer :: nx = 500, ny=500
    integer :: i , j 
    type(c_ptr) :: c_array

    print *, "Hello form Fortran!"

    allocate( psi(nx,ny) )


    do j=1,ny
        do i=1,nx
            psi(i,j)=i 
        end do
    end do


    call say_hello

   

    call get_array(psi, nx,ny)

    print * , psi(6,7)

   


    

end program
