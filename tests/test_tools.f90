module test_tools
    
    contains

    subroutine check( message, condition)
        logical, intent(in) :: condition
        character(len=*) :: message
        character(len=40) :: color

        if ( condition) then
            color='\033[32m'
        else
            color='\033[0;31m'
        endif 

        print * , trim(color) //trim(message)  , condition, '\033[0m'

        if ( .not. condition) call exit(1)
        
    end subroutine

end module