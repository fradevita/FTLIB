!> Define abstract procedure function and type function
module ftlib_function

    use ftlib_kinds, only : dp

    implicit none

    abstract interface
        function function_procedure(x, args) result(f)
            use ftlib_kinds, only : dp
            real(dp), intent(in)           :: x(:)    !< function variables
            real(dp), intent(in), optional :: args(:) !< function (optional) arguments
            real(dp)                       :: f       !< function value
        end function function_procedure
    end interface

    type function_type
        real(dp), dimension(:), allocatable :: args         !< function arguments
        procedure(function_procedure), pointer, nopass :: f !< pointer to the function
    end type function_type

end module ftlib_function