submodule (ftlib_quadrature) ftlib_quadrature_rectangle
    use ftlib_kinds
    use ftlib_function
    use ftlib_io
contains

    !===============================================================================================
    function rectangle_rule_a(x, f) result(integral)

        ! In/Out variables
        real(dp), intent(in) :: x(:)
        real(dp), intent(in) :: f(:)
        real(dp)             :: integral

        ! Local variables
        integer  :: N, i

        ! Check that size of x and f match
        if (size(x) .ne. (size(f) + 1)) then
            call print_error_message('ERROR: size of x must equal size of f + 1')
        endif

        N = size(f)
        integral = 0.0_dp
        do i = 1,N
            integral = integral + (x(i+1) - x(i))*f(i)
        end do

    end function rectangle_rule_a
    !===============================================================================================

    !===============================================================================================
    function rectangle_rule_f(x, f) result(integral)

        ! In/Out variables
        real(dp)           , intent(in) :: x(:)
        type(function_type), intent(in) :: f
        real(dp)                        :: integral

        ! Local variables
        integer  :: N, i

        ! Check that size of x and f match
        
        N = size(x)
        integral = 0.0_dp
        do i = 2,N
            integral = integral + (x(i) - x(i-1))*f%f([0.5_dp*(x(i) + x(i-1))], f%args)
        end do

    end function rectangle_rule_f
    !===============================================================================================

end submodule