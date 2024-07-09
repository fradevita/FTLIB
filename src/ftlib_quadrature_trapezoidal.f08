submodule (ftlib_quadrature) ftlib_quadrature_trapezoidal
    use ftlib_kinds
    use ftlib_function
    use ftlib_io
contains

    !===============================================================================================
    function trapezoidal_rule_a(x, f) result(integral)

        ! In/Out variables
        real(dp), intent(in) :: x(:)
        real(dp), intent(in) :: f(:)
        real(dp)             :: integral

        ! Local variables
        integer  :: N, i

        ! Check that size of x and f match
        if (size(x) .ne. size(f)) then
            call print_error_message('ERROR: size of x must equal size of f')
        endif

        N = size(f)
        integral = 0.0_dp
        do i = 1,N-1
            integral = integral + (x(i+1) - x(i))*(f(i) + f(i+1))
        end do
        integral = integral*0.5_dp

    end function trapezoidal_rule_a
    !===============================================================================================

end submodule
