module ftlib_utils

    use ftlib_kinds, only : dp

contains

    !===============================================================================================
    pure function solve_quadratic(a, b, c) result(x)

        ! Find root of quadratic equation
        real(dp), intent(in) :: a, b, c
        real(dp) :: x1, x2, x

        x1 = (-b + dsqrt(b**2 - 4.0_dp*a*c))/(2.0_dp*a)
        x2 = (-b - dsqrt(b**2 - 4.0_dp*a*c))/(2.0_dp*a)

        x = max(x1,x2)

    end function solve_quadratic
    !===============================================================================================

end module ftlib_utils