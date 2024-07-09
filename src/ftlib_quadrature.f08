module ftlib_quadrature

    use ftlib_kinds
    use ftlib_function

    implicit none

    interface rectangle_rule
        module procedure rectangle_rule_f
        module procedure rectangle_rule_a
    end interface

    interface trapezoidal_rule
        module procedure trapezoidal_rule_f
        module procedure trapezoidal_rule_a
    end interface

    interface gaussian_quadrature
        module procedure gaussian_quadrature_1d
        module procedure gaussian_quadrature_nd
    end interface

    interface
        module function rectangle_rule_f(x, f) result(integral)
            real(dp)           , intent(in) :: x(:)
            type(function_type), intent(in) :: f
            real(dp)                        :: integral
        end function rectangle_rule_f
        module function rectangle_rule_a(x, f) result(integral)
            real(dp), intent(in) :: x(:)
            real(dp), intent(in) :: f(:)
            real(dp)             :: integral
        end function rectangle_rule_a
        module function trapezoidal_rule_f(x, f) result(integral)
            real(dp)           , intent(in) :: x(:)
            type(function_type), intent(in) :: f
            real(dp)                        :: integral
        end function trapezoidal_rule_f
        module function trapezoidal_rule_a(x, f) result(integral)
            real(dp), intent(in) :: x(:)
            real(dp), intent(in) :: f(:)
            real(dp)             :: integral
        end function trapezoidal_rule_a
        module function gaussian_quadrature_1d(x, f, N) result(integral)
            real(dp)           , intent(in) :: x(:)
            type(function_type), intent(in) :: f
            integer            , intent(in) :: N
            real(dp)                        :: integral
        end function gaussian_quadrature_1d
        module function gaussian_quadrature_nd(x, f, N) result(integral)
            real(dp)           , intent(in) :: x(:,:)
            type(function_type), intent(in) :: f
            integer            , intent(in) :: N
            real(dp)                        :: integral
        end function gaussian_quadrature_nd
    end interface

contains
    !===============================================================================================
    subroutine get_x_and_w(x, w, N)

        ! In/Out variables
        integer , intent(in ) :: N
        real(dp), intent(out) :: x(N), w(N)

        select case(N)
        case(1)
            x = 0.0_dp
            w = 2.0_dp
        case(2)
            x = [-1.0_dp/sqrt(3.0_dp), 1.0_dp/sqrt(3.0_dp)]
            w = [1.0_dp, 1.0_dp]
        case(3)
            x = [-sqrt(3.0_dp/5.0_dp), 0.0_dp, sqrt(3.0_dp/5.0_dp)]
            w = [5.0_dp/9.0_dp, 8.0_dp/9.0_dp, 5.0_dp/9.0_dp]
        case(4)
            x = [-sqrt(3.0_dp/7.0_dp + 2.0_dp/7.0_dp*sqrt(6.0_dp/5.0_dp)), &
                 -sqrt(3.0_dp/7.0_dp - 2.0_dp/7.0_dp*sqrt(6.0_dp/5.0_dp)), &
                  sqrt(3.0_dp/7.0_dp - 2.0_dp/7.0_dp*sqrt(6.0_dp/5.0_dp)), &
                  sqrt(3.0_dp/7.0_dp + 2.0_dp/7.0_dp*sqrt(6.0_dp/5.0_dp))]
            w = [(18_dp - sqrt(30.0_dp))/36.0_dp, &
                 (18_dp + sqrt(30.0_dp))/36.0_dp, &
                 (18_dp + sqrt(30.0_dp))/36.0_dp, &
                 (18_dp - sqrt(30.0_dp))/36.0_dp]
        case(5)
            x = [-1.0_dp/3.0_dp*sqrt(5.0_dp + 2.0_dp*sqrt(10.0_dp/7.0_dp)), &
                 -1.0_dp/3.0_dp*sqrt(5.0_dp - 2.0_dp*sqrt(10.0_dp/7.0_dp)), &
                  0.0_dp, &
                  1.0_dp/3.0_dp*sqrt(5.0_dp - 2.0_dp*sqrt(10.0_dp/7.0_dp)), &
                  1.0_dp/3.0_dp*sqrt(5.0_dp + 2.0_dp*sqrt(10.0_dp/7.0_dp))]
            w = [(322.0_dp - 13.0_dp*sqrt(70.0_dp))/900.0_dp, &
                 (322.0_dp + 13.0_dp*sqrt(70.0_dp))/900.0_dp, &
                 128.0_dp/225.0_dp, &
                 (322.0_dp + 13.0_dp*sqrt(70.0_dp))/900.0_dp, &
                 (322.0_dp - 13.0_dp*sqrt(70.0_dp))/900.0_dp]
        end select

    end subroutine get_x_and_w
    !===============================================================================================

end module ftlib_quadrature