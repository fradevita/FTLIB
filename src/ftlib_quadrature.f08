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

end module ftlib_quadrature