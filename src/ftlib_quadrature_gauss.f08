submodule (ftlib_quadrature) ftlib_quadrature_rectangle
    use ftlib_kinds
    use ftlib_function
    use ftlib_io
contains

    !===============================================================================================
    function gaussian_quadrature(x, f, N) result(integral)

        ! In/Out variables
        real(dp)           , intent(in) :: x(:)
        type(function_type), intent(in) :: f
        integer            , intent(in) :: N
        real(dp)                        :: integral

        ! Local variables
        integer  :: i
        real(dp) :: c1, c2, xg(N), wg(N)

        call get_x_and_w(xg, wg, N)
        c1 = 0.5_dp*(x(2) - x(1))
        c2 = 0.5_dp*(x(2) + x(1))
        integral = 0.0_dp
        do i = 1,N
            integral = integral + wg(i)*f%f([c1*xg(i) + c2], f%args)
        end do
        integral = integral*c1

    end function gaussian_quadrature
    !===============================================================================================

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

end submodule