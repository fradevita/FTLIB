submodule (ftlib_quadrature) ftlib_quadrature_gauss
    use ftlib_kinds   , only : dp
    use ftlib_function, only : function_type
    use ftlib_io      , only : print_error_message
contains

    !===============================================================================================
    function gaussian_quadrature_1d(x, f, N) result(integral)

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

    end function gaussian_quadrature_1d
    !===============================================================================================

    !===============================================================================================
    function gaussian_quadrature_nd(x, f, N) result(integral)

        ! In/Out variables
        real(dp)           , intent(in) :: x(:,:)
        type(function_type), intent(in) :: f
        integer            , intent(in) :: N
        real(dp)                        :: integral

        ! Local variables
        integer  :: ndim, dir
        real(dp) :: xg(N), wg(N)
        real(dp), allocatable :: c1(:), c2(:), xsi(:)

        ! Get points and weights in in the interval [-1, 1]
        call get_x_and_w(xg, wg, N)

        ! Adjust for actual domain of integration
        ndim = size(x(:,1))
        allocate(c1(ndim))
        allocate(c2(ndim))
        do dir = 1,ndim
            c1(dir) = 0.5_dp*(x(dir,2) - x(dir,1))
            c2(dir) = 0.5_dp*(x(dir,2) + x(dir,1))
        end do

        allocate(xsi(ndim))
        select case(ndim)
        case(2)
            integral = 0.0_dp
            block
                integer :: i, j
                do j = 1,N
                    do i = 1,N
                        xsi = [c1(1)*xg(i) + c2(1), c1(2)*xg(j) + c2(2)]
                        integral = integral + wg(i)*wg(j)*f%f(xsi, f%args)
                    end do
                end do
            end block
            integral = integral*c1(1)*c1(2)
        case(3)
            integral = 0.0_dp
            block
                integer :: i, j, k
                do k = 1,N
                    do j = 1,N
                        do i = 1,N
                            xsi = [c1(1)*xg(i) + c2(1), c1(2)*xg(j) + c2(2), c1(3)*xg(k) + c2(3)]
                            integral = integral + wg(i)*wg(j)*wg(k)*f%f(xsi, f%args)
                        end do
                    end do
                end do
            end block
            integral = integral*c1(1)*c1(2)*c1(3)
        end select
        
    end function gaussian_quadrature_nd
    !===============================================================================================

end submodule
