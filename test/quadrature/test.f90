program test

    use ftlib_kinds
    use ftlib_quadrature

    implicit none

    real(dp), parameter :: L = 3.0_dp

    integer               :: i, N
    real(dp)              :: dx, solution
    real(dp), allocatable :: x(:), f(:), fn(:)
    type(function_type)   :: fp

    ! Analytical solution
    solution = test_function_integral([L]) - test_function_integral([0.0_dp])

    ! First test rectangle and trapezoidal rules
    print *, '**** Using rectangle and trapezoidal rules ****'
    do N = 10,50,5
        dx = L/real(N-1,dp)
        
        allocate(x(N))
        allocate(f(N-1))
        allocate(fn(N))

        do i = 1,N
            x(i) = (i-1)*dx
            fn(i) = test_function([x(i)]) 
        end do
        do i = 1,N-1
            f(i) = test_function([0.5_dp*(x(i+1) + x(i))])
        end do

        print *, N, abs(rectangle_rule(x, f) - solution)/abs(solution), &
                    abs(trapezoidal_rule(x, fn) - solution)/abs(solution)
    
        deallocate(x)
        deallocate(f)
        deallocate(fn)
    end do

    ! Next test the gaussian quadrature
    print *, '**** Using gaussian quadrature ****'
    fp%f => test_function
    do N = 2,5
        print *, N, abs(gaussian_quadrature([0.0_dp, L], fp, N) - solution)/abs(solution)
    end do

contains

    !===============================================================================================
    function test_function(x, args) result(f)

        real(dp), intent(in)           :: x(:)
        real(dp), intent(in), optional :: args(:)
        real(dp)                       :: f

        f = 2.0_dp - sin(x(1)) + cos(x(1))*2.0_dp

    end function test_function
    !===============================================================================================

    !===============================================================================================
    function test_function_integral(x, args) result(f)

        real(dp), intent(in)           :: x(:)
        real(dp), intent(in), optional :: args(:)
        real(dp)                       :: f

        f = 2.0_dp*x(1) + cos((x(1))) + sin(x(1))*2.0_dp

    end function test_function_integral
    !===============================================================================================

end program test