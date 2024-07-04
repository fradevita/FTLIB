program test

    use ftlib_kinds
    use ftlib_root

    implicit none

    ! Parameters
    real(dp), parameter :: a = 1.0_dp
    real(dp), parameter :: b = 2.0_dp
    real(dp), parameter :: tol = 1.0e-12_dp
    integer , parameter :: max_iter = 100

    ! Variable
    real(dp)                 :: x0
    class(root), allocatable :: root_solver

    !***********************************************************************************************
    print *, '**** Using bisection method ****'
    ! Setup the bisection solver
    allocate(bisection::root_solver) ! now root_solver is of dynamic type bisection
    call root_solver%setup(a, b, tol, max_iter, my_function)

    ! Find the root with the bisection solver
    x0 = root_solver%find_root()
    print *, 'Root is  x0 =', x0
    deallocate(root_solver)
    !***********************************************************************************************
 
    !***********************************************************************************************
    print *, '**** Using secant method ****'
    ! Setup the secant solver
    allocate(secant::root_solver) ! now root_solver is of dynamic type secant
    call root_solver%setup(a, b, tol, max_iter, my_function)

    ! Find the root with the secant solver
    x0 = root_solver%find_root()
    print *, 'Root is  x0 =', x0
    deallocate(root_solver)
    !***********************************************************************************************

    !***********************************************************************************************
    print *, '**** Using newton raphson method ****'
    ! Setup the newton-raphson solver
    allocate(newton_raphson::root_solver) ! now root_solver is of dynamic type newton_raphson
    call root_solver%setup(a, b, tol, max_iter, my_function, my_derivative)

    ! Find the root with the newton-raphson solver
    x0 = root_solver%find_root()
    print *, 'Root is  x0 =', x0
    deallocate(root_solver)
    !***********************************************************************************************

contains

    !===============================================================================================
    function my_function(x, args) result(f)

        real(dp), intent(in)           :: x(:)
        real(dp), intent(in), optional :: args(:)
        real(dp)                       :: f

        f = x(1)**3 - x(1) - 2.0d0

    end function my_function
    !===============================================================================================

    !===============================================================================================
    function my_derivative(x, args) result(f)

        real(dp), intent(in) :: x(:)
        real(dp), intent(in), optional :: args(:)
        real(dp)             :: f

        f = 3*x(1)**2 - 1.0d0

    end function my_derivative
    !===============================================================================================

end program test
