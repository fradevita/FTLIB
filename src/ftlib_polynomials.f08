module ftlib_polynomials

    use ftlib_kinds , only : dp
    use ftlib_grid
    use ftlib_io
    use ftlib_function

    implicit none

    ! Matrix coefficient for the reconstruction of the surface polynomial.
    ! The system is (in 3D):
    ! [XYZ] [a_p] = [b]
    ! where [XYZ] is the Vandermonde matrix, a the coefficients to be determined
    ! and b the interpolated field. Setting A = [XYZ] and x = [a_p] the system is
    ! transformed as
    ! AT*A*x = AT*b
    ! x = inv_(AT*A) * AT * b
    ! a_p is the solution array x
    real(dp), dimension(:,:), allocatable :: inv_ATAxAT

    ! The matrix A has size mxn which depends on the polynomial order
    integer :: m, n

    ! Pointer to the polynomial function
    procedure(function_procedure), pointer :: P => null()

contains

    !===============================================================================================
    subroutine init_polynomial_reconstruction(order, delta)

        ! In/Out variables
        integer , intent(in) :: order
        real(dp), intent(in) :: delta
        
        ! Local variables
        integer                               :: info, lwork
        real(dp)                              :: work_dimension(1)
        integer , dimension(:)  , allocatable :: ipiv
        real(dp), dimension(:)  , allocatable :: work
        real(dp), dimension(:,:), allocatable :: A, AT, inv_ATA

        ! For the moment the reconstruction works only for equispaced grid
        ! if (G%dx .ne. G%dy) then
        !    call print_error_message('ERROR: polynomial reconstuction works only for dx == dy.')
        ! endif

        ! Select problem size
        m = 9
        if (order == 1) then
            n = 3
            P => P1st
        else if (order == 2) then
            n = 6
            P => P2nd
        endif

        ! Compute matrix coefficient
        allocate(A(m,n))
        block
            integer  :: i, j, row
            real(dp) :: x, y
            row = 1
            do j = -1,1
                y = delta*real(j, dp)
                do i = -1,1
                    x = delta*real(i, dp)
                    if (order == 1) then
                        A(row,:) = [1.0_dp, x, y]
                    elseif (order == 2) then
                        A(row,:) = [1.0_dp, x, x**2, y, x*y, y**2]
                    endif
                    row = row + 1
                end do
            end do
        end block

        ! Compute the matrix int_ATAxAT
        allocate(inv_ATA(n,n))
        allocate(inv_ATAxAT(n,m))
        allocate(ipiv(n))

        ! Compute tranpose of A
        AT = transpose(A)

        ! Compute the matrix AT*A for now stored in inv_ATA
        inv_ATA = matmul(AT, A)

        ! Perform LU factorization
        call dgetrf(n, n, inv_ATA, n, ipiv, info)
        if (info /= 0) print *, 'error in dgertf'

        ! Query for optimal dimension
        call dgetri(n, inv_ATA, n, ipiv, work_dimension, -1, info)
        if (info /= 0) print *, 'error in dgerti'

        ! Allocate optimal work space
        lwork = int(work_dimension(1))
        allocate(work(lwork))

        ! Compute invers of AT*A
        call dgetri(n, inv_ATA, n, ipiv, work, lwork, info)
        if (info /= 0) print *, 'error in dgerti'

        ! Compute inv_ATA*AT
        inv_ATAxAT = matmul(inv_ATA, AT)

        ! Free memory
        deallocate(A, AT, inv_ATA, ipiv, work)

    end subroutine
    !===============================================================================================

    !===============================================================================================
    function get_coefficients(f) result(c)

        ! In/Out variables
        real(dp), intent(in ) :: f(m)
        real(dp)              :: c(n)

        c = matmul(inv_ATAxAT, f)

    end function get_coefficients
    !===============================================================================================

    !===============================================================================================
    function P1st(x, a) result(f)

        real(dp), intent(in)           :: x(:)
        real(dp), intent(in), optional :: a(:)
        real(dp)                       :: f

        f = a(1) + a(2)*x(1) + a(3)*x(2)

    end function P1st
    !===============================================================================================

    !===============================================================================================
    function P2nd(x, a) result(f)

        real(dp), intent(in)           :: x(:)
        real(dp), intent(in), optional :: a(:)
        real(dp)                       :: f

        f = a(1) + a(2)*x(1) + a(3)*x(1)**2 + a(4)*x(2) + a(5)*x(1)*x(2) + a(6)*x(2)**2

    end function P2nd
    !===============================================================================================

    !===============================================================================================
    subroutine destroy_polynomial_reconstruction

        deallocate(inv_ATAxAT)

    end subroutine destroy_polynomial_reconstruction
    !===============================================================================================


end module ftlib_polynomials
