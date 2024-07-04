program test

    use ftlib_kinds
    use ftlib_grid2d
    use ftlib_scalar2d
    use ftlib_polynomals

    implicit none

    type(grid2d), target :: G
    type(scalar2d)       :: s

    G = grid2D([10, 10], [1.0_dp, 1.0_dp], [0.0_dp, 0.0_dp])
    s = scalar2D(G, 0)

    ! Set the scalar field 
    block
        integer :: i, j
        do j = 1,G%Ny
            do i = 1,G%Nx
                s%f(i,j) = test_function(G%x(i), G%y(j)) 
            end do
        end do
    end block

    ! Setup the polynomail reconstruction with 1st order
    call init_polynomial_reconstruction(1, G)

    ! test the reconstruction
    block
        integer  :: i, j
        real(dp) :: solution, c(n), e, Linf, L2
        Linf = 0.0_dp
        L2 = 0.0_dp
        do j = 2,G%Ny-1
            do i = 2,G%Nx-1
                solution = test_function(G%x(i), G%y(j))
                c = get_coefficients(reshape(s%f(i-1:i+1,j-1:j+1), (/m/)))
                e = abs(solution - P([0.0_dp, 0.0_dp], c))/abs(solution)
                if (e > Linf) Linf = e
                L2 = L2 + e**2
            end do
        end do
        print *, '1st order, Linf = ', Linf, 'L2 = ', sqrt(L2)
    end block
    call destroy_polynomial_reconstruction()

    ! Setup the polynomail reconstruction with 2nd order
    call init_polynomial_reconstruction(2, G)

    ! test the reconstruction
    block
        integer  :: i, j
        real(dp) :: solution, c(n), e, Linf, L2
        Linf = 0.0_dp
        L2 = 0.0_dp
        do j = 2,G%Ny-1
            do i = 2,G%Nx-1
                solution = test_function(G%x(i), G%y(j))
                c = get_coefficients(reshape(s%f(i-1:i+1,j-1:j+1), (/m/)))
                e = abs(solution - P([0.0_dp, 0.0_dp], c))/abs(solution)
                if (e > Linf) Linf = e
                L2 = L2 + e**2
            end do
        end do
        print *, '2nd order, Linf = ', Linf, 'L2 = ', sqrt(L2)
    end block
    call destroy_polynomial_reconstruction()

    call G%destroy()
    call s%destroy()
contains
    
    !===============================================================================================
    function test_function(x, y) result(f)

        real(dp), intent(in) :: x, y
        real(dp)             :: f

        f = 1.5_dp*x**2 + 3.2_dp*y 

    end function test_function
    !===============================================================================================

end program test