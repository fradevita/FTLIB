program test

    use ftlib_kinds
    use ftlib_grid
    use ftlib_scalar
    use ftlib_polynomials
    use ftlib_function

    implicit none

    real(dp), parameter :: pi = acos(-1.0_dp), L = 2.0_dp*pi 

    integer             :: r, Nx, outid_1st, outid_2nd
    type(grid)          :: G
    type(scalar)        :: s
    type(function_type) :: tf

    open(newunit = outid_1st, file = 'error_1st.csv')
    write(outid_1st,'(A)') 'delta,Linf,L2'
    open(newunit = outid_2nd, file = 'error_2nd.csv')
    write(outid_2nd,'(A)') 'delta,Linf,L2'

    do r = 4,9
        Nx = 2**r

        ! Setup the grid
        G = grid([Nx, Nx, 1], [L, L, L/real(Nx ,dp)], [0.0_dp, 0.0_dp, 0.0_dp], 'grid')
        
        ! Setup the scalar
        s = scalar(G, [1, 1, 0], 's')
        tf%f => test_function
        call s%set_from_function(tf)

        ! Setup the polynomail reconstruction with 1st order
        call init_polynomial_reconstruction(1, G%delta(1))

        ! test the reconstruction
        block
            integer  :: i, j
            real(dp) :: solution, c(n), e, Linf, L2
            Linf = 0.0_dp
            L2 = 0.0_dp
            do j = 2,G%hi(2)-1
                do i = 2,G%hi(1)-1
                    solution = test_function([G%xc(i), G%yc(j)])
                    c = get_coefficients(reshape(s%f(i-1:i+1,j-1:j+1, 1), (/m/)))
                    e = abs(solution - P([0.0_dp, 0.0_dp], c))!/abs(solution)
                    if (e > Linf) Linf = e
                    L2 = L2 + e**2
                end do
            end do
            write(outid_1st,'(*(E16.8,:,","))') G%delta(1), Linf, sqrt(L2)
        end block
        call destroy_polynomial_reconstruction()

        ! Setup the polynomail reconstruction with 2nd order
        call init_polynomial_reconstruction(2, G%delta(1))

        ! test the reconstruction
        block
            integer  :: i, j
            real(dp) :: solution, c(n), e, Linf, L2
            Linf = 0.0_dp
            L2 = 0.0_dp
            do j = 2,G%hi(2)-1
                do i = 2,G%hi(1)-1
                    solution = test_function([G%xc(i), G%yc(j)])
                    c = get_coefficients(reshape(s%f(i-1:i+1,j-1:j+1, 1), (/m/)))
                    e = abs(solution - P([0.0_dp, 0.0_dp], c))!/abs(solution)
                    if (e > Linf) Linf = e
                    L2 = L2 + e**2
                end do
            end do
            write(outid_2nd,'(*(E16.8,:,","))') G%delta(1), Linf, sqrt(L2)
        end block
        call destroy_polynomial_reconstruction()

        call G%destroy()
        call s%destroy()

    end do
contains
    
    !===============================================================================================
    function test_function(x, args) result(f)

        real(dp), intent(in)           :: x(:)
        real(dp), intent(in), optional :: args(:)
        real(dp)                       :: f

        !f = 1.5_dp*x(1)**2 + 3.2_dp*x(2) 
        f = cos(2.0_dp*pi*x(1))*sin(2.0_dp*x(2))

    end function test_function
    !===============================================================================================

end program test
