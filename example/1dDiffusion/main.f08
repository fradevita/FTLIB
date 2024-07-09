program main

    use ftlib_kinds
    use ftlib_grid
    use ftlib_scalar
    use ftlib_fields

    implicit none

    integer , parameter :: N = 32
    real(dp), parameter :: L = 2.0_dp, alpha = 1.0_dp
    real(dp), parameter :: pi = acos(-1.0_dp), mu = 1.0_dp, sigma = 0.1_dp
    
    integer      :: i, s, Nstep
    real(dp)     :: dt
    type(grid)   :: G
    type(scalar) :: T, To, Tn, d2Tdx2

    ! Setup the grid and the scalar
    G = grid([N, 1, 1], [L, L/N, L/N], [0.0_dp, 0.0_dp, 0.0_dp])
    T = scalar(G, [1, 0, 0], 'T')
    To = scalar(G, [1, 0, 0], 'To')
    Tn = scalar(G, [1, 0, 0], 'Tn')
    d2Tdx2 = scalar(G, [0, 0, 0], 'd2Tdx2')

    open(1, file = 'T.dat')
    ! Initial T field
    do i = 1,G%hi(1)
        T%f(i,1,1) = 1.0_dp/sigma/sqrt(2.0_dp*pi)*exp(-0.5_dp*((G%xc(i) - mu)/sigma)**2)
        write(1,*) G%xc(i), T%f(i,1,1)
    end do
    call T%update_ghost_nodes()
    write(1,*) ''
    write(1,*) ''
    To = T

    ! Solve diffusion equation
    dt = 0.5_dp*G%delta(1)**2/alpha
    Nstep = 20000
    do s = 1,Nstep
        d2Tdx2 = laplacian(T, 1)
        Tn%f(1:N,:,:) = 2.0_dp*T%f(1:N,:,:) - To%f(1:N,:,:) + alpha*dt*dt*d2Tdx2%f 
        call Tn%update_ghost_nodes()
        do i = 1,G%hi(1)
            write(1,*) G%xc(i), Tn%f(i,1,1)
        end do
        write(1,*) ''
        write(1,*) ''
        To%f = T%f
        T%f = Tn%f
    end do
    close(1)

    ! Free the memory
    call G%destroy()
    call T%destroy()
    call To%destroy()
    call Tn%destroy()
    call d2Tdx2%destroy()
    
end program main
