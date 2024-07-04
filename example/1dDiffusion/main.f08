program main

    use ftlib_grid1d
    use ftlib_scalar1d
    use ftlib_fields

    implicit none

    integer , parameter :: N = 32
    real(dp), parameter :: alpha = 1.0_dp
    real(dp), parameter :: pi = acos(-1.0_dp), mu = 0.5_dp, sigma = 0.1_dp
    
    integer              :: i, s, Nstep
    real(dp)             :: dt
    type(grid1d), target :: G
    type(scalar1d)       :: T, To, Tn, d2Tdx2

    ! Setup the grid and the scalar
    G = grid1d(N, 1.0_dp, 0.0_dp)
    T = scalar1d(G, 1, 'T')
    To = scalar1d(G, 1, 'To')
    Tn = scalar1d(G, 1, 'Tn')
    d2Tdx2 = scalar1d(G, 0, 'd2Tdx2')

    open(1, file = 'T.dat')
    ! Initial T field
    do i = 1,G%N
        T%f(i) = 1.0_dp/sigma/sqrt(2.0_dp*pi)*exp(-0.5_dp*((G%x(i) - mu)/sigma)**2)
        write(1,*) G%x(i), T%f(i)
    end do
    call T%update_ghost_nodes()
    write(1,*) ''
    write(1,*) ''
    To = T

    ! Solve diffusion equation
    dt = 0.5_dp*G%dx**2/alpha
    Nstep = 2000
    do s = 1,Nstep
        d2Tdx2 = laplacian(T)
        Tn%f(1:N) = 2.0_dp*T%f(1:N) - To%f(1:N) + alpha*dt*dt*d2Tdx2%f 
        call Tn%update_ghost_nodes()
        do i = 1,G%N
            write(1,*) G%x(i), Tn%f(i)
        end do
        write(1,*) ''
        write(1,*) ''
        To = T
        T = Tn
    end do
    close(1)

    ! Free the memory
    call G%destroy()
    call T%destroy()
    call To%destroy()
    call Tn%destroy()
    call d2Tdx2%destroy()
    
end program main
