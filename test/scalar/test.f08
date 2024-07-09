program test

    use ftlib_kinds
    use ftlib_grid
    use ftlib_scalar

    implicit none

    integer , parameter :: N = 2
    real(dp), parameter :: L = 1.0_dp

    type(grid)   :: G
    type(scalar) :: s

    ! Create 1d grid
    G = grid(logical_size = [N, 1, 1], &
             physical_size = [L, L/N, L/N], &
             origin = [0.0_dp, 0.0_dp, 0.0_dp], &
             name = 'grid')

    ! Create the scalar s on the grid G
    s = scalar(G, [0, 0, 0], 's')

    call G%destroy()
    call s%destroy()

end program test