program test

    ! Test the creation of grids and the proper deallocation of memory
    use ftlib_kinds
    use ftlib_grid

    implicit none

    type(grid) :: G

    ! Create 1d grid
    G = grid(logical_size = [2, 1, 1], &
             physical_size = [1.0_dp, 0.5_dp, 0.5_dp], &
             origin = [0.0_dp, 0.0_dp, 0.0_dp], &
             name = 'grid')
    call G%print_json()
    call G%destroy()

end program test