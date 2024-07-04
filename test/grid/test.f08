program test

    use ftlib_grid2D

    implicit none

    type(grid2D) :: G

    G = grid2D([2, 2], [1.0_dp, 1.0_dp], [0.0_dp, 0.0_dp], 'G')
    call G%print_json()
    call G%destroy()

end program test