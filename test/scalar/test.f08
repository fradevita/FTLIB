program test

    use ftlib_grid2d
    use ftlib_scalar2d

    implicit none

    type(grid2d), target :: G
    type(scalar2d)       :: s

    G = grid2d([2, 2], [1.0_dp, 1.0_dp], [0.0_dp, 0.0_dp], 'G')
    s = scalar2d(G, 0)  
    
    call G%destroy()
    call s%destroy()

end program test