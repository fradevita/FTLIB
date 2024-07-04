module ftlib_grid

    use ftlib_kinds, only : dp

    implicit none
    private 
    public :: grid

    type, abstract :: grid
    end type grid

end module ftlib_grid