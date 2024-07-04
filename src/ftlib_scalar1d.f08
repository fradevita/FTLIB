module ftlib_scalar1d

    use ftlib_kinds , only : dp
    use ftlib_grid1d, only : grid1d
    
    implicit none

    type :: scalar1d
        integer               :: gl
        real(dp), allocatable :: f(:)
        type(grid1d), pointer :: G => Null()
        character(len=99)     :: name = 'unset'
    contains
        procedure, pass(self) :: update_ghost_nodes
        procedure, pass(self) :: destroy
    end type scalar1d

    interface scalar1d
        procedure constructor
    end interface

contains

    !===============================================================================================
    function constructor(grid, gl, name)

        ! In/Out variables
        type(scalar1d)                         :: constructor
        type(grid1d)    , intent(in), target   :: grid
        integer         , intent(in)           :: gl
        character(len=*), intent(in), optional :: name

        constructor%G =>  grid
        constructor%gl = gl
        allocate(constructor%f(-gl+1:grid%N+gl))

        if (present(name)) constructor%name = name

    end function constructor
    !===============================================================================================

    !===============================================================================================
    subroutine update_ghost_nodes(self)

        class(scalar1d), intent(inout) :: self

        self%f(0) = self%f(self%G%N)
        self%f(self%G%N+1) = self%f(1)

    end subroutine update_ghost_nodes
    !===============================================================================================
   
    !===============================================================================================
    subroutine destroy(self)

        class(scalar1d), intent(inout) :: self

        deallocate(self%f)

    end subroutine destroy
    !===============================================================================================

end module ftlib_scalar1d