module ftlib_scalar2d

    use ftlib_kinds , only : dp
    use ftlib_grid2d, only : grid2d
    
    implicit none

    type :: scalar2d
        integer               :: gl
        real(dp), allocatable :: f(:,:)
        type(grid2d), pointer :: G => Null()
        character(len=99)     :: name = 'unset'
    contains
        procedure, pass(self) :: destroy
    end type scalar2d

    interface scalar2d
        procedure constructor
    end interface

contains

    !===============================================================================================
    function constructor(grid, gl, name)

        ! In/Out variables
        type(scalar2d)                         :: constructor
        type(grid2D)    , intent(in), target   :: grid
        integer         , intent(in)           :: gl
        character(len=*), intent(in), optional :: name

        constructor%G =>  grid
        constructor%gl = gl
        allocate(constructor%f(-gl:grid%Nx + gl,-gl:grid%Ny + gl))

        if (present(name)) constructor%name = name

    end function constructor
    !===============================================================================================
   
    !===============================================================================================
    subroutine destroy(self)

        class(scalar2D), intent(inout) :: self

        deallocate(self%f)

    end subroutine destroy
    !===============================================================================================

end module ftlib_scalar2d