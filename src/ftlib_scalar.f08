module ftlib_scalar

    use ftlib_kinds, only : dp
    use ftlib_grid
    
    implicit none
    private
    public :: scalar

    type :: scalar
        integer                 :: gl(3)
        real(dp)  , allocatable :: f(:,:,:)
        type(grid), pointer     :: G => Null()
        character(len=99)       :: name = 'unset'
    contains
        procedure, pass(self) :: destroy
        procedure, pass(self) :: set_from_function
        procedure, pass(self) :: update_ghost_nodes
    end type

    interface scalar
        procedure constructor
    end interface

contains

    !===============================================================================================
    function constructor(G, gl, name)

        ! In/Out variables
        type(scalar)                           :: constructor
        type(grid)      , intent(in), target   :: G
        integer         , intent(in)           :: gl(3)
        character(len=*), intent(in), optional :: name

        constructor%G => G
        constructor%gl = gl
        allocate(constructor%f(-gl(1)+1:G%hi(1)+gl(1),-gl(2)+1:G%hi(2)+gl(2),-gl(3)+1:G%hi(3)+gl(3)))
        if (present(name)) constructor%name = name

    end function constructor
    !===============================================================================================

    !===============================================================================================
    subroutine set_from_function(self, f)

        use ftlib_function

        ! In/Out variables
        class(scalar)      , intent(inout) :: self
        type(function_type), intent(in   ) :: f

        ! Local variables
        integer :: i, j, k

        do k = 1,self%G%hi(3)
            do j = 1,self%G%hi(2)
                do i = 1,self%G%hi(1)
                    self%f(i,j,k) = f%f([self%G%xc(i), self%G%yc(j), self%G%zc(k)], f%args)
                end do
            end do
        end do

    end subroutine set_from_function 
    !===============================================================================================

    !===============================================================================================
    subroutine update_ghost_nodes(self)

        use ftlib_io

        class(scalar), intent(inout) :: self

        if (maxval(self%gl) == 0) then
            call print_error_message('ERROR: gl must be > 0 to update ghost ndoes.')
        end if

        self%f(0,:,:) = self%f(self%G%hi(1),:,:)
        self%f(self%G%hi(1)+1,:,:) = self%f(1,:,:)

    end subroutine update_ghost_nodes
    !===============================================================================================
   
    !===============================================================================================
    subroutine destroy(self)

        class(scalar), intent(inout) :: self

        deallocate(self%f)
   
    end subroutine destroy
    !===============================================================================================

end module ftlib_scalar