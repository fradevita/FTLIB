module ftlib_grid2D

    use ftlib_kinds, only : dp

    implicit none
    private 
    public :: grid2D

    type :: grid2D
        integer               :: Nx, Ny
        real(dp)              :: x0, y0
        real(dp)              :: Lx, Ly
        real(dp)              :: dx, dy
        real(dp), allocatable :: x(:), y(:)
        character(len=99)     :: name = 'grid2d'
    contains
        procedure, pass(self) :: destroy
        procedure, pass(self) :: print_json
    end type grid2D

    interface grid2D
        procedure constructor
    end interface

contains

    !===============================================================================================
    function constructor(logical_size, physical_size, origin, name)

        ! In/Out variables
        type(grid2D)                           :: constructor
        integer         , intent(in)           :: logical_size(2)
        real(dp)        , intent(in)           :: physical_size(2)
        real(dp)        , intent(in)           :: origin(2)
        character(len=*), intent(in), optional :: name
        
        constructor%Nx = logical_size(1)
        constructor%Ny = logical_size(2)
        constructor%Lx = physical_size(1)
        constructor%Ly = physical_size(2)
        constructor%x0 = origin(1)
        constructor%y0 = origin(2)
        constructor%dx = constructor%Lx/real(constructor%Nx, dp)
        constructor%dy = constructor%Ly/real(constructor%Ny, dp)
        allocate(constructor%x(constructor%Nx))
        allocate(constructor%y(constructor%Ny))
        
        block
            integer :: i
            do i = 1,constructor%Nx
                constructor%x(i) = constructor%x0 + (i - 0.5_dp)*constructor%dx
            end do
            do i = 1,constructor%Ny
                constructor%y(i) = constructor%y0 + (i - 0.5_dp)*constructor%dy
            end do
        end block

        if (present(name)) constructor%name = name

    end function constructor
    !===============================================================================================
   
    !===============================================================================================
    subroutine print_json(self)
   
        class(grid2D), intent(in) :: self
        
        integer           :: out_id
        character(len=99) :: filename

        filename = trim(self%name)//'.json'
        open(newunit = out_id, file = filename)
        write(out_id,'(A1)') '{'
        write(out_id,'(4x,A9)') '"Grid": {'
        write(out_id,'(8x,A6,1x,I7,A1)') '"Nx": ', self%Nx, ','
        write(out_id,'(8x,A6,1x,I7,A1)') '"Ny": ', self%Ny, ','
        write(out_id,'(8x,A11,1x,E16.8,A1,E16.8,A2)') '"origin": [', self%x0, ',', self%y0, '],'
        write(out_id,'(8x,A6,1x,E16.8,A1)') '"Lx": ', self%Lx, ','
        write(out_id,'(8x,A6,1x,E16.8,A1)') '"Ly": ', self%Ly
        write(out_id,'(4x,A3)') '}'
        write(out_id,'(A1)') '}'

        call flush(out_id)
        
    end subroutine
    !===============================================================================================

    !===============================================================================================
    subroutine destroy(self)

        class(grid2D), intent(inout) :: self

        deallocate(self%x, self%y)

    end subroutine destroy
    !===============================================================================================

end module ftlib_grid2D