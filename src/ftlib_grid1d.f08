module ftlib_grid1d

    use ftlib_kinds, only : dp
    use ftlib_grid , only :grid

    implicit none
    private 
    public :: grid1d

    type, extends(grid) :: grid1d
        integer               :: N
        real(dp)              :: x0
        real(dp)              :: L
        real(dp)              :: dx
        real(dp), allocatable :: x(:)
        character(len=99)     :: name = 'grid1d'
    contains
        procedure, pass(self) :: destroy
        procedure, pass(self) :: print_json
    end type grid1d

    interface grid1d
        procedure constructor
    end interface

contains

    !===============================================================================================
    function constructor(logical_size, physical_size, origin, name)

        ! In/Out variables
        type(grid1d)                           :: constructor
        integer         , intent(in)           :: logical_size
        real(dp)        , intent(in)           :: physical_size
        real(dp)        , intent(in)           :: origin
        character(len=*), intent(in), optional :: name
        
        constructor%N = logical_size
        constructor%L = physical_size
        constructor%x0 = origin
        constructor%dx = constructor%L/real(constructor%N, dp)
        allocate(constructor%x(constructor%N))
        block
            integer :: i
            do i = 1,constructor%N
                constructor%x(i) = constructor%x0 + (i - 0.5_dp)*constructor%dx
            end do
        end block

        if (present(name)) constructor%name = name

    end function constructor
    !===============================================================================================
   
    !===============================================================================================
    subroutine print_json(self)
   
        class(grid1d), intent(in) :: self
        
        integer           :: out_id
        character(len=99) :: filename

        filename = trim(self%name)//'.json'
        open(newunit = out_id, file = filename)
        write(out_id,'(A1)') '{'
        write(out_id,'(4x,A9)') '"Grid": {'
        write(out_id,'(8x,A6,1x,I7,A1)') '"Nx": ', self%N, ','
        write(out_id,'(8x,A11,1x,E16.8,A2)') '"origin": [', self%x0, '],'
        write(out_id,'(8x,A6,1x,E16.8)') '"Lx": ', self%L
        write(out_id,'(4x,A3)') '}'
        write(out_id,'(A1)') '}'

        call flush(out_id)
        
    end subroutine
    !===============================================================================================

    !===============================================================================================
    subroutine destroy(self)

        class(grid1d), intent(inout) :: self

        deallocate(self%x)

    end subroutine destroy
    !===============================================================================================

end module ftlib_grid1d