module ftlib_grid

    use ftlib_kinds, only : dp

    implicit none
    private
    public :: grid

    type :: grid
        ! Grid lower and higher bounds
        integer :: lo(3), hi(3)
        ! Grid physical size, origin and (constant) spacing
        real(dp) :: L(3), x0(3), delta(3)
        ! Grid cell centers
        real(dp), allocatable :: xc(:), yc(:), zc(:)  
        ! Grid name for output file
        character(len=99) :: name = 'grid'
    contains
        procedure, pass(self) :: destroy
        procedure, pass(self) :: print_json
    end type grid

    interface grid
        procedure constructor
    end interface

contains

    !===============================================================================================
    function constructor(logical_size, physical_size, origin, name)
        
        ! In/Out variables
        type(grid)                             :: constructor
        integer         , intent(in)           :: logical_size(:)
        real(dp)        , intent(in)           :: physical_size(:)
        real(dp)        , intent(in)           :: origin(:)
        character(len=*), intent(in), optional :: name
        
        ! Local variables
        integer :: i        

        constructor%lo = [1,1,1]
        constructor%hi = logical_size
        constructor%L = physical_size
        constructor%x0 = origin
        allocate(constructor%xc(constructor%hi(1)))
        allocate(constructor%yc(constructor%hi(2)))
        allocate(constructor%zc(constructor%hi(3)))
        constructor%delta = physical_size/logical_size
        do i = 1,constructor%hi(1)
            constructor%xc(i) = constructor%x0(1) + (i - 0.5_dp)*constructor%delta(1)
        end do
        do i = 1,constructor%hi(2)
            constructor%yc(i) = constructor%x0(2) + (i - 0.5_dp)*constructor%delta(2)
        end do
        do i = 1,constructor%hi(3)
            constructor%zc(i) = constructor%x0(3) + (i - 0.5_dp)*constructor%delta(3)
        end do
        if (present(name)) constructor%name = name

    end function constructor
    !===============================================================================================

    !===============================================================================================
    subroutine print_json(self)
   
        ! In/Out variables
        class(grid), intent(in) :: self
        
        ! Local variables
        integer           :: out_id
        character(len=99) :: filename

        filename = trim(self%name)//'.json'
        open(newunit = out_id, file = filename)
        write(out_id,'(A1)') '{'
        write(out_id,'(4x,A9)') '"Grid": {'
        write(out_id,'(8x,A6,1x,I7,A1)') '"Nx": ', self%hi(1), ','
        write(out_id,'(8x,A6,1x,I7,A1)') '"Ny": ', self%hi(2), ','
        write(out_id,'(8x,A6,1x,I7,A1)') '"Nz": ', self%hi(3), ','
        write(out_id,'(8x,A11,1x,E16.8,A1,E16.8,A1,E16.8,A2)') '"origin": [', &
                                        self%x0(1), ',', self%x0(2), ',', self%x0(3), '],'
        write(out_id,'(8x,A6,1x,E16.8,A1)') '"Lx": ', self%L(1), ','
        write(out_id,'(8x,A6,1x,E16.8,A1)') '"Ly": ', self%L(2), ','
        write(out_id,'(8x,A6,1x,E16.8)'   ) '"Lz": ', self%L(3)
        write(out_id,'(4x,A3)') '}'
        write(out_id,'(A1)') '}'

        call flush(out_id)
        
    end subroutine
    !===============================================================================================

    !===============================================================================================
    subroutine destroy(self)

        class(grid), intent(inout) :: self

        deallocate(self%xc, self%yc, self%zc)
        
    end subroutine destroy
    !===============================================================================================

end module ftlib_grid