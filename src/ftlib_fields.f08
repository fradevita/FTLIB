module ftlib_fields

    use ftlib_io

    implicit none

    private

    interface laplacian
        module procedure laplacian1d
        !module procedure laplacian2d
    end interface

    public :: laplacian

contains

    !===============================================================================================
    function laplacian1d(s)

        use ftlib_scalar1d

        type(scalar1d), intent(in) :: s
        type(scalar1d)             :: laplacian1d

        ! Local variables
        integer :: i

        ! Assert that s has ghost nodes
        if (s%gl == 0) then
            call print_error_message('ERROR: cannot compute laplacian without ghos nodes.')
        endif

        laplacian1d = scalar1d(s%G, 0)
        do i = 1,s%G%N
            laplacian1d%f(i) = (s%f(i+1) - 2.0_dp*s%f(i) + s%f(i-1))/s%G%dx**2
        end do

    end function
    !===============================================================================================

end module ftlib_fields