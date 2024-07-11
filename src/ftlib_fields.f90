module ftlib_fields

    use ftlib_kinds
    use ftlib_scalar
    use ftlib_io

    implicit none

    private
    public :: laplacian

contains

    !===============================================================================================
    function laplacian(s, Ndim)

        type(scalar), intent(in) :: s
        integer     , intent(in) :: Ndim
        type(scalar)             :: laplacian

        ! Assert that s has ghost nodes
        if (maxval(s%gl) == 0) then
            call print_error_message('ERROR: cannot compute laplacian without ghos nodes.')
        endif

        if (Ndim == 1) then
            laplacian = laplacian1d(s)
        elseif (Ndim == 2) then
                laplacian = laplacian2d(s)
        endif

    end function laplacian

    !===============================================================================================
    function laplacian1d(s)

        type(scalar), intent(in) :: s
        type(scalar)             :: laplacian1d

        ! Local variables
        integer :: i

        laplacian1d = scalar(s%G, [0,0,0])
        do i = 1,s%G%hi(1)
            laplacian1d%f(i,:,:) = (s%f(i+1,:,:) - 2.0_dp*s%f(i,:,:) + s%f(i-1,:,:))/s%G%delta(1)**2
        end do

    end function
    !===============================================================================================

    !===============================================================================================
    function laplacian2d(s)

        type(scalar), intent(in) :: s
        type(scalar)             :: laplacian2d

        ! Local variables
        integer :: i, j

        laplacian2d = scalar(s%G, [0,0,0])
        do j = 1,s%G%hi(2)
            do i = 1,s%G%hi(1)
                laplacian2d%f(i,j,:) = (s%f(i+1,j,:) - 2.0_dp*s%f(i,j,:) + s%f(i-1,j,:))/s%G%delta(1)**2 + &
                                       (s%f(i,j+1,:) - 2.0_dp*s%f(i,j,:) + s%f(i,j-1,:))/s%G%delta(2)**2
            end do
        end do

    end function
    !===============================================================================================

end module ftlib_fields