!> This module contains some I/O functions using iso_fortran_env.
module ftlib_io

    use, intrinsic :: iso_fortran_env, only: stdout => OUTPUT_UNIT
    use, intrinsic :: iso_fortran_env, only: stderr => ERROR_UNIT

    implicit none

contains

    !===============================================================================================
    subroutine print_error_message(message)
        !< Subroutine to print an errore message on stderr
        
        ! In/Out variables
        character(len=*), intent(in) :: message !< Input message

        ! Output the error message to stderr
        write(stderr, '(A)') message
        write(stderr,     *) 'Aborting ...'
        
        ! Abort program
        call abort()

    end subroutine print_error_message
    !===============================================================================================

end module
