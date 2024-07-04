!> This moule contains the definition of the type root and its procedure.
!> Type root is an abstract object for solving root finding.
module ftlib_root

    use ftlib_kinds   , only : dp
    use ftlib_function, only : function_procedure
    use ftlib_io      , only : print_error_message

    implicit none
    private

    !< Abstract root object
    type, abstract :: root
        !< lower and upper extreme of the itnerval
        real(dp) :: a, b
        !< tolerance and maximum number of iteration of the solver
        real(dp) :: tol         
        integer  :: max_iter
        !< pointers to the function and function derivative
        procedure(function_procedure), nopass, pointer :: evaluate_f
        procedure(function_procedure), nopass, pointer :: evaluate_df
        contains
            procedure                      , pass(self)           :: setup
            procedure(root_solver_function), pass(self), deferred :: find_root
    end type root

    type, extends(root) :: bisection 
        contains
            procedure, pass(self) :: find_root => bisection_solver
    end type

    type, extends(root) :: secant
        contains
            procedure, pass(self) :: find_root => secant_solver
    end type

    type, extends(root) :: newton_raphson 
    contains
        procedure, pass(self) :: find_root => newton_raphson_solver
    end type

    abstract interface
        function root_solver_function(self) result(f)
            use ftlib_kinds, only : dp
            import root
            class(root), intent(inout) :: self
            real(dp)                   :: f
        end function root_solver_function
    end interface

    public :: root, bisection, secant, newton_raphson

contains

    !===============================================================================================
    subroutine setup(self, a, b, tol, max_iter, f, df)

        class(root)                  , intent(inout) :: self     !< root_founding object
        real(dp)                     , intent(in   ) :: a        !< lower extreme of the interval
        real(dp)                     , intent(in   ) :: b        !< upper extreme of the interval
        real(dp)                     , intent(in   ) :: tol      !< tolerance of the solver
        integer                      , intent(in   ) :: max_iter !< maximum number of iterations
        procedure(function_procedure)                :: f        !< user defined function
        procedure(function_procedure), optional      :: df

        ! Setup self object
        self%a = a
        self%b = b
        self%tol = tol
        self%max_iter = max_iter
        self%evaluate_f => f
        if (present(df)) self%evaluate_df => df

    end subroutine setup
    !===============================================================================================

    !===============================================================================================
    function bisection_solver(self) result(c)

        ! In/Out variables
        class(bisection), intent(inout) :: self !< bisection object
        real(dp)                        :: c    !< root

        ! Local variables
        integer  :: iter
        real(dp) :: a, b, fa, fb, fc

        ! Select initial interval
        a = self%a
        b = self%b

        ! Check that the pointer evaluate_f is associated
        if (associated(self%evaluate_f) .eqv. .false.) then
            call print_error_message('ERROR: the pointer evaluate_f must be associated.')
        endif

        ! Start iterations
        iterate: do iter = 1,self%max_iter

            ! Compute midpoint
            c = (a + b)/2.0_dp

            ! Evaluate the function in the midpoint
            fc = self%evaluate_f([c])

            ! Check for convergence
            if (abs(fc) < self%tol) then
                ! Converged
                exit iterate
            else
                ! Search new interval
                fa = self%evaluate_f([a])
                fb = self%evaluate_f([b])

                if (fa*fc < 0.0_dp) then
                    !use [a, c]
                    b = c
                elseif (fb*fc < 0.0_dp) then
                    ! use [c, b]
                    a = c
                else
                    call print_error_message('ERROR: unable to find an interval containing the root.')
                endif
            endif

#ifdef VERBOSE
            print '(A11,I3,A12,E16.8)', 'Iteration: ', iter, 'Residual: ', fc
#endif

        end do iterate

    end function bisection_solver
    !===============================================================================================

     !==============================================================================================
    function secant_solver(self) result(x2)

        ! In/Out variables
        class(secant), intent(inout) :: self !< secant object
        real(dp)                     :: x2    !< root

        ! Local variables
        integer  :: iter
        real(dp) :: x0, x1

        ! Check that the pointer evaluate_f is associated
        if (associated(self%evaluate_f) .eqv. .false.) then
            call print_error_message('ERROR: the pointer evaluate_f must be associated.')
        endif

        x0 = self%a
        x1 = self%b

        ! Start iterations
        iterate: do iter = 1,self%max_iter

            x2 = x1 - self%evaluate_f([x1])*(x1 - x0)/ &
                 (self%evaluate_f([x1]) - self%evaluate_f([x0]))

            ! Check for convergence
            if (abs(x2 - x1) < self%tol) then
                ! Converged
                exit iterate
            else
                x0 = x1
                x1 = x2
            endif

#ifdef VERBOSE
            print '(A11,I3,A12,E16.8)', 'Iteration: ', iter, 'Residual: ', self%evaluate_f([x2])
#endif

        end do iterate

    end function secant_solver
    !===============================================================================================

    !===============================================================================================
    function newton_raphson_solver(self) result(x1)

        ! In/Out variables
        class(newton_raphson), intent(inout) :: self !< newton_raphson object
        real(dp)                             :: x1   !< root

        ! Local variables
        integer  :: iter
        real(dp) :: x0

        ! Check that the pointer evaluate_f is associated
        if (associated(self%evaluate_f) .eqv. .false.) then
            call print_error_message('ERROR: the pointer evaluate_f must be associated.')
        endif
        if (associated(self%evaluate_df) .eqv. .false.) then
            call print_error_message('ERROR: the pointer evaluate_df must be associated.')
        endif

        x0 = self%a
        x1 = self%b

        ! Start iterations
        iterate: do iter = 1,self%max_iter

            x1 = x0 - self%evaluate_f([x0])/(self%evaluate_df([x0]) + 1.0e-16_dp)

            ! Check for convergence
            if (abs(x1 - x0) < self%tol) then
                ! Converged
                exit iterate
            else
                x0 = x1
            endif

#ifdef VERBOSE
            print '(A11,I3,A12,E16.8)', 'Iteration: ', iter, 'Residual: ', self%evaluate_f([x1])
#endif

        end do iterate

    end function newton_raphson_solver
    !===============================================================================================

end module ftlib_root