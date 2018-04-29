module heat_equation
    
    ! generic interface calculating average heat equation solution error for different precision levels
    interface average_error
        module procedure average_error_4, average_error_8, average_error_16
    end interface average_error
    ! generic interface solving the heat equation for different precision levels
    interface heat_equation_solver
        module procedure heat_equation_solver_4, heat_equation_solver_8, heat_equation_solver_16
    end interface heat_equation_solver

contains

    subroutine average_error_4(N, ans)
        integer :: N, i
        real (kind = 4), allocatable :: A(:, :), X(:)
        real (kind = 4) :: ans
        allocate(A(0:N, 0:N))
        allocate(X(0:N))
        call heat_equation_solver(A, X)
        ans = 0.0_4
        do i = 0, N
            ans = ans + abs((X(i)/A(i,i)) - (real(i, kind = 4)/real(N, kind = 4)))
        end do
        ans = ans/real((N+1), kind = 4)
        if (ALLOCATED(A)) deallocate(A)
        if (ALLOCATED(X)) deallocate(X)
    end subroutine average_error_4

    subroutine average_error_8(N, ans)
        integer :: N, i
        real (kind = 8), allocatable :: A(:, :), X(:)
        real (kind = 8) :: ans
        allocate(A(0:N, 0:N))
        allocate(X(0:N))
        call heat_equation_solver(A, X)
        ans = 0.0_8
        do i = 0, N
            ans = ans + abs((X(i)/A(i,i)) - (real(i, kind = 8)/real(N, kind = 8)))
        end do
        ans = ans/real((N+1), kind = 8)
        if (ALLOCATED(A)) deallocate(A)
        if (ALLOCATED(X)) deallocate(X)
    end subroutine average_error_8

    subroutine average_error_16(N, ans)
        integer :: N, i
        real (kind = 16), allocatable :: A(:, :), X(:)
        real (kind = 16) :: ans
        allocate(A(0:N, 0:N))
        allocate(X(0:N))
        call heat_equation_solver(A, X)
        ans = 0.0_16
        do i = 0, N
            ans = ans + abs((X(i)/A(i,i)) - real(i, kind = 16)/real(N, kind = 16))
        end do
        ans = ans/real((N+1), kind = 16)
        if (ALLOCATED(A)) deallocate(A)
        if (ALLOCATED(X)) deallocate(X)
    end subroutine average_error_16        

    subroutine heat_equation_solver_4(A, X)
        use matrix_operations, only : gj_elim
        integer ::  i, N
        real (kind = 4), allocatable :: A(:, :), X(:)
        real (kind = 4) :: P1, P2
        N = UBOUND(A,1)
        if (allocated(A) .AND. allocated(X)) then
            A(:,:) = 0.0_4
            X(:) = 0.0_4
            P1 = 1.0_4*real(N, kind = 4)*real(N, kind = 4)
            P2 = -2.0_4 * P1
            do i = LBOUND(A,1)+1, N-1
                A(i-1, i) = P1
                A(i, i) = P2
                A(i+1, i) = P1
            end do
            A(0,0) = 1.0_4
            A(N,N) = 1.0_4
            X(N) = 1.0_4
            call gj_elim(A, X) 
        endif
    end subroutine heat_equation_solver_4

    subroutine heat_equation_solver_8(A, X)
        use matrix_operations, only : gj_elim
        integer :: i, N
        real (kind = 8), allocatable :: A(:, :), X(:)
        real (kind = 8) :: P1, P2
        N = UBOUND(A,1)
        if (allocated(A) .AND. allocated(X)) then
            A(:,:) = 0.0_8
            X(:) = 0.0_8
            P1 = 1.0_8*real(N, kind = 8)*real(N, kind = 8)
            P2 = -2.0_8 * P1
            do i = LBOUND(A,1)+1, N-1
                A(i-1, i) = P1
                A(i, i) = P2
                A(i+1, i) = P1
            end do
            A(0,0) = 1.0_8
            A(N,N) = 1.0_8
            X(N) = 1.0_8
            call gj_elim(A, X) 
        endif
    end subroutine heat_equation_solver_8

    subroutine heat_equation_solver_16(A, X)
        use matrix_operations, only : gj_elim
        integer :: i, N
        real (kind = 16), allocatable :: A(:, :), X(:)
        real (kind = 16) :: P1, P2
        N = UBOUND(A,1)
        if (allocated(A) .AND. allocated(X)) then
            A(:,:) = 0.0_16
            X(:) = 0.0_16
            P1 = 1.0_16*real(N, kind = 16)*real(N, kind = 16)
            P2 = -2.0_16 * P1
            do i = LBOUND(A,1)+1, N-1
                A(i-1, i) = P1
                A(i, i) = P2
                A(i+1, i) = P1
            end do
            A(0,0) = 1.0_16
            A(N,N) = 1.0_16
            X(N) = 1.0_16
            call gj_elim(A, X) 
        endif
    end subroutine heat_equation_solver_16

end module heat_equation