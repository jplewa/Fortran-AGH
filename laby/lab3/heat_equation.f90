module heat_equation
    interface average_error
        module procedure average_error_1, average_error_2, average_error_3
    end interface average_error
    interface heat_equation_solver
        module procedure heat_equation_solver_1, heat_equation_solver_2, heat_equation_solver_3
    end interface heat_equation_solver

contains
    function average_error_1(N, what_kind) result(ans)
        integer :: N, i
        real (kind = 4), allocatable :: A(:, :), X(:)
        real (kind = 4) :: what_kind, ans
        allocate(A(0:N, 0:N))
        allocate(X(0:N))
        call heat_equation_solver(A, X, 0, N)
        ans = 0
        do i = 0, N
            ans = ans + abs((A(i,i)*X(i)) - i/real(N))
        end do
        ans = ans/(N+1)
        if (ALLOCATED(A)) deallocate(A)
        if (ALLOCATED(X)) deallocate(X)
    end function average_error_1

    function average_error_2(N, what_kind) result(ans)
        integer :: N, i
        real (kind = 8), allocatable :: A(:, :), X(:)
        real (kind = 8) :: what_kind, ans
        allocate(A(0:N, 0:N))
        allocate(X(0:N))
        call heat_equation_solver(A, X, 0, N)
        ans = 0
        do i = 0, N
            ans = ans + abs((A(i,i)*X(i)) - i/real(N))
        end do
        ans = ans/(N+1)
        if (ALLOCATED(A)) deallocate(A)
        if (ALLOCATED(X)) deallocate(X)
    end function average_error_2

    function average_error_3(N, what_kind) result(ans)
        integer :: N, i
        real (kind = 16), allocatable :: A(:, :), X(:)
        real (kind = 16) :: what_kind, ans
        allocate(A(0:N, 0:N))
        allocate(X(0:N))
        call heat_equation_solver(A, X, 0, N)
        ans = 0
        do i = 0, N
            ans = ans + abs((A(i,i)*X(i)) - i/real(N))
        end do
        ans = ans/(N+1)
        if (ALLOCATED(A)) deallocate(A)
        if (ALLOCATED(X)) deallocate(X)
    end function average_error_3        

    subroutine heat_equation_solver_1(A, X, M, N)
        use gauss_elim, only : jg_elim
        integer :: M, N, i
        real (kind = 4), allocatable :: A(:, :), X(:)
        real (kind = 4) :: h
        if (allocated(A) .AND. allocated(X)) then
            h = 1/real(N)
            A(:,:) = 0
            X(:) = 0
            do i = M+1, N-1
                A(i-1, i) = 1/(h**2)
                A(i, i) = -2/(h**2)
                A(i+1, i) = 1/(h**2)
            end do
            A(0,0) = 1
            A(N,N) = 1
            X(N) = 1
            call jg_elim(A, X, 0, N) 
        endif
    end subroutine heat_equation_solver_1

    subroutine heat_equation_solver_2(A, X, M, N)
        use gauss_elim, only : jg_elim
        integer :: M, N, i
        real (kind = 8), allocatable :: A(:, :), X(:)
        real (kind = 8) :: h
        if (allocated(A) .AND. allocated(X)) then
            h = 1/real(N)
            A(:,:) = 0
            X(:) = 0
            do i = M+1, N-1
                A(i-1, i) = 1/(h**2)
                A(i, i) = -2/(h**2)
                A(i+1, i) = 1/(h**2)
            end do
            A(0,0) = 1
            A(N,N) = 1
            X(N) = 1
            call jg_elim(A, X, 0, N) 
        endif
    end subroutine heat_equation_solver_2

    subroutine heat_equation_solver_3(A, X, M, N)
        use gauss_elim, only : jg_elim
        integer :: M, N, i
        real (kind = 16), allocatable :: A(:, :), X(:)
        real (kind = 16) :: h
        if (allocated(A) .AND. allocated(X)) then
            h = 1/real(N)
            A(:,:) = 0
            X(:) = 0
            do i = M+1, N-1
                A(i-1, i) = 1/(h**2)
                A(i, i) = -2/(h**2)
                A(i+1, i) = 1/(h**2)
            end do
            A(0,0) = 1
            A(N,N) = 1
            X(N) = 1
            call jg_elim(A, X, 0, N) 
        endif
    end subroutine heat_equation_solver_3

end module heat_equation