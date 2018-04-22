module tests
    real (kind = 4), parameter :: eps4 = 0.00001_4
    real (kind = 8), parameter :: eps8 = 0.00000000000001_8
    real (kind = 16), parameter :: eps16 = 0.00000000000000000000000000000001_16
    interface compare_epsilon
        module procedure compare_epsilon_4, compare_epsilon_8, compare_epsilon_16
    end interface compare_epsilon
    interface gj_elim_test
        module procedure gj_elim_test_4, gj_elim_test_8, gj_elim_test_16
    end interface gj_elim_test
contains
    function compare_epsilon_4(a, b, e) result(x)
        real (kind = 4) :: a, b, e
        LOGICAL :: x
        if (ABS(a - b) < e) then
            x = .TRUE.
        else 
            x = .FALSE.
        endif
    end function compare_epsilon_4
    function compare_epsilon_8(a, b, e) result(x)
        real (kind = 8) :: a, b, e
        LOGICAL :: x
        if (ABS(a - b) < e) then
            x = .TRUE.
        else 
            x = .FALSE.
        endif
    end function compare_epsilon_8
    function compare_epsilon_16(a, b, e) result(x)
        real (kind = 16) :: a, b, e
        LOGICAL :: x
        if (ABS(a - b) < e) then
            x = .TRUE.
        else 
            x = .FALSE.
        endif
    end function compare_epsilon_16

    function gj_elim_test_4(A, X, B, Y) result(passed)
        use matrix_operations, only : gj_elim
        real (kind = 4), allocatable :: A(:,:), X(:), B(:,:), Y(:)
        integer :: i, j
        logical :: passed
        passed = .FALSE.
        if (allocated(A) .AND. allocated(B) .AND. allocated(X) .AND. allocated(Y)) then
            call gj_elim(A,X)
            passed = .TRUE.
            do i = LBOUND(A,1),UBOUND(A,1)
                do j = LBOUND(A,1),UBOUND(A,1)
                    if (.NOT. (compare_epsilon(A(i,j), B(i,j), eps4))) then 
                        passed = .FALSE. 
                    endif
                end do
                if (.NOT. (compare_epsilon(X(i), Y(i), eps4))) then 
                    passed = .FALSE. 
                endif
            end do
        endif
    end function gj_elim_test_4
    function gj_elim_test_8(A, X, B, Y) result(passed)
        use matrix_operations, only : gj_elim
        real (kind = 8), allocatable :: A(:,:), X(:), B(:,:), Y(:)
        integer :: i, j
        logical :: passed
        passed = .FALSE.
        if (allocated(A) .AND. allocated(B) .AND. allocated(X) .AND. allocated(Y)) then
            call gj_elim(A,X)
            passed = .TRUE.
            do i = LBOUND(A,1),UBOUND(A,1)
                do j = LBOUND(A,1),UBOUND(A,1)
                    if (.NOT. (compare_epsilon(A(i,j), B(i,j), eps8))) then 
                        passed = .FALSE. 
                    endif
                end do
                if (.NOT. (compare_epsilon(X(i), Y(i), eps8))) then 
                    passed = .FALSE. 
                endif
            end do
        endif
    end function gj_elim_test_8
    function gj_elim_test_16(A, X, B, Y) result(passed)
        use matrix_operations, only : gj_elim
        real (kind = 16), allocatable :: A(:,:), X(:), B(:,:), Y(:)
        integer :: i, j
        logical :: passed
        passed = .FALSE.
        if (allocated(A) .AND. allocated(B) .AND. allocated(X) .AND. allocated(Y)) then
            call gj_elim(A,X)
            passed = .TRUE.
            do i = LBOUND(A,1),UBOUND(A,1)
                do j = LBOUND(A,1),UBOUND(A,1)
                    if (.NOT. (compare_epsilon(A(i,j), B(i,j), eps16))) then 
                        passed = .FALSE. 
                    endif
                end do
                if (.NOT. (compare_epsilon(X(i), Y(i), eps16))) then 
                    passed = .FALSE. 
                endif
            end do
        endif
    end function gj_elim_test_16

end module tests

program main
    use tests
    real (kind = 4), allocatable :: A4(:,:), X4(:), B4(:,:), Y4(:)
    real (kind = 8), allocatable :: A8(:,:), X8(:), B8(:,:), Y8(:)
    real (kind = 16), allocatable :: A16(:,:), X16(:), B16(:,:), Y16(:)
    
    write(*,*) "-------------------------------------------------------------------"
    write(*,*) "epsilon (kind = 4) =>", eps4
    write(*,*) "epsilon (kind = 8) =>", eps8
    write(*,*) "epsilon (kind = 16) =>", eps16
    write(*,*) "-------------------------------------------------------------------"
    write(*,*) 
    write(*,*) "-------------------------------------------------------------------"
    write (*,*) "GAUSS-JORDAN ELIMINATION TESTS (T = PASSED | F = FAILED)"
    write(*,*) "-------------------------------------------------------------------"    
    allocate(A4(3,3))
    allocate(B4(3,3))
    allocate(X4(3))
    allocate(Y4(3))    
    A4(:,1) = [1,1,1]
    A4(:,2) = [2,3,5]
    A4(:,3) = [4,0,5]
    X4(:) = [5,8,2]
    B4(:,1) = [1,0,0]
    B4(:,2) = [0,1,0]
    B4(:,3) = [0,0,1]
    Y4(:) = [3,4,-2]

    write (*,*) "3x3 | kind =  4 |", gj_elim_test(A4,X4,B4,Y4)
    
    if (allocated(A4)) deallocate(A4)
    if (allocated(B4)) deallocate(B4)
    if (allocated(X4)) deallocate(X4)
    if (allocated(Y4)) deallocate(Y4)

    allocate(A8(3,3))
    allocate(B8(3,3))
    allocate(X8(3))
    allocate(Y8(3))    
    A8(:,1) = [1,1,1]
    A8(:,2) = [2,3,5]
    A8(:,3) = [4,0,5]
    X8(:) = [5,8,2]
    B8(:,1) = [1,0,0]
    B8(:,2) = [0,1,0]
    B8(:,3) = [0,0,1]
    Y8(:) = [3,4,-2]

    write (*,*) "3x3 | kind =  8 |", gj_elim_test(A8,X8,B8,Y8)
    
    if (allocated(A8)) deallocate(A8)
    if (allocated(B8)) deallocate(B8)
    if (allocated(X8)) deallocate(X8)
    if (allocated(Y8)) deallocate(Y8)

    allocate(A16(3,3))
    allocate(B16(3,3))
    allocate(X16(3))
    allocate(Y16(3))    
    A16(:,1) = [1,1,1]
    A16(:,2) = [2,3,5]
    A16(:,3) = [4,0,5]
    X16(:) = [5,8,2]
    B16(:,1) = [1,0,0]
    B16(:,2) = [0,1,0]
    B16(:,3) = [0,0,1]
    Y16(:) = [3,4,-2]

    write (*,*) "3x3 | kind = 16 |", gj_elim_test(A16,X16,B16,Y16)
    
    if (allocated(A16)) deallocate(A16)
    if (allocated(B16)) deallocate(B16)
    if (allocated(X16)) deallocate(X16)
    if (allocated(Y16)) deallocate(Y16)

    write(*,*) "-------------------------------------------------------------------"

    allocate(A4(4,4))
    allocate(B4(4,4))
    allocate(X4(4))
    allocate(Y4(4))    
    
    A4(:,1) = [1,1,2,0]
    A4(:,2) = [2,-1,0,1]
    A4(:,3) = [1,-1,-1,-2]
    A4(:,4) = [2,-1,2,-1]
    X4(:) = [1,-2,4,0]
    B4(:,1) = [1,0,0,0]
    B4(:,2) = [0,1,0,0]
    B4(:,3) = [0,0,1,0]
    B4(:,4) = [0,0,0,1]
    Y4(:) = [1,2,-1,-2]
    
    write (*,*) "4x4 | kind =  4 |", gj_elim_test(A4,X4,B4,Y4)    
    
    if (allocated(A4)) deallocate(A4)
    if (allocated(B4)) deallocate(B4)
    if (allocated(X4)) deallocate(X4)
    if (allocated(Y4)) deallocate(Y4)

    allocate(A8(4,4))
    allocate(B8(4,4))
    allocate(X8(4))
    allocate(Y8(4))    
    
    A8(:,1) = [1,1,2,0]
    A8(:,2) = [2,-1,0,1]
    A8(:,3) = [1,-1,-1,-2]
    A8(:,4) = [2,-1,2,-1]
    X8(:) = [1,-2,4,0]
    B8(:,1) = [1,0,0,0]
    B8(:,2) = [0,1,0,0]
    B8(:,3) = [0,0,1,0]
    B8(:,4) = [0,0,0,1]
    Y8(:) = [1,2,-1,-2]
    
    write (*,*) "4x4 | kind =  8 |", gj_elim_test(A8,X8,B8,Y8)    
    
    if (allocated(A8)) deallocate(A8)
    if (allocated(B8)) deallocate(B8)
    if (allocated(X8)) deallocate(X8)
    if (allocated(Y8)) deallocate(Y8)

    allocate(A16(4,4))
    allocate(B16(4,4))
    allocate(X16(4))
    allocate(Y16(4))    
    
    A16(:,1) = [1,1,2,0]
    A16(:,2) = [2,-1,0,1]
    A16(:,3) = [1,-1,-1,-2]
    A16(:,4) = [2,-1,2,-1]
    X16(:) = [1,-2,4,0]
    B16(:,1) = [1,0,0,0]
    B16(:,2) = [0,1,0,0]
    B16(:,3) = [0,0,1,0]
    B16(:,4) = [0,0,0,1]
    Y16(:) = [1,2,-1,-2]
    
    write (*,*) "4x4 | kind = 16 |", gj_elim_test(A16,X16,B16,Y16)    
    
    if (allocated(A16)) deallocate(A16)
    if (allocated(B16)) deallocate(B16)
    if (allocated(X16)) deallocate(X16)
    if (allocated(Y16)) deallocate(Y16)

    write(*,*) "-------------------------------------------------------------------"

end program  main