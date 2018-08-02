module tests

contains
    function compare_epsilon(a, b, e) result(x)
        real :: a, b, e
        LOGICAL :: x
        if (ABS(a - b) < e) then
            x = .TRUE.
        else 
            x = .FALSE.
        endif
    end function compare_epsilon

    subroutine jg_elim_test
        use gauss_elim, only : jg_elim
        real, allocatable :: A(:,:), X(:)
        real :: B(3,3), Y(3)
        integer :: i, j
        logical :: passed
        allocate(A(3,3))
        allocate(X(3))
        A(:,1) = [1,1,1]
        A(:,2) = [2,3,5]
        A(:,3) = [4,0,5]
        X(:) = [5,8,2]
        call jg_elim(A,X,1,3)
        passed = .TRUE.
        B(:,1) = [1,0,0]
        B(:,2) = [0,1,0]
        B(:,3) = [0,0,1]
        Y(:) = [3,4,-2]
        do i = 1,3
            do j = 1,3
                write (*,*) abs((A(i,j) - B(i,j)))
                if (A(i,j) .NE. B(i,j)) then 
                    passed = .FALSE. 
                endif
            end do
        end do
        if (passed) then 
            write (*,*) "jg_elim_test1 passed" 
        else 
            write (*,*) "jg_elim_test1 failed" 
        endif
        if (allocated(A)) deallocate(A) 
        if (allocated(X)) deallocate(X) 
    end subroutine jg_elim_test

end module tests

