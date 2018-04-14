! rozwiazanie mrsu przyrownujemy do wartosci dokladnej
! wykres
! kind jako paramenter?
module gauss_elim
    implicit none

contains

    subroutine jg_elim(N, A, X)
        integer :: N, i, j
        real :: A(N,N), X(N), c

        do i = 1,N
            do j = 1,N
                if (I .NE. J) then
                    c = A(i,j)/A(i,i)
                    A(:,j) = A(:,j) - c*A(:,i)
                    X(j) = X(j) - c*X(i)
                    X(i) = X(i)/A(i,i)
                    A(:,i) = A(:,i)/A(i,i)
                endif
            end do
        end do 

    end subroutine jg_elim

end module gauss_elim


module gauss_elim_tests

contains
    subroutine jg_elim_test
        use gauss_elim, only : jg_elim
        real :: A(3,3), X(3), B(3,3), Y(3)
        integer :: i, j
        logical :: passed
        A(:,1) = [1,1,1]
        A(:,2) = [2,3,5]
        A(:,3) = [4,0,5]
        X(:) = [5,8,2]
        call jg_elim(3,A,X)
        passed = .TRUE.
        B(:,1) = [1,0,0]
        B(:,2) = [0,1,0]
        B(:,3) = [0,0,1]
        Y(:) = [3,4,-2]
        do i=1,3
            do j=1,3
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
    end subroutine jg_elim_test

end module gauss_elim_tests


program main
    use gauss_elim_tests, only : jg_elim_test
    call jg_elim_test
end program main