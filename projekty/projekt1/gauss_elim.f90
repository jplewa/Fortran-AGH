module gauss_elim
    implicit none
    interface jg_elim
        module procedure jg_elim_1, jg_elim_2, jg_elim_3
    end interface jg_elim

contains
   
    subroutine jg_elim_1(A, X, M, N)
        integer :: i, j, M, N
        real (kind = 4), allocatable :: A(:,:), X(:)
        real (kind = 4) :: c

        if (allocated(A) .AND. allocated(X)) then
            do i = M, N
                do j = M, N
                    if (i .NE. j) then
                        c = A(i,j)/A(i,i)
                        A(:,j) = A(:,j) - c*A(:,i)
                        X(j) = X(j) - c*X(i)
                        X(i) = X(i)/A(i,i)
                        A(:,i) = A(:,i)/A(i,i)
                    endif
                end do
            end do 
        endif
    end subroutine jg_elim_1

    subroutine jg_elim_2(A, X, M, N)
        integer :: i, j, M, N
        real (kind = 8), allocatable :: A(:,:), X(:)
        real (kind = 8) :: c

        if (allocated(A) .AND. allocated(X)) then
            do i = M, N
                do j = M, N
                    if (i .NE. j) then
                        c = A(i,j)/A(i,i)
                        A(:,j) = A(:,j) - c*A(:,i)
                        X(j) = X(j) - c*X(i)
                        X(i) = X(i)/A(i,i)
                        A(:,i) = A(:,i)/A(i,i)
                    endif
                end do
            end do 
        endif
    end subroutine jg_elim_2

    subroutine jg_elim_3(A, X, M, N)
        integer :: i, j, M, N
        real (kind = 16), allocatable :: A(:,:), X(:)
        real (kind = 16) :: c

        if (allocated(A) .AND. allocated(X)) then
            do i = M, N
                do j = M, N
                    if (i .NE. j) then
                        c = A(i,j)/A(i,i)
                        A(:,j) = A(:,j) - c*A(:,i)
                        X(j) = X(j) - c*X(i)
                        X(i) = X(i)/A(i,i)
                        A(:,i) = A(:,i)/A(i,i)
                    endif
                end do
            end do 
        endif
    end subroutine jg_elim_3
end module gauss_elim