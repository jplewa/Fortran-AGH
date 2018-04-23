module matrix_operations
    implicit none
    ! generic interface for Gauss-Jordan elimination for different precision levels
    ! works with allocatable matrices
    interface gj_elim
        module procedure gj_elim_4, gj_elim_8, gj_elim_16
    end interface gj_elim

contains
   
    subroutine gj_elim_4(A, X)
        integer :: i, j
        real (kind = 4), allocatable :: A(:,:), X(:)
        real (kind = 4) :: c
        if (allocated(A) .AND. allocated(X)) then
            do i = LBOUND(A,1), UBOUND(A,1)
                do j = LBOUND(A,2), UBOUND(A,2)
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
    end subroutine gj_elim_4

    subroutine gj_elim_8(A, X)
        integer :: i, j
        real (kind = 8), allocatable :: A(:,:), X(:)
        real (kind = 8) :: c
        if (allocated(A) .AND. allocated(X)) then
            do i = LBOUND(A,1), UBOUND(A,1)
                do j = LBOUND(A,2), UBOUND(A,2)
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
    end subroutine gj_elim_8

    subroutine gj_elim_16(A, X)
        integer :: i, j
        real (kind = 16), allocatable :: A(:,:), X(:)
        real (kind = 16) :: c
        if (allocated(A) .AND. allocated(X)) then
            do i = LBOUND(A,1), UBOUND(A,1)
                do j = LBOUND(A,2), UBOUND(A,2)
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
    end subroutine gj_elim_16

end module matrix_operations