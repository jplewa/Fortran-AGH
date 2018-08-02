!------------------------------------------------------------------------------
! MODULE: sequential_mat_ops
!
!> @author
!> Julia Plewa
!
! DESCRIPTION: 
!> This module contains sequential matrix operations.
!
! REVISION HISTORY:
! 18 06 2018 - Initial version
!------------------------------------------------------------------------------

module sequential_mat_ops
    
    implicit none

    contains

    !—————————————————————————
    !> @author
    !> Julia Plewa
    !
    ! DESCRIPTION:
    !> Sequential Gauss-Jordan elimination algorithm.
    !> 
    !> 
    !
    ! REVISION HISTORY:
    ! 19 06 2018 - Initial Version
    !
    !> @param[inout] A - coefficient matrix of size NxN
    !> @param[inout] X - constant term vector of size N
    !> @param[in] N - size
    !—————————————————————————

    subroutine gauss(A, X, N)!B, Y, N)
        integer :: i, j
        integer, intent(in) :: N
        real (kind = 8), intent(inout) :: A(N,N), X(N)
        real (kind = 8) :: c
        !f2py intent(in,out) :: A, X, N

        do i = 1, N
            do j = 1, N
                if (i .NE. j) then
                    c = A(i,j)/A(i,i)
                    A(:,j) = A(:,j) - c*A(:,i)
                    X(j) = X(j) - c*X(i)
                    X(i) = X(i)/A(i,i)
                    A(:,i) = A(:,i)/A(i,i)
                endif
            end do
        end do 

    end subroutine

    !—————————————————————————
    !> @author
    !> Julia Plewa
    !
    ! DESCRIPTION:
    !> Sequential square matrix multiplication algorithm.
    !> 
    !> 
    !
    ! REVISION HISTORY:
    ! 19 06 2018 - Initial Version
    !
    !> @param[in] m1 - first matrix of size NxN
    !> @param[in] m2 - second matrix of size NxN
    !> @param[in] N - size of matrices
    !> @param[out] result - resulting matrix
    !—————————————————————————

    subroutine mm(m1, m2, N, result)
        integer, intent(in) :: N
        real (kind = 8), intent(in) :: m1(N,N), m2(N,N)
        real (kind = 8), intent(out) :: result(N,N)
        integer (kind = 4) :: i, j, k

        !f2py intent(in) :: m1, m2, N
        !f2py intent(out) :: result

        result = 0.d0

        do j = 1, N
            do k = 1, N
                do i = 1, N
                    result(i,j) = result(i,j) + m1(i,k) * m2(k,j)
                end do
            end do  
        end do
    end subroutine 

end module