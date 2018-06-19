module co_mat_ops
    
    implicit none

    contains

    !—————————————————————————
    !> @author
    !> Julia Plewa
    !
    ! DESCRIPTION:
    !> 
    !> 
    !> 
    !
    ! REVISION HISTORY:
    ! 19 06 2018 - Initial Version
    !
    !> @param[in] n - number of functions on the knot minus one
    !> @param[in] p - degree of polynomial
    !> @param[out] U - array to fill with points
    !> @param[out] nelem - number of elements
    !—————————————————————————

    subroutine gauss(A, X, N)!B, Y, N)
        integer :: i, j
        integer, intent(in) :: N
        real (kind = 8), intent(inout) :: A(N,N), X(N)
        real (kind = 8) :: c

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
    !> 
    !> 
    !> 
    !
    ! REVISION HISTORY:
    ! 19 06 2018 - Initial Version
    !
    !> @param[in] n - number of functions on the knot minus one
    !> @param[in] p - degree of polynomial
    !> @param[out] U - array to fill with points
    !> @param[out] nelem - number of elements
    !—————————————————————————

    subroutine mm(m1, m2, N, result)
        integer, intent(in) :: N
        real (kind = 8), intent(in) :: m1(N,N), m2(N,N)
        real (kind = 8), allocatable, codimension[:] :: n1(:,:), n2(:,:)
        real (kind = 8), allocatable, codimension[:] :: tmp(:,:)
        real (kind = 8), intent(out) :: result(N,N)
        integer (kind = 4) :: i, j, k

        syncall()
        allocate(n1(N,N)[*])
        allocate(n2(N,N)[*])
        allocate(tmp(N,N)[*])
        n1 = m1
        n2 = m2

        syncall()

        tmp = 0.d0

        do j = 1, N
            do i = 1+(this_image()-1)*N/num_images(),this_image()*N/num_images()
                do k = 1, N
                    tmp(i, j) = tmp(i, j) + n1(i,k)*n2(k,j)
                end do
            end do 
        end do
        
        syncall()

        if (THIS_IMAGE() .EQ. 1) then
            do j = 1, NUM_IMAGES()
                do i = 1+(j-1)*N/num_images(),j*N/num_images()
                    result(i,:) = tmp(i,:)[j]
                end do
            end do 
        endif

        syncall()

    end subroutine 

end module