!------------------------------------------------------------------------------
! MODULE: parallel_mat_ops
!
!> @author
!> Julia Plewa
!
! DESCRIPTION: 
!> This module contains parallel matrix operations.
!
! REVISION HISTORY:
! 18 06 2018 - Initial version
!------------------------------------------------------------------------------

module parallel_mat_ops
    
    implicit none

    contains

    !—————————————————————————
    !> @author
    !> Julia Plewa
    !
    ! DESCRIPTION:
    !> Parallel square matrix multiplication algorithm.
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
        real (kind = 8), allocatable, codimension[:] :: n1(:,:), n2(:,:)
        real (kind = 8), allocatable, codimension[:] :: tmp(:,:)
        real (kind = 8), intent(out) :: result(N,N)
        integer (kind = 4) :: i, j, k

        allocate(n1((1+(this_image()-1)*N/num_images()):(this_image()*N/num_images()) ,N)[*])
        allocate(n2(N, N)[*])
        allocate(tmp(N,N)[*])

        n1 = m1((1+(this_image()-1)*N/num_images()):(this_image()*N/num_images()), :)
        n2 = m2(:, :)
        tmp = 0.d0

        do j = 1, N
            do k = 1, N
                do i = 1+(this_image()-1)*N/num_images(),this_image()*N/num_images()
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
        
        if (ALLOCATED(n1)) deallocate(n1)
        if (ALLOCATED(n2)) deallocate(n2)
        if (ALLOCATED(tmp)) deallocate(tmp)

    end subroutine 

end module