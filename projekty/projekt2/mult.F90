module mult

    implicit none

contains

    subroutine mm1(m1, m2, result, error)

        real (kind = 8), intent(in) :: m1(:, :), m2(:, :)
        real (kind = 8), intent(out) :: result(:, :)
        integer (kind = 4), intent(out) :: error
        integer :: i, j, k, shape1(2), shape2(2), shape3(2)

        result = 0.d0
        error = 0

        shape1 = shape(m1)
        shape2 = shape(m2)
        shape3 = shape(result)

        if (shape1(1) .NE. shape2(2)) then
            error = 1
            return
        endif
        if ((shape3(1) .NE. shape2(1)) .OR. (shape3(2) .NE. shape1(2))) then
            error = 2
            return
        endif

        do i = 1, shape3(1)
            do j = 1, shape3(2)
                do k = 1, shape1(1)
                    result(i,j) = result(i,j) + m1(k,j) * m2(i,k)
                end do
            end do  
        end do

    end subroutine

end module