#define ichunk 512

module mult

    implicit none

contains

    subroutine mm1(m1, m2, result, error)

        real (kind = 8), intent(in) :: m1(:, :), m2(:, :)
        real (kind = 8), intent(out) :: result(:, :)
        integer (kind = 4), intent(out) :: error
        integer (kind = 4) :: i, j, k, shape1(2), shape2(2), shape3(2)

        result = 0.d0
        error = 0

        shape1 = shape(m1)
        shape2 = shape(m2)
        shape3 = shape(result)

        if (shape1(2) .NE. shape2(1)) then
            error = 1
            return
        endif
        if ((shape3(1) .NE. shape1(1)) .OR. (shape3(2) .NE. shape2(2))) then
            error = 2
            return
        endif

        do j = 1, shape3(2)
            do k = 1, shape1(2)
                do i = 1, shape3(1)
                    result(i,j) = result(i,j) + m1(i,k) * m2(k,j)
                end do
            end do  
        end do

    end subroutine

    subroutine mm2(m1, m2, result, error)

        real (kind = 8), intent(in) :: m1(:, :), m2(:, :)
        real (kind = 8), intent(out) :: result(:, :)
        integer (kind = 4), intent(out) :: error
        integer (kind = 4) :: i, j, shape1(2), shape2(2), shape3(2)

        result = 0.d0
        error = 0

        shape1 = shape(m1)
        shape2 = shape(m2)
        shape3 = shape(result)

        if (shape1(2) .NE. shape2(1)) then
            error = 1
            return
        endif
        if ((shape3(1) .NE. shape1(1)) .OR. (shape3(2) .NE. shape2(2))) then
            error = 2
            return
        endif

        do j = 1, shape3(2)
            do i = 1, shape3(1)
                result(i,j) = DOT_PRODUCT(m1(i,:),m2(:,j))
            end do
        end do

    end subroutine

    subroutine mm3(m1, m2, result, error)

        real (kind = 8), intent(in) :: m1(:, :), m2(:, :)
        real (kind = 8), intent(out) :: result(:, :)
        integer (kind = 4), intent(out) :: error
        integer (kind = 4) :: i, ii, j, jj, k, shape1(2), shape2(2), shape3(2)

        result = 0.d0
        error = 0

        shape1 = shape(m1)
        shape2 = shape(m2)
        shape3 = shape(result)

        if (shape1(2) .NE. shape2(1)) then
            error = 1
            return
        endif
        if ((shape3(1) .NE. shape1(1)) .OR. (shape3(2) .NE. shape2(2))) then
            error = 2
            return
        endif

        do jj = 1, shape3(2), ichunk
            do ii = 1, shape3(1), ichunk
            
                    do j = jj, min(jj + ichunk - 1, shape3(2))
                        do i = ii, min(ii + ichunk - 1, shape3(1))
                            do k = 1, shape1(2)
                                result(i, j) = result(i, j) + m1(i, k) * m2(k, j)
                            end do
                        end do
                    end do
                
                
            end do
        end do

    end subroutine

    subroutine mm4(m1, m2, result, error)

        real (kind = 8), intent(in) :: m1(:, :), m2(:, :)
        real (kind = 8), intent(out) :: result(:, :)
        integer (kind = 4), intent(out) :: error
        integer (kind = 4) :: i, ii, j, jj, shape1(2), shape2(2), shape3(2)

        result = 0.d0
        error = 0

        shape1 = shape(m1)
        shape2 = shape(m2)
        shape3 = shape(result)

        if (shape1(2) .NE. shape2(1)) then
            error = 1
            return
        endif
        if ((shape3(1) .NE. shape1(1)) .OR. (shape3(2) .NE. shape2(2))) then
            error = 2
            return
        endif

        do jj = 1, shape3(2), ichunk      
            do ii = 1, shape3(1), ichunk
                do j = jj, min(jj + ichunk - 1, shape3(2))
                    do i = ii, min(ii + ichunk - 1, shape3(1))
                        result(i, j) = DOT_PRODUCT(m1(i, :), m2(:, j))
                    end do
                end do
            
            end do
        end do

    end subroutine

end module