module mult

    implicit none

contains

    subroutine mult1(m1, m2, result, error)

        real, intent(in) :: m1(:, :), m2(:, :)
        real, intent(out) :: result(:, :)
        integer, intent(out) :: error
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

        if ((shape3(2) .NE. shape1(2)) .OR. (shape3(1) .NE. shape2(1))) then
            error = 2
            return
        endif

        do j = 1, shape2(1)
            do k = 1, shape1(1)
                do i = 1, shape1(2)
                    result(i,j) = result(i,j) + m1(k,j) * m2(i,k)
                end do
            end do  
        end do

    end subroutine

    subroutine mult2(m1, m2, result, error)

        real, intent(in) :: m1(:, :), m2(:, :)
        real, allocatable :: m1T(:, :)
        real, intent(out) :: result(:, :)
        integer, intent(out) :: error
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

        if ((shape3(2) .NE. shape1(2)) .OR. (shape3(1) .NE. shape2(1))) then
            error = 2
            return
        endif

        allocate(m1T(shape1(2), shape1(1)))
        m1T = TRANSPOSE(m1)

        do j = 1, shape2(1)
            do k = 1, shape1(1)
                do i = 1, shape1(2)
                    result(i,j) = result(i,j) + m1T(j,k) * m2(i,k)
                end do
            end do  
        end do

        if (ALLOCATED(m1T)) deallocate(m1T)

    end subroutine

    subroutine mult3(m1, m2, result, error)

        real, intent(in) :: m1(:, :), m2(:, :)
        real, intent(out) :: result(:, :)
        integer, intent(out) :: error
        integer :: i, j, shape1(2), shape2(2), shape3(2)

        result = 0.d0
        error = 0

        shape1 = shape(m1)
        shape2 = shape(m2)
        shape3 = shape(result)

        if (shape1(1) .NE. shape2(2)) then
            error = 1
            return
        endif

        if ((shape3(2) .NE. shape1(2)) .OR. (shape3(1) .NE. shape2(1))) then
            error = 2
            return
        endif

        do j = 1, shape2(1)
            do i = 1, shape1(2)
                result(i,j) = DOT_PRODUCT(m1(:, j), m2(i,:))
            end do  
        end do

    end subroutine

    subroutine mult4(m1, m2, result, error)

        real, intent(in) :: m1(:, :), m2(:, :)
        real, intent(out) :: result(:, :)
        integer, intent(out) :: error
        integer :: shape1(2), shape2(2), shape3(2)
        integer(kind = 4) :: i, j, k, jj, kk
        integer (kind = 4) :: ichunk
        
        result = 0.d0
        error = 0
        
        shape1 = shape(m1)
        shape2 = shape(m2)
        shape3 = shape(result)
        
        if (shape1(1) .NE. shape2(2)) then
            error = 1
            return
        endif
        
        if ((shape3(2) .NE. shape1(2)) .OR. (shape3(1) .NE. shape2(1))) then
            error = 2
            return
        endif

        ! use -funroll-loops
        ichunk = 512 ! I have a 3MB cache size (real*4)
        do jj = 1, shape2(1), ichunk
           do kk = 1, shape1(1), ichunk

              do j = jj, min(jj + ichunk - 1, shape2(1))
                 do k = kk, min(kk + ichunk - 1, shape1(1))
                    do i = 1, shape1(2)
                       result(i, j) = result(i, j) + m1(k,j) * m2(i,k)
                    end do
                 end do
              end do

           end do
        end do

    end subroutine

end module