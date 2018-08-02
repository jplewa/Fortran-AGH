function number_length(number) result(length)
    integer, intent(in) :: number
    integer :: length, tmp
    length = 0
    tmp = number
    do while (tmp > 0)
        length = length+1
        tmp = tmp/10
    end do
end function number_length

subroutine count_digits(length, number_array, digit_array)
    integer, intent (out) :: digit_array(0:9)
    integer, intent(in) :: length, number_array(length)
    integer :: i
    digit_array = 0
    do i=1, length
        digit_array(number_array(i)) = digit_array(number_array(i)) +1
    end do
end subroutine count_digits

subroutine number_to_array(number, array, length)
    integer, intent(in) :: number, length
    integer :: tmp, i
    integer, intent(out) :: array(length)
    tmp = number
    do i=1, length
        array(length-i+1) = modulo(tmp, 10)
        tmp = tmp/10
    end do
end subroutine number_to_array

program main
    integer :: digits1(0:9), digits2(0:9), i, length, j, found, number_length, k
    integer, allocatable :: array(:), array2(:)
    logical :: digits(0:9)
    do i=10, 10000000
        found = 1
        length = number_length(i)
        if (allocated(array)) then
            if (size(array) .NE. length) then
                deallocate(array) 
                allocate(array(length))
                deallocate(array2) 
                allocate(array2(length))
            end if
        else
            allocate(array(length))
            allocate(array2(length))
        end if
        call number_to_array(i, array, length)
        call count_digits(length, array, digits1)
        do j = 2, 6
            if (number_length(i*j) == length) then
                call number_to_array(i*j, array2, length)
                call count_digits(length, array2, digits2)
                digits = digits1 .EQ. digits2
                do k=0, 9
                    if (digits(k) .eqv. .FALSE.) then
                        found = 0
                    end if
                end do
            else
                found = 0
            end if
        end do
        if (found == 1) then
            write (*,*) i
            stop 0
        end if
    end do
end program main