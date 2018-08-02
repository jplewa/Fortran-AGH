! ifort euler51.f90 -o i_euler51 -std08 -module . -free -implicitnone -cpp -warn all -check bounds -O2
! gfortran euler51.f90 -o g_euler51 -ffree-form -std=f2008 -fimplicit-none -cpp -Wall -pedantic -fbounds-check -O2 -pthread

function is_prime(number) result (ans)
    integer :: number, ans, i
    ans = 1
    if (number < 2) then 
        ans = 0
    else if (number == 2) then
        ans = 1
    else if (modulo(number, 2) == 0) then 
        ans = 0
    else    
        i = 3
        do while (i*i <= number)
            if (modulo(number, i) == 0) then
                ans = 0
            end if
            i = i+2
        end do
    end if
end function is_prime

subroutine array_to_number(array, size, number)
    integer, intent(in) :: size, array(size)
    integer, intent(out) :: number
    integer :: i

    number = 0

    do i = 1, size
        number = number*10
        number = number + array(i)
    end do

end subroutine array_to_number

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
   
function replace (array, in_digit, out_digit, length) result (ans)
    integer, intent(in) :: in_digit, out_digit, length, array(length)
    integer :: new_array(length), ans, i

    do i = 1, length
        if (array(i) == in_digit) then
            new_array(i) = out_digit
        else
            new_array(i) = array(i)
        end if
    end do
    call array_to_number(new_array, length, ans)
end function replace

program main
    integer :: i, j, k, tmp, count, lowest, replace, length, number_length, is_prime
    integer :: digits(0:9)
    integer, allocatable :: array(:)
    count = 0
    do i = 10, 1000000
        lowest = i
        if (is_prime(i) == 1) then
            length = number_length(i)
            if (allocated(array)) then
                if (size(array) .NE. length) then
                    deallocate(array) 
                    allocate(array(length))
                end if
            else
                allocate(array(length))
            end if
            call number_to_array(i, array, length)
            call count_digits(length, array, digits)
            do j = 0, 9
                if (digits(j) > 0) then
                    lowest = i
                    count = 1
                    do k = 0, 9
                        if (k .NE. j) then
                            tmp = replace(array, j, k, length)
                            if (number_length(tmp) == length) then
                                if (is_prime(tmp) == 1) then
                                    count = count+1
                                    if (tmp < lowest) then
                                        lowest = tmp
                                    end if
                                end if
                            end if
                        end if
                    end do
                    if (count == 8) then
                        write (*,*) lowest
                        stop 0
                    end if
                end if
            end do
        end if
    end do

end program main