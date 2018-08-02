subroutine ins_sort(A, n)
    integer :: A(0:(n-1)), n, key, j
    
    do i = 1, (n-1)
        key = A(i)
        j = i - 1
        do while (A(j) > key .AND. j >= 0)
            A(j+1) = A(j)
            j = j - 1
        end do
        A(j+1) = key
    end do

end subroutine

program main2
    implicit none
    integer :: A(0:9)
    A = (/10,9,8,7,6,5,4,3,2,1/)
    call ins_sort(A, 10)
    write(*,*) A   
end program