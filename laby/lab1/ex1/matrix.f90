program main
    integer :: array(10,10), i, j

    do i=1, 10
        do j=1, 10
            array(i,j) = i
        end do
    end do

    write (*,*) array
end program main