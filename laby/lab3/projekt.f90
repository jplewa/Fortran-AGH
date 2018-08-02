program main
    use heat_equation, only : average_error
    integer :: j
    real(kind = 4) :: k1 = 0
    real(kind = 8) :: k2 = 0
    real(kind = 16) :: k3 = 0
    do j = 0,3
        write (*,*) (10**j), 4, average_error(10**j, k1)
        write (*,*) (10**j), 8, average_error(10**j, k2)
        write (*,*) (10**j), 16, average_error(10**j, k3)

    end do    
end program main