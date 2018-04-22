program main
    use heat_equation, only : average_error
    integer :: j
    real(kind = 4) :: a4 = 0
    real(kind = 8) :: a8 = 0
    real(kind = 16) :: a16 = 0
    do j = 0,3
        call average_error(10**j, a4)
        call average_error(10**j, a8)
        call average_error(10**j, a16)
        write (*,*) "--------------------------------------------------------------"
        write (*,*) "N:", (10**j)
        write (*,*) "--------------------------------------------------------------"
        write (*,*) "kind =  4 | error =", a4
        write (*,*) "kind =  8 | error =", a8
        write (*,*) "kind = 16 | error =", a16
    end do    
    write (*,*) "--------------------------------------------------------------"
end program main