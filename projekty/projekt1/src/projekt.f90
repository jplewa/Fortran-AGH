program main
    use heat_equation, only : average_error
    integer :: N, M, j
    real(kind = 4) :: a4 = 0
    real(kind = 8) :: a8 = 0
    real(kind = 16) :: a16 = 0
    read (*,*) N, M
    open (unit = 1, file = "out/results/xAxis")
    open (unit = 2, file = "out/results/kind4")
    open (unit = 3, file = "out/results/kind8")
    open (unit = 4, file = "out/results/kind16")
    write(*,*) "-------------------------------------------------------------------"    
    write(*,*) "HEAT EQUATION SOLUTION ERROR"
    do j = 1, N, M
        call average_error(j, a4)
        call average_error(j, a8)
        call average_error(j, a16)
        write(*,*) "-------------------------------------------------------------------"    
        write (*,*) "N:", j
        write(*,*) "-------------------------------------------------------------------"    
        write (*,*) "kind =  4 | error =", a4
        write (*,*) "kind =  8 | error =", a8
        write (*,*) "kind = 16 | error =", a16
        write (1,*) j
        write (2,*) a4
        write (3,*) a8
        write (4,*) a16
    end do    
    write(*,*) "-------------------------------------------------------------------"    
    close(1)
    close(2)
    close(3)
    close(4)
    end program main