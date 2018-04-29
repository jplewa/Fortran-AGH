program main
    use heat_equation, only : average_error
    integer :: N, M, i, parse_result
    real (kind = 4) :: a4 = 0
    real (kind = 8) :: a8 = 0
    real (kind = 16) :: a16 = 0
    character (len = 10) :: arg(2)
    if (command_argument_count() .NE. 2) then
        write(*,*) "Error: missing program arguments"
        error stop
    end if
    call get_command_argument(1, arg(1))
    call get_command_argument(2, arg(2))
    read(arg(1), *, iostat = parse_result) N
    if (parse_result .NE. 0) then
        write(*,*) "Error: incorrect program arguments"
        error stop
    end if
    read(arg(2), *, iostat = parse_result) M
    if (parse_result .NE. 0) then
        write(*,*) "Error: incorrect program arguments"
        error stop
    end if
    open(unit = 1, file = "out/results/xAxis")
    open(unit = 2, file = "out/results/kind4")
    open(unit = 3, file = "out/results/kind8")
    open(unit = 4, file = "out/results/kind16")
    write(*,*) "-------------------------------------------------------------------"    
    write(*,*) "HEAT EQUATION SOLUTION ERROR"
    do i = 1, N, M
        call average_error(i, a4)
        call average_error(i, a8)
        call average_error(i, a16)
        write(*,*) "-------------------------------------------------------------------"    
        write(*,*) "N:", i
        write(*,*) "-------------------------------------------------------------------" 
        write(*,*) "kind =  4 | error =", a4
        write(*,*) "kind =  8 | error =", a8
        write(*,*) "kind = 16 | error =", a16
        write(1,*) i
        write(2,*) a4
        write(3,*) a8
        write(4,*) a16
    end do    
    write (*,*) "-------------------------------------------------------------------"    
    close(1)
    close(2)
    close(3)
    close(4)
    end program main