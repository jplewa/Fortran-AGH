program pi_test
    implicit none
    integer :: i, N, parse_result
    character (len = 10) :: arg(1)
    real (kind = 8), codimension[*] :: p
    if (command_argument_count() .NE. 1) then
        write(*,*) "Error: missing program arguments"
        error stop
    end if
    call get_command_argument(1, arg(1))
    read(arg(1), *, iostat = parse_result) N

    p = 0.d0
    do i=1+(this_image()-1)*N/num_images(),this_image()*N/num_images()
        p = p + ((-1.d0)**(i+1))/(2*i-1)
    end do
    p = p*4.d0
    syncall()
    if (this_image() .EQ. 1) then
        do i = 2, num_images()
            p[1] = p[1] + p[i]
        end do
        write(*,*) this_image(), p
    endif

 end program
 