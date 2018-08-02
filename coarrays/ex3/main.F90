subroutine test_parallel(N)
    use parallel_mat_ops
    integer, intent(in) :: N
    integer (kind = 8) :: before, after, ticks
    real (kind = 8) :: m1(N,N), m2(N,N), result(N,N)
    call RANDOM_NUMBER(m1)
    call RANDOM_NUMBER(m2)
    call system_clock(before,ticks)
    syncall()
    call mm(m1,m2,N,result)
    syncall()
    call system_clock(after)
    if (THIS_IMAGE() .eq. 1) then 
        write (*,*) "parallel:", (real((after-before))/ticks)
    endif
end subroutine

subroutine test_sequential(N)
    use sequential_mat_ops
    integer, intent(in) :: N
    integer (kind = 8) :: before, after, ticks
    real (kind = 8) :: m1(N,N), m2(N,N), result(N,N)
    if (THIS_IMAGE() .EQ. 1) then
        call RANDOM_NUMBER(m1)
        call RANDOM_NUMBER(m2)
        call system_clock(before,ticks)
        call mm(m1,m2,N,result)
        call system_clock(after)
        write (*,*) "sequential:", (real((after-before))/ticks)
    endif
end subroutine

program main
    use parallel_mat_ops
    integer :: i
    
    do i = 100, 2500, 200
        if (THIS_IMAGE() .EQ. 1) then
            write (*,*) "---------------------------"
            write (*,*) i
            write (*,*) "---------------------------"
        endif
        call test_parallel(i)
        call test_sequential(i)
    enddo

end program main