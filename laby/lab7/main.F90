subroutine fun1(A, B, N)
    integer :: i
    integer, intent(in) :: N
    real, intent(inout) :: A(N), B(N)
    do i = 1,N
        A(i) = A(i)*B(i)
    end do
    S = SUM(A)
end subroutine

subroutine fun2(A, B, N)
    integer :: i
    integer, intent(in) :: N
    real, intent(inout) :: A(N), B(N)
    A = A*B
    S = SUM(A)
end subroutine

subroutine fun3(A, B, N)
    integer :: i
    integer, intent(in) :: N
    real, intent(inout) :: A(N), B(N)
    do concurrent (i = 1:N)
        A(i) = A(i)*B(i)
    end do
    S = SUM(A)
end subroutine

program main
    real :: A(100000000), B(100000000)
    integer :: before, after
    call RANDOM_NUMBER(A)
    call RANDOM_NUMBER(B)
    call system_clock(before)
    call fun1(A,B,100000000)
    call system_clock(after)
    write (*,*) (after-before)
    call system_clock(before)
    call fun2(A,B,100000000)
    call system_clock(after)
    write (*,*) (after-before)
    call system_clock(before)
    call fun3(A,B,100000000)
    call system_clock(after)
    write (*,*) (after-before)
end program main