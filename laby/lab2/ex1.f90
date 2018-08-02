program main
    real (kind = 8), allocatable :: x(:), fx(:), sums(:)
    integer (kind = 4), parameter :: isize = 100000000
    integer :: i
    real (kind = 8) :: ibeg, iend, int
    allocate(x(isize))
    allocate(fx(isize))
    allocate(sum(isize-1))

    read (*,*) ibeg
    read (*,*) iend

    do i=1, isize
        x(i) = ibeg + real((i-1), kind=8)*(iend - ibeg)/real(isize-1, kind=8)
    end do
    fx = f(x)

    int = integral(x)
    write (*,*) int

    contains



    function f(x) result (ans)
        real (kind = 8) :: x, ans
        ans = (x**3) + (5 * (x**2)) + (10 * x) - 15
    end function f
    
    function integral (x) result (val)
        real (kind = 8), intent(in) :: x(isize)
        real (kind = 8) :: val
        val = 0
        do i=1, isize-1
            val = val + (f((x(i+1) + x(i))/2) * (x(i+1) - x(i)))
        end do
    end function integral

    subroutine integral2(x, ans)

end program main