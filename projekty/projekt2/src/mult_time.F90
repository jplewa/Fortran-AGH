program main

    use mult, only : mm1, mm2, mm3, mm4

    implicit none

    integer (kind = 4) :: i, error
    integer (kind = 8) :: before, after, ticks
    real (kind = 8), allocatable :: m1(:,:), m2(:,:), result(:,:)
    
    open(unit = 1, file = "./out/results/mm1")
    open(unit = 2, file = "./out/results/mm2")
    open(unit = 3, file = "./out/results/mm3")
    open(unit = 4, file = "./out/results/mm4")
    open(unit = 5, file = "./out/results/matmul")
    open(unit = 6, file = "./out/results/N")
    
    do i = 1, 1001, 20
    
        write (6, *) (i)
    
        allocate(m1(i,i))
        allocate(m2(i,i))
        allocate(result(i,i))
    
        call RANDOM_NUMBER(m1)
        call RANDOM_NUMBER(m2)
    
        call system_clock(before)
        call mm1(m1,m2,result,error)
        call system_clock(after)
        write (1,*) (after-before)
    
        call system_clock(before)
        call mm2(m1,m2,result,error)
        call system_clock(after)
        write (2,*) (after-before)
    
        call system_clock(before)
        call mm3(m1,m2,result,error)
        call system_clock(after)
        write (3,*) (after-before)

        call system_clock(before)
        call mm4(m1,m2,result,error)
        call system_clock(after)
        write (4,*) (after-before)

        call system_clock(before)
        result = MATMUL(m1,m2)
        call system_clock(after)
        write (5,*) (after-before)
    
        if (allocated(m1)) deallocate(m1)        
        if (allocated(m2)) deallocate(m2)
        if (allocated(result)) deallocate(result)
        
    end do     

    close(1)
    close(2)
    close(3)
    close(4)
    close(5)
    close(6)

    open(unit = 7, file = "./out/results/ticks")
    call system_clock(before, ticks)
    write(7,*) ticks
    close(7)

end program main