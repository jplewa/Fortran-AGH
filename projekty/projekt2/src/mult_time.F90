program main

    use mult, only : mm1, mm2

    implicit none

    integer (kind = 4) :: i, error, before, after, ticks
    real (kind = 8), allocatable :: m1(:,:), m2(:,:), result(:,:)
    
    open(unit = 1, file = "./out/results/mm1")
    open(unit = 2, file = "./out/results/mm2")
    open(unit = 3, file = "./out/results/matmul")
    open(unit = 4, file = "./out/results/N")
    
    do i = 1, 1001, 10
    
        write (4, *) i
    
        allocate(m1(i,i))
        allocate(m2(i,i))
        allocate(result(i,i))
    
        call RANDOM_NUMBER(m1)
        call RANDOM_NUMBER(m2)
    
        call system_clock(before)
        call mm1(m1,m2,result,error)
        call system_clock(after)
        write (1,*) before, after
    
        call system_clock(before)
        call mm2(m1,m2,result,error)
        call system_clock(after)
        write (2,*) before, after
    
        call system_clock(before)
        result = MATMUL(m1,m2)
        call system_clock(after)
        write (3,*) before, after
    
        if (allocated(m1)) deallocate(m1)        
        if (allocated(m2)) deallocate(m2)
        if (allocated(result)) deallocate(result)
        
    end do     

    close(1)
    close(2)
    close(3)
    close(4)

    open(unit = 5, file = "./out/results/ticks")
    call system_clock(before, ticks)
    write(5,*) ticks
    close(5)

end program main