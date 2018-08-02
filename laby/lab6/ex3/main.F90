program main
    use co_mat_ops
    real (kind = 8), volatile :: m1(2,2), m2(2,2), result(2,2)
    
    m1(1,:) = [1,2]
    m1(2,:) = [3,4]
    m2(1,:) = [2,0]
    m2(2,:) = [1,2]
    call mm(m1, m2, 2, result)
    if (THIS_IMAGE() .EQ. 1) then         
        write(*,*) result
    endif
    syncall()

end program main