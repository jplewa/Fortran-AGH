program gauss_test
    integer :: N
    integer :: i, j
    real, allocatable :: X(:), A(:,:), c
    N=10
    allocate(X(N))
    allocate(A(N,N))
    A(:,:) = 0.d0
    X(:) = 0.d0
    P1 = 1.d0*real(N, kind = 4)*real(N, kind = 4)
    P2 = -2.d0*P1
    do i = 1, N-1
        A(i-1, i) = P1
        A(i, i) = P2
        A(i+1, i) = P1
    end do
    A(0,0) = 1.d0
    A(N,N) = 1.d0
    X(N) = 1.d0

    do i = 1, N
        do j = 1, N
            if (i .NE. j) then
                c = A(i,j)/A(i,i)
                A(:,j) = A(:,j) - c*A(:,i)
                X(j) = X(j) - c*X(i)
                X(i) = X(i)/A(i,i)
                A(:,i) = A(:,i)/A(i,i)
            endif
        end do
    end do 
    syncall() 
    write(*,*) A(:,:)
end program

    ! coarrays
    ! doxygen
    ! 