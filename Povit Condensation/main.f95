program main

    implicit none

    real , dimension(:,:) , allocatable :: matrix 
    integer :: order,i,j

    print *, "Enter the number of the rows in the matrix :: "
    read *, order

    if (order <= 0 ) then 
        print *, "Invalid order for matrix"
        stop "Indexing Error"
    endif

    allocate(matrix(order, order))


    print *, "Enter the matrix :: "
    do i = 1,order
        do j = 1,order
            print *, i,j,"th element is :: "
            read *, matrix(i,j)
        enddo
    enddo

    do i = 1,order
1        if (matrix(i,1) == 0) then
            

