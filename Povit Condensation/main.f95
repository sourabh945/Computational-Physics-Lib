program main

    implicit none

    real , dimension(:,:) , allocatable :: matrix 
    real :: temp_row , determinent
    integer :: order,i,j,row1,row2,k,sign

    sign = 1

    print *, "Enter the number of the rows in the matrix :: "
    read *, order

    if (order <= 0 ) then 
        print *, "Invalid order for matrix"
        stop "Indexing Error"
    endif

    allocate(matrix(order, order))

    subroutine flip(order,matrix(order,order),row1,row2)
        allocate(temp_row)
        do i = 1,order
            temp_row = matrix(row1,i)
            matrix(row1,i) = matrix(row2,i)
            matrix(row2,i) = temp_row
        enddo 
        end subroutine


    print *, "Enter the matrix :: "
    do i = 1,order
        do j = 1,order
            print *, i,j,"th element is :: "
            read *, matrix(i,j)
        enddo
    enddo


    do i = 1,order
1       if (matrix(i,i) == 0) then
            do k = i+1,order 
                if (matrix(k,i) .ne. 0) then
                    call flip(order,matrix,k,i)
                    sign = sign*(-1)
                    goto 1
                endif
            enddo
        do j = i+1,order
            do k = i,order
                matrix(j,k) = matrix(j,k) - (matrix(j,k)/matrix(i,k))*matrix(i,k)
            enddo
        enddo
    enddo

    determinent = 1

    do i = 1,order
        if (matrix(i,i) == 0) then 
            print *, "Determinent of the matrix is zero"
            stop
        else
            determinent = determinent*matrix(i,i)
        endif
    enddo

    print *, "Determinent of the matrix is ", determinent

    stop

    end program