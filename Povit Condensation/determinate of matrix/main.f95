program main

    implicit none

    ! declaration the variable used for the operation

    real , dimension(:,:) , allocatable :: matrix 
    real :: determinent , mul
    integer :: order,i,j,k,sign

    ! This sign variable give the sign for the determinant when the row is exchange with each other

    sign = 1

    ! Input of the order of the matrix and validate the order of the matrix

    print *, "Enter the number of the rows in the matrix :: "
    read *, order

    if (order <= 0 ) then 
        print *, "Invalid order for matrix"
        stop "Indexing Error"
    endif

    ! Here we allocate the matrix array in the memory with variable lenght

    allocate(matrix(order, order))

    ! We getting the matrix element as input

    print *, "Enter the matrix :: "
    do i = 1,order
        do j = 1,order
            print *, i,j,"th element is :: "
            read *, matrix(i,j)
        enddo
    enddo

    ! Here we do the povit condenstation and getting the reduced matrix

    do i = 1,order-1
1       if (matrix(i,i) == 0) then
            do k = i+1,order 
                if (matrix(k,i) .ne. 0) then
                    call flip(k,i)
                    sign = sign*(-1)
                    goto 1
                endif
            enddo
        endif
        do j = i+1,order
            mul = matrix(j,i)/matrix(i,i)
            do k = i,order
                matrix(j,k) = matrix(j,k) - (mul)*matrix(i,k)
            enddo
        enddo
    enddo

    ! Here we get the determinent by multiplying all the digonal elements

    determinent = 1

    do i = 1,order
        determinent = determinent*matrix(i,i)
    enddo

    ! here we deallocate the matrix from the heap memory 

    deallocate(matrix)

    ! Here we multiply the determinent with its sign getting from the row exchanges 

    determinent = determinent*sign

    print *, "Determinent of the matrix is ", determinent ! printing the determinent

    stop

    ! Here we make subroutine for flip the row for avoid the povit zero condition.

    contains 
            subroutine flip(row1,row2)
                integer ,intent(in)::row1,row2 
                real :: temp_row
                integer :: i
                do i = 1,order
                    temp_row = matrix(row1,i)
                    matrix(row1,i) = matrix(row2,i)
                    matrix(row2,i) = temp_row
                enddo
                return  
                end subroutine

    end program