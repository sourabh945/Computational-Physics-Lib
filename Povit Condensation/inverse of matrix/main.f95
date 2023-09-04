program inverse

    implicit none

    ! defining the variable used for the program 

    real , dimension(:,:) , allocatable :: matrix !here the matrix is allocatable means we can allocate in momeory with dimension we take from user
    integer :: order , i , j , k , sign
    real :: mul , determinent , div

    ! This sign variable give the sign for the determinant when the row is exchange with each other

    sign = 1

    ! here order of the matrix is taken from the user and also validate the order

    print *, "Enter the order of the matrix"
    read *, order 
    if (order <= 0) then
        print*, "Invalid order for the matrix"
        stop "Indexing Error"
    endif

    ! allocate the matrix in the momeory with the size of row is order and column is order*2

    allocate(matrix(order,order*2))

    ! getting the elements of the matrix from user and also the define the idenity matrix

    print *, "Enter the elements of the matrix"
    do i = 1,order
        do j = 1,order
            if (i==j) then
                matrix(i,order+i) = 1.0
            else 
                matrix(i,order+j) = 0.0
            endif
            print *, "enter",i,j,"th elment :"
            read *, matrix(i,j)
        enddo
    enddo

    ! make upper triangular matrix from the povit condensation

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
            do k = i,order*2
                matrix(j,k) = matrix(j,k) - (mul)*matrix(i,k)
            enddo
        enddo
    enddo


    ! calculating the determinent from the upper triangular matrix from last step and make sure that the matrix having non zero determinent are go for next st

    determinent = 1

    do i = 1,order
        determinent = determinent*matrix(i,i)
    enddo

    determinent = determinent*sign

    print *, "Determinent of the matrix is ", determinent

    if (determinent == 0) then
        print*, "Inverse of the matrix is not exists"
        stop "Zero Determinant"
    endif

    ! making upper triangular matrix into the identity matrix using povit condenstation for inverse of the matrix

    do i = order,1,-1
        div = matrix(i,i)
        do j = i,order*2
            matrix(i,j) = matrix(i,j)/div
        enddo
        do k = i-1,1,-1
            mul = matrix(k,i)
                do j = i,order*2
                    matrix(k,j) = matrix(k,j) - mul*matrix(i,j)
                enddo
        enddo
    enddo

    ! here printing the side matrix having inverse of the input matrix

    print *, "Inverse of the matrix is :: "
    do i = 1,order
        do j = order+1,order*2
            print *, matrix(i,j)
        enddo
        print *, ""
    enddo

    ! here we deallocate the matrix from the heap memory 

    deallocate(matrix)


    stop

    ! there is the subroutine for the flip the row of the matrix

    contains 
        subroutine flip(row1,row2)
            integer ,intent(in)::row1,row2 
            real :: temp_row
            integer :: i
            do i = 1,order*2
                temp_row = matrix(row1,i)
                matrix(row1,i) = matrix(row2,i)
                matrix(row2,i) = temp_row
            enddo
            return  
            end subroutine

end program
