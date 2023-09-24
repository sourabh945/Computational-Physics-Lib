program main
    implicit none ! declaring the variable 

    real , allocatable :: matrix(:,:) , vector(:) , pervious_vector(:)
    real :: user_error , eignValue , error = 1074E12
    integer :: i ,j,order , iteration 

    ! getting the input from the user and also verified the input and allocating the space

    print *, "Enter the order of matrix :: "
    read *, order
    
    if (order <=0) stop "Invalid order of the matrix"

    allocate(matrix(order,order),vector(order),pervious_vector(order))

    print *, "Enter the Matrix :: "
    do i = 1,order
        do j = 1,order 
            print *, "Enter the ",i,j,"th element :: "
            read *, matrix(i,j)
        enddo
    enddo

    print *, "Enter the guess for the eignvector :: "
    read *, (pervious_vector(i),i=1,order)

    print *, "Enter the tolerance :: "
    read *, user_error

    print *, "Enter the maximum iteration :: "
    read *, iteration

    if (iteration <=0 ) stop "Invalid number of the iteration"

    ! making a loop until the error is less than tolerance  or if iteration is complete

    do j = 1,iteration

        eignValue = 0
        do i = 1,order     ! multiplying the matrix and also find the largest value save in eignvalue

            vector(i) = sum(matrix(i,:)*pervious_vector(:))

            if (abs(vector(i)) > eignValue) eignValue = vector(i)

        enddo
        
        error = 0

        do i = 1,order      ! Caluclation the error and making vector equal to pervious vector

            vector(i) = vector(i)/eignValue

            error = max(abs(vector(i) - pervious_vector(i)),error)

            pervious_vector(i) = vector(i)

        enddo

        if (user_error > abs(error)) goto 1

    enddo

1   print *, "The eign Vector correspond to matrix in ",j,"is ",(vector(i), i= 1,order)

    eignValue = 0 ! here we calculating the eignvalue by using the ratio of pervious vector and new vector

    do i = 1,order

        vector(i) = sum(matrix(i,:)*pervious_vector(:))
        eignValue = eignValue + vector(i)/pervious_vector(i)

    enddo

    eignValue = eignValue/real(order)
    
    print *, "The eign Value is " , eignValue

    deallocate(vector,pervious_vector,matrix) ! deallocating the all variables

    stop 
end program 