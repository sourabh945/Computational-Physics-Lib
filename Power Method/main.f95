program main
    implicit none ! declaring the variable 

    real , allocatable :: matrix(:,:) , vector(:) , pervious_vector(:)
    real :: user_error , eigenValue , error = 1074E12 , count=0
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

    print *, "Enter the guess for the eigenvector :: "
    read *, (pervious_vector(i),i=1,order)

    print *, "Enter the tolerance :: "
    read *, user_error

    print *, "Enter the maximum iteration :: "
    read *, iteration

    if (iteration <=0 ) stop "Invalid number of the iteration"

    ! making a loop until the error is less than tolerance  or if iteration is complete

    do j = 1,iteration

        eigenValue = 0
        do i = 1,order     ! multiplying the matrix and also find the largest value save in eigenValue

            vector(i) = sum(matrix(i,:)*pervious_vector(:))

            if (abs(vector(i)) > eigenValue) eigenValue = vector(i)

        enddo
        
        if(eigenValue == 0 ) stop "Unable to find the eigenVector" 
        
        error = 0

        do i = 1,order      ! Calculation the error and making vector equal to pervious vector

            vector(i) = vector(i)/eigenValue

            error = max(abs(vector(i) - pervious_vector(i)),error)

            pervious_vector(i) = vector(i)

        enddo

        if (user_error > abs(error)) goto 1

    enddo

1   print *, "The eigenVector correspond to matrix in ",j,"is ",(vector(i), i= 1,order)

    eigenValue = 0 ! here we calculating the eigenValue by using the ratio of pervious vector and new vector

    do i = 1,order
        vector(i) = sum(matrix(i,:)*pervious_vector(:))
        if(pervious_vector(i) /= 0 ) then
            eigenValue = eigenValue + vector(i)/pervious_vector(i)
        else 
            count = count + 1
        endif

    enddo

    eigenValue = eigenValue/real(order - count)
    
    print *, "The eigenValue is " , eigenValue

    deallocate(vector,pervious_vector,matrix) ! deallocate the all variables

    stop 
end program 