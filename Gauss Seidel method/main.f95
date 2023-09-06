program main

    implicit none

    real , dimension(:,:) , allocatable :: matrix , dumy
    real , dimension(:) , allocatable :: new_ans , last_ans
    real :: u_err , err ,div , sum
    integer :: order , i, j

    ! getting the number of the variable and equation and validating it

    print *, "Enter the number of the variable :: "
    read *, order 

    if(order<=0) then
        stop "Invalid number of the variable"
    endif

    ! allocate the matrix new_ans and last_ans array in the heap memory
    allocate(dumy(order,order+1),new_ans(order+1),last_ans(order+1))


    ! getting the equation the form of the matrix

    print *, "Enter the equation in the form of the matrix :: "
    do i = 1, order
        do j = 1,order+1
            if (j > order) then
                print *,"Enter the ",i,"the equation constant :: "
                read *, dumy(i,j)
            else
                print *, "Enter ",i,j,"th element :: "
                read *, dumy(i,j)
            endif
        enddo
    enddo

    print *, "Enter the best guess for the solution of the variables :: "
    do i = 1,order
        print *, "Enter the value of ",i,"th element :: "
        read *, last_ans(i)
        new_ans(i) = 0
    enddo
    new_ans(order+1) = 1
    last_ans(order+1) = 1

    print *, "Enter the error allowed :: "
    read *, u_err

    allocate(matrix(order,order+1))

    do i = 1,order
        div = (dumy(i,i))**(-1)
        do j = 1,order+1
            if (i==j) then
                matrix(i,j) = 0
            else
                matrix(i,j) = -1.0*dumy(i,j)*div
            endif
        enddo
    enddo

    deallocate(dumy)

    err = 1073E12

    do while (u_err < err)
        do i = 1,order 
            sum = 0
            do j = 1,order+1
                sum = sum + matrix(i,j)*last_ans(j)
            enddo
            new_ans(i) = sum
        enddo
        err = abs(new_ans(1) - last_ans(1))
        do i = 2,order
            err = max(err , abs(new_ans(i) - last_ans(i)))
        enddo
        do i = 1,order
            last_ans(i) = new_ans(i)
        enddo
    enddo

    print *, "Output::"

    do i = 1,order
        print *, "Value of ",i,"th variable"
        print*,new_ans(i)
    enddo

    stop

end program
