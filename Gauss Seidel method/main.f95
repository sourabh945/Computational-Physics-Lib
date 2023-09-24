program main
    implicit none

    real , allocatable :: matrix(:,:),constants(:),variable(:),guess(:),dummy(:,:)
    real :: defined_error , error , div , sum , max_error
    integer :: order , i , j

    print *, "Enter the number of the variables are :: "
    read *, order

    if (order <= 0 ) then
        stop 'Invalid number of variable'
    endif

    allocate(dummy(order,order),constants(order),variable(order),guess(order))

    print *, "Enter the value coefficients  and constants in : "
    do i = 1, order
        do j = 1,order
            print *, "Enter the value of ",i,j,"th coefficient "
            read *, dummy(i,j)
        enddo
        print *, "Enter the ",i,"th equation's constant "
        read *, constants(i)
    enddo

    print *,order

    print *, "Enter the guess for the variables values : "
    read *, (guess(i),i=1,order)

    print *, "okay"

    allocate(matrix(order,order))

    do i = 1,order
        div = dummy(i,i)
        if (div == 0) then
            stop 'Enter the equation in correct order '
        endif
        do j = 1,order
            if (i==j) then
                matrix(i,i) = 0
            endif
            matrix(i,j) = dummy(i,j)/div
        enddo
        constants(i) = -constants(i)/div
    enddo

    deallocate(dummy)

    error = 1073E12

    do while(error > defined_error)
        max_error = 0
        do i = 1,order
            sum = 0
            do j = 1,order
                sum = sum + matrix(i,j)*guess(j) 
            enddo
            variable(i) = sum + constants(i)
            if (abs(variable(i) - guess(i)) > max_error) max_error = abs(variable(i) -guess(i))
        enddo
        do i = 1,order
            print *, variable(i)
            guess(i) = variable(i)
        enddo
    enddo

    deallocate(guess,matrix,variable,constants)

    print *, "The variables are with error ",error," is :: "
    print *, (variable(i),i = 1,order)

    stop

end program