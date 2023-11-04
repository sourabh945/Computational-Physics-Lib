program main

    implicit none

    ! declaring the variables need 

    real :: upper,lower , integration = 0
    real , dimension(:,:), allocatable  :: optimizer
    integer :: i , degree 

    ! getting the input about the upper and the lower limit and also value of n (number of points)

    print *, "Enter the number of points taken (Hint : 2 to 5):: "
    read *, degree
    print *, "Enter the upper limit :: "
    read *, upper
    print *, "Enter the lower limit :: "
    read *, lower

    ! declaring the optimizer points value in the array

    if (degree == 2) then
        allocate(optimizer(2,2))
        optimizer(1,1) = 1
        optimizer(1,2) = -0.5773502692
        optimizer(2,1) = 1
        optimizer(2,2) = 0.5773502692

    else if (degree == 3) then
        allocate(optimizer(3,2))
        optimizer(1,1) = 0.5555555
        optimizer(1,2) = -0.7745967
        optimizer(2,1) = 0.8888888
        optimizer(2,2) = 0
        optimizer(3,1) = 0.5555555
        optimizer(3,2) = 0.7745967

    else if (degree == 4) then
        allocate(optimizer(4,2))
        optimizer(1,2) = 0.8611363116 
        optimizer(1,1) = 0.3478548451
        optimizer(2,2) = 0.3399810436 
        optimizer(2,1) = 0.6521451549
        optimizer(3,2) = -0.3399810436 
        optimizer(3,1) = 0.6521451549
        optimizer(4,2) = -0.8611363116 
        optimizer(4,1) = 0.3478548451

    else if (degree == 5) then
        allocate(optimizer(5,2))
        optimizer(1,2) = 0.9061798459 
        optimizer(1,1) = 0.2369268850
        optimizer(2,2) = 0.5384693101 
        optimizer(2,1) = 0.4786286705
        optimizer(3,2) = 0.0000000000 
        optimizer(3,1) = 0.5688888889
        optimizer(4,2) = -0.5384693101 
        optimizer(4,1) = 0.4786286705
        optimizer(5,2) = -0.9061798459 
        optimizer(5,1) = 0.2369268850

    else 
        stop "Unable to do this operation for this degree of polynomial"
    endif

    ! taking loop for all the value for calculating the integration

    do i = 1,degree
        integration = integration + optimizer(i,1)*(func(optimizer(i,2)))
    enddo
  
    deallocate(optimizer) ! deallocate the array

    integration = (integration*(upper  - lower))/2.0

    print *, "The integration is :: ",integration !printing the result 

    stop 
    
    contains
        real function func(t) ! function for making change in variable for condition
            real , intent(in) :: t !of integration limit satisfied
            real :: output , x
            x = ((upper - lower)*t + upper + lower)/2.0
            output = x**4+1
            func = output
            return 
        end function func

end program