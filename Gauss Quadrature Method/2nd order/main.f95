program main

    implicit none

    real :: upper_y,upper_x,lower_y,lower_x , integration = 0
    real , dimension(:,:), allocatable :: optimizer
    integer :: i , j , degree

    print *, "Enter the order of the polynomial  (Hint : 2 to 5):: "
    read *, degree
    print *, "Enter the upper limit of x axis :: "
    read *, upper_x
    print *, "Enter the lower limit of x axis:: "
    read *, lower_x
    print *, "Enter the upper limit of y axis:: "
    read *, upper_y
    print *, "Enter the lower limit of y axis:: "
    read *, lower_y


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


    do i = 1,degree
        do j = 1,degree
            integration = integration + optimizer(i,1)*optimizer(j,1)*func(optimizer(i,2),optimizer(j,2))
        enddo
    enddo

    deallocate(optimizer)

    integration = (integration*(upper_y  - lower_y)*(upper_x - lower_x))/4.0

    print *, "The integration is :: ",integration

    stop

    contains
        real function func(m,t)
            real , intent(in) :: t , m
            real :: output , x ,y
            x = ((upper_x - lower_x)*t + upper_x + lower_x)/2.0
            y = ((upper_y - lower_y)*m + upper_y + lower_y)/2.0
            output = log(x+2*y)
            func = output
            return 
        end function func

end program 
