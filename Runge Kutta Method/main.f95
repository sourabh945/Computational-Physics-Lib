program main
    implicit none

    real , allocatable :: x(:), y(:)
    integer :: num_of_intervals 
    real :: x_upper , x_lower

    print *, "Enter the lower limit of x: "
    read *, x_lower
    print *, "Enter the upper limit of x: " 
    read *, x_upper
    print *, "Enter the number of intervals we take "
    read *, num_of_intervals

    allocate(x(num_of_intervals),y(num_of_intervals))

    print *, "Enter the value of y at lower limit of x :: "
    read *, x_lower