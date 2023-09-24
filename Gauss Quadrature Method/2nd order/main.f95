program main

    implicit none

    real :: upper_y,upper_x,lower_y,lower_y , integration = 0
    real , dimension(:,:), allocatable :: optimiser
    integer :: i , degree

    print *, "Enter the order of the polynomial  (Hint : 3 or 5):: "
    read *, degree
    print *, "Enter the uppper limit of x axis :: "
    read *, upper_x
    print *, "Enter the lower limit of x axis:: "
    read *, lower_x
    print *, "Enter the uppper limit of y axis:: "
    read *, upper_y
    print *, "Enter the lower limit of y axis:: "
    read *, lower_y


    if (degree == 3) then
        allocate(optimiser(2,2))
        optimiser(1,1) = 1
        optimiser(1,2) = -sqrt(1.0/3.0)
        optimiser(2,1) = 1
        optimiser(2,2) = sqrt(1.0/3.0)

    else if (degree == 5) then
        allocate(optimiser(3,2))
        optimiser(1,1) = 0.5555555
        optimiser(1,2) = -0.7745967
        optimiser(2,1) = 0.8888888
        optimiser(2,2) = 0
        optimiser(3,1) = 0.5555555
        optimiser(3,2) = 0.7745967

    else 
        stop "Unable to do this operation for this degree of polynomial"
    endif

    degree = (degree + 1)/2

    integration = integration + optimiser(i,1)*(GuassQuadmethod(optimiser(i,2)))

    deallocate(optimiser)

    integration = (integration*(upper_y  - lower_y))/2.0

    print *, "The integration is :: ",integration

    contains
        real function func(m,t)
            real , intent(in) :: t , m
            real :: output , x ,y
            x = ((upper_x - lower_x)*t + upper_x + lower_x)/2.0
            y = ((upper_y - lower_y)*t + upper_y + lower_y)/2.0
            output = x**3 + y**3
            func = output
            return 
        end function func

        real function GuassQuadmethod(y)
            real , intent(in) :: y
            real :: integeration_x 
            integer :: j
            do j = 1,degree
                integeration_x = integeration_x + optimiser(i,1)*func(y,optimiser(i,2))
            enddo
            integeration_x = (integeration_x*(upper_x - lower_x))/2.8
            GuassQuadmethod = integeration_x
            return
        end function GuassQuadmethod