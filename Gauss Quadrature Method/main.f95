program main

    implicit none

    real :: upper,lower , integration = 0
    real , dimension(:,:), allocatable  :: optimiser
    integer :: i , degree 

    print *, "Enter the order of the polynomial  (Hint : 3 or 5):: "
    read *, degree
    print *, "Enter the uppper limit :: "
    read *, upper
    print *, "Enter the lower limit :: "
    read *, lower

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


    do i = 1,degree
        integration = integration + optimiser(i,1)*(func(optimiser(i,2)))
    enddo

    deallocate(optimiser)

    integration = (integration*(upper  - lower))/2.0

    print *, "The integration is :: ",integration

    stop 

    contains
        real function func(t)
        real , intent(in) :: t
        real :: output , x
        x = ((upper - lower)*t + upper + lower)/2.0
        output = x**3
        func = output
        return 
        end function func

end program



