program main

    implicit none

    real :: upper,lower , integration = 0
    real , dimension(:,:), allocatable , parameter :: optimiser
    integer :: i , degree 

    print *, "Enter the order of the polynomial :: "
    read *, degree
    print *, "Enter the lower limit :: "
    read *, lower
    print *, "Enter the uppper limit :: "
    read *, upper

    if (degree == 3) then
        allocate(optimiser(2,2))
        optimiser(1,1) = 1
        optimiser(1,2) = sqrt(real(1/3))
        optimiser(2,1) = 1
        optimiser(2,2) = -sqrt(real(1/3))

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


    do i = 1,degree:
        integration = integration + optimiser(i,1)*func(optimiser(i,2))
    enddo

    print *, "The integration is :: ",integration

    stop 

    contains
        subroutine 



