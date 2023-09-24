program main

    implicit none

    real :: upper,lower , integration = 0
    real , dimension(:,:), allocatable  :: optimiser
    integer :: i , degree 

    print *, "Enter the order of the polynomial  (Hint : 2 to 5):: "
    read *, degree
    print *, "Enter the uppper limit :: "
    read *, upper
    print *, "Enter the lower limit :: "
    read *, lower

    if (degree == 2) then
        allocate(optimiser(2,2))
        optimiser(1,1) = 1
        optimiser(1,2) = -0.5773502692
        optimiser(2,1) = 1
        optimiser(2,2) = 0.5773502692

    else if (degree == 3) then
        allocate(optimiser(3,2))
        optimiser(1,1) = 0.5555555
        optimiser(1,2) = -0.7745967
        optimiser(2,1) = 0.8888888
        optimiser(2,2) = 0
        optimiser(3,1) = 0.5555555
        optimiser(3,2) = 0.7745967

    else if (degree == 4) then
        allocate(optimiser(4,2))
        optimiser(1,2) = 0.8611363116 
        optimiser(1,1) = 0.3478548451
        optimiser(2,2) = 0.3399810436 
        optimiser(2,1) = 0.6521451549
        optimiser(3,2) = -0.3399810436 
        optimiser(3,1) = 0.6521451549
        optimiser(4,2) = -0.8611363116 
        optimiser(4,1) = 0.3478548451

    else if (degree == 5) then
        allocate(optimiser(5,2))
        optimiser(1,2) = 0.9061798459 
        optimiser(1,1) = 0.2369268850
        optimiser(2,2) = 0.5384693101 
        optimiser(2,1) = 0.4786286705
        optimiser(3,2) = 0.0000000000 
        optimiser(3,1) = 0.5688888889
        optimiser(4,2) = -0.5384693101 
        optimiser(4,1) = 0.4786286705
        optimiser(5,2) = -0.9061798459 
        optimiser(5,1) = 0.2369268850

    else 
        stop "Unable to do this operation for this degree of polynomial"
    endif


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
            output = x**4+1
            func = output
            return 
        end function func

end program



