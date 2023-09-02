program lsf

    implicit none

    ! declaration of the variables 

    real , dimension(:) , allocatable :: x,y
    real sumx , sumy , sumxx , sumxy , c , m
    integer i ,n
    
    ! Inzillizating the value of the variable

    sumx = 0 
    sumy = 0 
    sumxx = 0
    sumxy = 0

    ! getting number  of the points given

    print *, "Enter the number of the entry required :: "
    read *, n

    ! allocate the array for store the value of the x and y coordinates and getting from the user

    allocate(x(n))
    allocate(y(n))

    print *, "Enter the value of x and y :: "
    print *, "x      y"
    read *, (x(i),y(i),i=1,n)

    ! find the required terms with summations over the input

    do i = 1,n
        sumx = sumx + x(i)
        sumy = sumy + y(i)
        sumxx = sumxx + (x(i))**2
        sumxy = sumxy + x(i)*y(i)
    enddo

    ! deallocate the arrays to free the memory

    deallocate(x)
    deallocate(y)

    ! calculating the intersection and the slope of the line

    c = (sumxx*sumy - sumxy*sumx)/(n*(sumxx)-(sumx)**2)
    m = (n*sumxy - sumx*sumy)/(n*sumxx - (sumx)**2)

    ! giving the output 

    print *, "The value of slope is ",m," and the value of y axis intersection is ",c
    print *, "The equation of the line is :: y = ",m,"x +",c

    stop 

end program



    