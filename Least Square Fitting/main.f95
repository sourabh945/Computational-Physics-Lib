program lsf

    implicit none

    ! declaration of the variables 

    real , dimension(:) , allocatable :: x,y,f
    real :: sumx , sumy , sumxx , sumxy , c , m
    integer ::  i ,n
    logical :: f1 , f2
    
    ! Inzillizating the value of the variable

    sumx = 0 
    sumy = 0 
    sumxx = 0
    sumxy = 0

    ! check exiting file and open it and if file doesn't exit then create the files

    inquire(file="allpoints.dat",exist=f1) ! for checking the existance of the file
    inquire(file="fittedpoints.dat",exist=f2) 
    if (f1) then
        open(1,file="allpoints.dat",status="replace")
    else
        open(1,file="allpoints.dat",status="new",action="write")
    endif
    if (f1) then
        open(2,file="fittedpoints.dat",status="replace")
    else
        open(2,file="fittedpoints.dat",status="new",action="write")
    endif

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
        write(1,*)x(i),y(i)
        sumx = sumx + x(i)
        sumy = sumy + y(i)
        sumxx = sumxx + (x(i))**2
        sumxy = sumxy + x(i)*y(i)
    enddo

    ! deallocate the arrays to free the memory

    deallocate(y)

    ! calculating the intersection and the slope of the line


    c = (sumxx*sumy - sumxy*sumx)/(n*(sumxx)-(sumx)**2)
    m = (n*sumxy - sumx*sumy)/(n*sumxx - (sumx)**2)

    ! storing the fitted points

    allocate(f(n)) ! allocating the vector for fitting points

    ! giving the output 

    print *, "The value of slope is ",m," and the value of y axis intersection is ",c
    print *, "The equation of the line is :: y = ",m,"x +",c
    print *, "       x            fitted points "

    do i = 1,n ! calculated the fitting points
        f(i) = m*x(i) + c
        write(2,*)x(i),f(i)
        print *, x(i),f(i)
    enddo

    deallocate(f)
    deallocate(x)

    close(1)
    close(2)


    stop 

end program



    