program lsf
    implicit none

    real , dimension(:) , allocatable :: x,y
    real sumx , sumy , sumxx , sumxy , c , m
    integer i ,n
    logical f1

    sumx = 0 
    sumy = 0 
    sumxx = 0
    sumxy = 0

    print *, "Enter the number of the entry required :: "
    read *, n

    allocate(x(n))
    allocate(y(n))

    print *, "Enter the value of x and y :: "
    print *, "x      y"
    read *, (x(i),y(i),i=1,n)

    do i = 1,n
        sumx = sumx + x(i)
        sumy = sumy + y(i)
        sumxx = sumxx + (x(i))**2
        sumxy = sumxy + x(i)*y(i)
    enddo

    deallocate(x(n))
    deallocate(y(n))

    stop 

end program



    