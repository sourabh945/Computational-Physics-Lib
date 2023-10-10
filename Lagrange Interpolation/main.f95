program main

    implicit none

    ! declearing the variables 

    real , dimension(:) , allocatable :: x ,y , nx , ny
    integer :: n,i,m , j
    real :: larg , x_rand , result , upper , lower ,h
    logical :: f1,f2

    ! getting the number of the set of points given and validate the number of points

    print *, "Enter the number of the entry :: "
    read *, n

    if (n <= 0) then 
        stop "Invalid number of the entry"
    endif

    inquire(file="points.dat",exist=f1)
    inquire(file="inputpoints.dat",exist=f2)
    if (f1) then
        open(1,file="points.dat",status="replace")
    else
        open(1,file="points.dat",status="new",action="write")
    endif
    if (f2) then
        open(2,file="inputpoints.dat",status="replace")
    else
        open(2,file="inputpoints.dat",status="new",action="write")
    endif

    ! allocate the x and y array for storing the points

    allocate(x(n),y(n))

    ! getting the points from the user

    print *, "Enter the entry :: "
    print *, "x     y"
    read *,(x(i),y(i),i = 1,n)
    do i = 1,n
        write(2,*)x(i),y(i)
    enddo

    ! getting the point where we find the value of the function

    print *, "Ente the upper limit of the polation ::"
    read *, upper
    print *, "Enter the lower limit of the polation ::"
    read *, lower
    print *, "Enter the number of x we find between the limits :: "
    read *, m

    if (m <=0) stop "Invalid number of x taken "

    ! initizating the value of result and loop for getting the summation value of y*lagrange(x)

    h = (upper - lower)/real(m)


    if (h<=0) stop "upper and lower limit is not good"

    do j = 1,m
        x_rand = lower + j*h
        result = 0
        do i = 1,n
            call lagrange(i,x_rand)
            result = result + y(i)*larg
        enddo
        write(1,*)x_rand,result
    enddo

    print*, "polation is completed"

    ! deallocate the x and y array to free the memory

    deallocate(x,y)

    close(1)
    close(2)

    stop

    contains
        ! subroutine for calculating the lagrange at the point
        subroutine lagrange(index,val)
            integer , intent(in) :: index
            real , intent(in) :: val
            integer :: j
            larg = 1.0
            do j = 1,n
                if (j.ne.index) then
                    larg = larg*(val-x(j))/(x(index)-x(j))
                endif
            enddo
            return 
            end subroutine

    end program