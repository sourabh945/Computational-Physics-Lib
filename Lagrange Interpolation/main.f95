program main

    implicit none

    ! declearing the variables 

    real , dimension(:) , allocatable :: x ,y
    integer :: n,i
    real :: v,result,larg

    ! getting the number of the set of points given and validate the number of points

    print *, "Enter the number of the entry :: "
    read *, n

    if (n <= 0) then 
        stop "Invalid number of the entry"
    endif

    ! allocate the x and y array for storing the points

    allocate(x(n),y(n))

    ! getting the points from the user

    print *, "Enter the entry :: "
    print *, "x     y"
    read *,(x(i),y(i),i = 1,n)

    ! getting the point where we find the value of the function

    print *, "Enter the value of x for find the f(x) :: "
    read *, v

    ! initizating the value of result and loop for getting the summation value of y*lagrange(x)

    result = 0

    do i = 1,n
        call lagrange(i,v)
        result = result + y(i)*larg
    enddo

    ! deallocate the x and y array to free the memory

    deallocate(x,y)

    ! printing the result 

    print *, "The value of f(x) at x = ",v," is ::",result

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