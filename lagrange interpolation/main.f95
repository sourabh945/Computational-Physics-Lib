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



    print *, "Enter the entry :: "
    print *, "x     y"
    read *,(x(i),y(i),i = 1,n)

    print *, "Enter the value of x for find the f(x) :: "
    read *, v

    result = 0

    do i = 1,n
        call lagrange(i,v)
        result = result + y(i)*larg
    enddo

    deallocate(x,y)

    print *, "The result is ",result

    stop

    contains
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