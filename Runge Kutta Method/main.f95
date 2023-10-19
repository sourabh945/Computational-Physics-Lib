program main
    implicit none

    real , allocatable :: x(:), y(:) , y1(:)
    integer :: num_of_intervals , j
    real :: x_upper , x_lower , h
    logical :: f1

    print *, "Enter the lower limit of x: "
    read *, x_lower
    print *, "Enter the upper limit of x: " 
    read *, x_upper
    print *, "Enter the number of intervals we take "
    read *, num_of_intervals
    if (num_of_intervals <=0) stop "Invalid number of the intervals"

    allocate(x(num_of_intervals+1),y(num_of_intervals+1),y1(num_of_intervals+1))

    print *, "Enter the value of y at lower limit of x :: "
    read *, y(1)
    print*, "Enter the value of y' at lower limit of x :: "
    read*, y1(1)

    inquire(file="points.dat",exist=f1)
    if(f1) then
        open(1,file="points.dat",status="replace")
    else
        open(1,file="points.dat",status="new",action="write")
    endif

    h = (x_upper - x_lower)/real(num_of_intervals)
    if (h<=0) stop "Invalid entry for upper and lower limit"
    x(1) = x_lower

    write(1,*)x(1),y(1),y1(1)

    do j=2,num_of_intervals+1
        call rkmethod(j)
        write(1,*)x(j),y(j),y1(j)
    enddo

    close(1)

    print*, "ODE is solved"
    print*, "Value of y at x = ",x(num_of_intervals+1)," is  :: ",y(num_of_intervals+1)," and y' is :: ",y1(num_of_intervals+1)
    
    stop

    contains
        real function func(val_x,val_y,val_y1)
            real , intent(in) :: val_x,val_y,val_y1
            real :: result 
            result = exp(2*val_x)*sin(val_x) - 2*val_y + 2*val_y1
            func = result 
            return 
        end function func

        subroutine rkmethod(i)
            integer , intent(in) :: i
            real :: k1,k2,k3,k4,k11,k12,k13,k14,result
            k11 = h*y1(i-1)
            k1 = h*func(x(i-1),y(i-1),y1(i-1))
            k12 = (y1(i-1) + k1/2.0)*h
            k2 = h*func((x(i-1)+h/2.0),(y(i-1)+k11/2.0),(y1(i-1)+k1/2.0))
            k13 = (y1(i-1) + k2/2.0)*h
            k3 = h*func((x(i-1)+h/2.0),(y(i-1)+k13/2.0),(y1(i-1)+k2/2.0))
            k14 = (y1(i-1) + k3)*h
            k4 = h*func((x(i-1)+h),(y(i-1)+k14),(y1(i-1)+k3))
            x(i) = x(i-1) + h
            y(i) = y(i-1) + (k11+2*k12+2*k13+k14)/6.0
            y1(i) = y1(i-1) + (k1+2*k2+2*k3+k4)/6.0
            return 
        end subroutine rkmethod
end program 