program main
    implicit none 

    real ,dimension(:) , allocatable :: x , y , y1 
    real :: upper_t , s_t , k  , o , r , q , h
    integer :: num_of_intervals , j
    logical :: f1,f2

    print*, "Enter the value of the number of interval :: "
    read*, num_of_intervals
    print*, "Enter the upper limit of the time :: "
    read*, upper_t

    h = upper_t/real(num_of_intervals)
    if(h<=0 ) stop "Invalid entry"

    allocate(x(num_of_intervals+1),y(num_of_intervals+1),y1(num_of_intervals+1))
    
    x(1) = 0.0
    k= 1.5*1.5
    r = 0.75

    print*, "Enter the value of y at t=0 ::"
    read*, y(1)
    print*, "Enter the value of y' at t=0 :: "
    read*, y1(1)
    print*, "Enter the value of Q :: "
    read*, q 

    inquire(file="phase_diagram.dat",exist=f1)
    if(f1) then
        open(1,file="phase_diagram.dat",status="replace")
    else
        open(1,file="phase_diagram.dat",status="new",action="write")
    endif
    inquire(file="processed.dat",exist=f2)
    if(f2) then
        open(2,file="processed.dat",status="replace")
    else
        open(2,file="processed.dat",status="new",action="write")
    endif

    write(1,*)x(1),y(1),y1(1)

    do j = 2,num_of_intervals+1
        call rkmethod(j)
        write(1,*)x(j),y(j),y1(j)
        if(j >= num_of_intervals*0.1) write(2,*)x(j),y(j),y1(j)
    enddo

    print*, "simulation is complete"

    close(1)
    close(2)
    stop

    contains
        real function func(val_x,val_y,val_y1)
            real , intent(in) :: val_x,val_y,val_y1
            real :: result 
            result = q*k*cos(val_x) - k*sin(val_y) - r*val_y1
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