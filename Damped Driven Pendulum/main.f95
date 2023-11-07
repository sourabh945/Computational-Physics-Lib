program main
    implicit none 
    ! Reference: https://galileoandeinstein.phys.virginia.edu/7010/CM_22a_Period_Doubling_Chaos.html

    real ,dimension(:) , allocatable :: x , y , y1 ! declaring variables
    real :: upper_t , s_t , k  , o , r , q , h
    integer :: num_of_intervals , j
    logical :: f1,f2

    print*, "Enter the value of the number of interval :: " ! getting the number of the points
    ! read*, num_of_intervals
    print*, "Enter the upper limit of the time :: "
    ! read*, upper_t
    num_of_intervals = 1000000
    upper_t = 10000

    h = upper_t/real(num_of_intervals) ! calculation the time interval 
    if(h<=0 ) stop "Invalid entry"

    allocate(x(num_of_intervals+1),y(num_of_intervals+1),y1(num_of_intervals+1))
    
    x(1) = 0.0 ! defining the constants value of the equation 
    k= 1
    r = 0.1


    print*, "Enter the value of y at t=0 ::" ! getting the starting point in the phase diagram 
    ! read*, y(1)
    y(1) = 0
    print*, "Enter the value of y' at t=0 :: "
    ! read*, y1(1)
    y1(1) = 0
    print*, "Enter the value of Q :: "
    read*, q 
    print*,"enter o"
    read*, o

    inquire(file="phase_diagram.dat",exist=f1) ! opening the file for save the data
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

    do j = 2,num_of_intervals+1 ! loop for calculating the value at every time interval 
        call rkmethod(j)
        write(1,*)x(j),y(j),y1(j) ! writing the in the file 
        if(j >= num_of_intervals*o) write(2,*)x(j),y(j),y1(j) ! deleting the first entry and save into another file
    enddo

    print*, "simulation is complete"

    close(1)
    close(2)
    stop

    contains
        real function func(val_x,val_y,val_y1) ! function of the damped driven pendulum
            real , intent(in) :: val_x,val_y,val_y1
            real :: result 
            result = q*sin(val_x*real(3/2)) - k*sin(val_y) - r*val_y1
            func = result 
            return 
        end function func

        subroutine rkmethod(i) ! subroutine for solving the differential equation 
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