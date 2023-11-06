program main
    implicit none ! declaring the variables , here si1 is the first derivative of the si wrt x

    real , allocatable :: x(:), si(:) , si1(:)
    integer :: num_of_intervals , j, step_num_energy , k , l 
    real :: x_upper , x_lower , h , energy , a  , energy_upper , step_size_energy
    logical :: f1

    print*, "Enter the upper limit of energy (MeV) and number of steps :: " 
    read*, energy

    inquire(file="points.dat",exist=f1) ! opening the file for the store the data for plot 
    if(f1) then
        open(1,file="points.dat",status="replace")
    else
        open(1,file="points.dat",status="new",action="write")
    endif

    num_of_intervals = 10000
    a = -1*sqrt(3.2653*energy)
    x_lower = -20
    x_upper = 20
    h = (x_upper - x_lower)/real(num_of_intervals)
    allocate(x(num_of_intervals+1),si(num_of_intervals+1),si1(num_of_intervals+1))
    x(1) = x_lower
    si(1) = 0
    si1(1) = 0
    write(1,*)x(1),si(1),si1(1)
    do l = 2,num_of_intervals+1
        call rkmethod(l)
        write(1,*)x(l),si(l),si1(l),energy
    enddo
    if(si(num_of_intervals+1) == 0) then 
        print*, "The graph is satisfied at ",energy," and the graph value is ",k
    endif
    deallocate(x,si,si1)


    print*, "All value calculated"
    stop

    contains
        real function func(val_x,val_y,val_y1) ! here the ODE we wanted to solve 
            real , intent(in) :: val_x,val_y,val_y1
            real :: result 
            result = -energy/20.0 + (0.765625e-2)*val_x*val_x
            func = result 
            return 
        end function func

        subroutine rkmethod(i)   ! here we calculate the value by the rk method and this is for 2nd order equation 
            integer , intent(in) :: i
            real :: k1,k2,k3,k4,k11,k12,k13,k14,result  ! here we use 4th order rk method for 2nd order ODE
            k11 = h*si1(i-1)
            k1 = h*func(x(i-1),si(i-1),si1(i-1))
            k12 = (si1(i-1) + k1/2.0)*h
            k2 = h*func((x(i-1)+h/2.0),(si(i-1)+k11/2.0),(si1(i-1)+k1/2.0))
            k13 = (si1(i-1) + k2/2.0)*h
            k3 = h*func((x(i-1)+h/2.0),(si(i-1)+k13/2.0),(si1(i-1)+k2/2.0))
            k14 = (si1(i-1) + k3)*h
            k4 = h*func((x(i-1)+h),(si(i-1)+k14),(si1(i-1)+k3))
            x(i) = x(i-1) + h
            si(i) = si(i-1) + (k11+2*k12+2*k13+k14)/6.0
            si1(i) = si1(i-1) + (k1+2*k2+2*k3+k4)/6.0
            return 
        end subroutine rkmethod
end program 