program main 
   implicit none
   
   double precision , allocatable :: x(:) , si(:) , si1(:) , potential_energy(:)
   integer :: num_of_intervals , j , last_point
   double precision :: x_upper , x_lower , h , energy , a , step_size , energy_vibrator , normalizer , pot_energy , max_si
   logical :: f1 , peak = .false. 
   
   print*, "Enter the value of the energy : " 
   read*, energy 
   
   inquire(file="points.dat",exist=f1) ! opening the file for the store the data for plot 
   if(f1) then
   open(1,file="points.dat",status="replace")
   else
   open(1,file="points.dat",status="new",action="write")
   endif
   
   num_of_intervals = 10000
   x_upper = 40 
   x_lower = -40
   h = (x_upper - x_lower)/real(num_of_intervals)
    		allocate(x(num_of_intervals+1),si(num_of_intervals+1),si1(num_of_intervals+1),potential_energy(num_of_intervals+1))
   
   x(1) = x_lower 
   si(1) = 1.0e-5
   si1(1) = 1
   call potential_energy_cal(potential_energy(1),x(1))
   normalizer = 0 
   last_point = num_of_intervals
   do j = 2,num_of_intervals
       x(j) = x(1) + h*j
       call potential_energy_cal(pot_energy,x(j))
       potential_energy(j) = pot_energy  
       if(pot_energy > energy) then 
            si(j) = si(1)  
            si1(j) = si1(1)
       else
            call rkmethod(j) 
       endif
      if( peak .eqv. .false.) max_si = max(max_si,abs(si(j)))
      if(max_si > abs(si(j))) peak = .true.
      if((abs(si(j)) > 1.1*max_si) .and. peak ) then
       	   last_point = j
       	   exit
       endif
       normalizer = normalizer + 0.5*h*(si(j)*si(j) + si(j-1)*si(j-1))
   enddo
   
   normalizer = sqrt(normalizer)
   do j = 1, num_of_intervals
   	if(j < last_point ) then 
		if (potential_energy(j) < energy) then 
		     si(j) = si(j)/normalizer
		     write(1,*)x(j),si(j),si1(j)/normalizer,potential_energy(j),energy,si(j)**2
		endif
         endif 
   enddo
   close(1)
   	
   print*,'simulation is end'
   stop
   
   contains
   	subroutine potential_energy_cal(po_energy,val_x)
   	    double precision , intent(out) :: po_energy 
   	    double precision, intent(in) :: val_x
   	    po_energy = (0.765625e-2)*(val_x**2)
   	    return 
   	end subroutine potential_energy_cal
   	
   	double precision function func(val_x,val_y,val_y1) ! here the ODE we wanted to solve 
            double precision , intent(in) :: val_x,val_y,val_y1
            double precision :: result 
            result = (-energy/20.0 + (0.765625e-2)*val_x*val_x)*val_y
            func = result 
            return 
        end function func

        subroutine rkmethod(i)   ! here we calculate the value by the rk method and this is for 2nd order equation 
            integer , intent(in) :: i
            double precision :: k1,k2,k3,k4,k11,k12,k13,k14,result  ! here we use 4th order rk method for 2nd order ODE
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
   
