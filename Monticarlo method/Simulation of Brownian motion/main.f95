program main
    implicit none ! declaring the variable to use in program 

    real :: x,y,dx,dy,step_size   ! dx and dy are change in x and y 
    integer :: number_step,i
    logical :: f1

    print*,"Enter the initial position of the particle :: "
    read*,x,y
    print*,"Enter the step size and number of steps for the motion:: "
    read*,step_size,number_step

    if (number_step < 0 ) stop "Invalid number of steps "

    inquire(file="points.dat",exist=f1) !opening the for store the x and y
    if(f1)then
        open(1,file="points.dat",status="replace")
    else
        open(1,file="points.dat",status="new",action="write")
    endif

    do i = 1,number_step       ! making loop generating the random dx and dy
        call random_number(dx)  ! and add into in the last position
        call random_number(dy)
        x = x + (dx - 0.5)*step_size    
        y = y + (dy - 0.5)*step_size
        write(1,*)x,y
    enddo
    print*, "Simulation is complete"
    stop 
end program 