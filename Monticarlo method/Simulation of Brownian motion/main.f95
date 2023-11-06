program main
    implicit none ! declaring the variable to use in program 

    real :: x,y,dx,dy,step_size
    real , allocatable :: points(:,:,:)   ! dx and dy are change in x and y 
    integer :: number_step,i , j ,snapshots , number_points , k 
    logical :: f1
    x = 0 
    y = 0 
    number_step = 50000
    
    print*, "Enter number of points , step size and snapshots::"
    read*, number_points, step_size , snapshots

    allocate(points(snapshots,number_points,2))

    inquire(file="points.dat",exist=f1) !opening the for store the x and y
    if(f1)then
        open(1,file="points.dat",status="replace")
    else
        open(1,file="points.dat",status="new",action="write")
    endif
    do j = 1,snapshots
        do k = 1, number_points
            x = points(j-1,k,1)
            y = points(j-1,k,2)
            do i = 1,number_step       ! making loop generating the random dx and dy
                call random_number(dx)  ! and add into in the last position
                call random_number(dy)
                x = x + (dx - 0.5)*step_size    
                y = y + (dy - 0.5)*step_size
            enddo
            points(j,k,1) = x
            points(j,k,2) = y
            write(1,*)x,y
        enddo
        write(1,*)""
    enddo
    print*, "Simulation is complete"
    stop 
end program 