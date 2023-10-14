program main 

    implicit none

    real :: resistance , inductance , capicatance , impedence , freq_lower , freq_upper , step 
    integer :: step_num , i
    real , parameter :: pi = 3.14159265
    logical :: f1

    print *, "Enter the value of resistance : "
    read *, resistance
    print *, "Enter the value of inductance : "
    read *, inductance
    print *, "Enter the value of capicatance : "
    read *, capicatance

    print *, "Enter the lower limit of frequency : "
    read *, freq_lower
    print *, "Enter the upper limit of frequency : "
    read *, freq_upper
    print *, "Enter the step taken to from lower frequency to upper frequency : "
    read *, step_num

    step = (freq_upper - freq_lower)/(real(step_num))

    !check exiting file and open it and if file doesn't exit then create the files

    inquire(file="freq_response.dat",exist=f1) ! for checking the existance of the file
    if (f1) then
        open(1,file="freq_response.dat",status="replace")
    else
        open(1,file="freq_response.dat",status="new",action="write")
    endif

    do i = 0,step_num
        impedence = 1/sqrt(resistance**2 + (2*pi*(freq_lower + i*step)*inductance - 1/(2*pi*(freq_lower + i*step)*capicatance))**2)
        write(1,*)freq_lower+i*step,impedence
    
    enddo

    close(1)

    print *, "Frequency response is show in graph"

    stop

end program 
    