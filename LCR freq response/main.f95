program main 

    implicit none ! declaring the variables
    real :: resistance , inductance , capacitance , inv_imp , freq_lower , freq_upper , step 
    real :: resonate_freq , voltage , current , high_current , cal_resonate
    integer :: step_num , i
    real , parameter :: pi = 3.14159265
    logical :: f1
! getting the value of resistance , inductance  and capacitance from the user

    print *, "Enter the value of resistance : " 
    read *, resistance
    print *, "Enter the value of inductance (in mH): "
    read *, inductance
    print *, "Enter the value of capacitance (in mF): "
    read *, capacitance
    print *, "Enter the value of voltage (in Volt): "
    read *,voltage

! getting the frequency value form the user
    print *, "Enter the lower limit of frequency : "
    read *, freq_lower
    print *, "Enter the upper limit of frequency : "
    read *, freq_upper
    print *, "Enter the step taken to from lower frequency to upper frequency : "
    read *, step_num

    step = (freq_upper - freq_lower)/(real(step_num)) ! calculating the steps for frequency 

    inductance = inductance/1000 ! convert them into mH and mF

    capacitance = capacitance/1000

    cal_resonate = 1/(2*pi*sqrt(inductance*capacitance)) ! calculating the resonant from formula

    !check exiting file and open it and if file doesn't exit then create the files

    inquire(file="freq_response.dat",exist=f1) ! for checking the existance of the file
    if (f1) then
        open(1,file="freq_response.dat",status="replace")
    else
        open(1,file="freq_response.dat",status="new",action="write")
    endif

    high_current = 0.0

    do i = 0,step_num ! creating the frequency response of LCR circuit

        inv_imp = 1/sqrt(resistance**2 + (2*pi*(freq_lower + i*step)*inductance - 1/(2*pi*(freq_lower + i*step)*capacitance))**2)
        current = inv_imp*voltage
        write(1,*)freq_lower+i*step,current
        if(current > high_current) then ! calculating the resonant frequency from the graph
            high_current = current
            resonate_freq = freq_lower + i*step
        endif
    
    enddo

    close(1)

    print *, "Simulation complete"
    print *, ""
! printing the output
    
    if (cal_resonate > freq_upper) then
        print *, "Resonate frequency is higher than the upper limit of frequency" 
    else   
        print *, "Resonate Frequency is " ,resonate_freq, "and current for ",voltage,"V is ",high_current
    endif

    stop

end program 
    