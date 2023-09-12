program main 

    implicit none

    real :: resistance , inductance , capicatance , impedence , freq_lower , freq_upper , step_num

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
    print *, "Enter the step taken to t