program simpson_rule
    implicit none

    real :: sum , upper_x , lower_x , upper_y , lower_y , h_y , h_x
    integer :: i , d_x , d_y

! getting the input for upper limit and lower limit of integration 

    print *, "Enter the upper limit of x axis of the integration :: "
    read *, upper_x
    print *, "Enter the lower limit of x axis of the integeration :: "
    read *, lower_x
    print *,"Enter the upper limit of y axis of the integration :: "
    read *, upper_y
    print *, "Enter the lower limit of y axis of the integration :: "
    read *, lower_y
    print*, "Enter the number of iteration of x axis :: "
    read *, d_x
    print *, "Enter the number of iteration of y axis :: "
    read *, d_y

! validating the number of interation

    if(d_x <= 0 .or. d_y <=0) then 
        print *, "Value of iteration is not correct"
        call Exit(1)
    endif

    if(mod(d_x,2) .ne. 0)then
        d_x = d_x +1
    endif

    if(mod(d_y,2) .ne. 0)then
        d_y = d_y + 1
    endif

! defining the interval

    h_y = (upper_y - lower_y)/(real(d_y))
    h_x = (upper_x - lower_x)/(real(d_x))

    sum =  simpson(lower_y) - simpson(upper_y)

! loop calcuation the area of the all rectangle inside the limits
    do i = 1,d_y/2
        sum = sum + 4*simpson(lower_y + real(2*i-1)*h_y) + 2*simpson(lower_y + real(2*i)*h_y)
    enddo 

    sum = sum*(h_y/3)

! print the result
    print*, "Value of the integration is ",sum

    stop


! defining the function which integration we intersed to find
    contains 

        real function func(x,y)
            real ,intent(in) :: x , y 
            real :: value
            value = log(x+2*y)
            func = value
            return 
            end function func

        real function simpson(t)
            real x_sum , t
            integer j
            X_sum = func(lower_x,t) - func(upper_x,t)  
            do j = 1,d_x/2
                x_sum = x_sum + 4*func(lower_x + (2*j-1)*h_x,t) + 2*func(lower_x + 2*j*h_x,t)
            enddo
            x_sum = x_sum*(h_x/3.0)
            simpson = x_sum
            return 
            end function simpson

end program 

