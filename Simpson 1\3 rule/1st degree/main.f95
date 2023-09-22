program simpson_rule
    implicit none

    real func , sum , upper , lower , h
    integer i , d

! getting the input for upper limit and lower limit of integration 

    print *, "Enter the upper limit of the integration :: "
    read(*,*) upper
    print *, "Enter the lower limit of the integeration :: "
    read(*,*) lower
    print*, "Enter the number of iteration :: "
    read(*,*) d

! validating the number of interation

    if(d <= 0) then 
        print *, "Value of iteration is not correct"
        call Exit(1)
    endif

    if(mod(d,2) .ne. 0)then
        d = d+1
    endif

! defining the interval

    h = (upper - lower)/(real(d))

    sum =  func(lower) - func(upper) 

! loop calcuation the area of the all rectangle inside the limits
    do i = 1,d/2
        sum = sum + 4*func(lower + real(2*i-1)*h) + 2*func(lower + real(2*i)*h)
    enddo 

    sum = sum*(h/3)

! print the result
    print*, "Value of the integration is ",sum

    stop
    end program

! defining the function which integration we intersed to find
real function func(x)
    real x , value
    value = x**2
    func = value
    return 
    end function



