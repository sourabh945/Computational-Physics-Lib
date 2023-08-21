program monticarlo

    real x ,y ,m ,n ,radius ,area ,length ,distance ,error ,area2
    real, parameter :: pi = 3.141592653589793
    integer all , inner ,i
    logical f1,f2

    print *, "Enter the value of the radius of the circle :: "
    read(*,*)radius
    print *, "Enter the number of the points maps :: "
    read(*,*)all

    if ((radius <= 0).and.(all <= 0)) then
        print *, "The Program is not working with these value of the radius and points maps"
        call Exit(1)
    endif

    inquire(file="allpoints.dat",exist=f1)
    inquire(file="innerpoints.dat",exist=f2)
    if (f1) then
        open(1,file="allpoints.dat",status="replace")
    else
        open(1,file="allpoints.dat",status="new",action="write")
    endif
    if (f2) then 
        open(2,file="innerpoints.dat",status="replace")
    else
        open(2,file="innerpoints.dat",status="new",action="write")
    endif

    length = 2*radius
    inner = 0

    do i = 1,all
        call random_number(m)
        x = length*m
        call random_number(n)
        y = length*n
        write(1,*) x,y
        distance = (x-radius)**2 + (y-radius)**2
        if (distance <= radius**2) then 
            write(2,*) x,y
            inner = inner + 1
        endif
    enddo
    
    
    area = ((real(inner))/real(all))*(length**2)
    print *, "The area of the circle by Monti Carlo method is :: ",area
    print *, "Inner points :: ",inner
    area2 = pi*(radius**2)
    error = (area - area2)*100/area
    print *, "The area by simple formula is :: ", area2
    print *, "The percentage error :: ", error

    close(1)
    close(2)

    stop
    end program 
