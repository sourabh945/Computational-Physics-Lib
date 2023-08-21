program monticarlo

    implicit none

    real x ,y ,m ,n ,radius ,area ,length ,distance ,error ,area2
    real, parameter :: pi = 3.141592653589793
    integer all , inner ,i
    logical f1,f2

! getting the input

    print *, "Enter the value of the radius of the circle :: "
    read(*,*)radius
    print *, "Enter the number of the points uses :: "
    read(*,*)all

! check the value of the radius and points fit for program

    if (radius <= 0)then
        print *, "The Program is not working with these value of the radius"
        call Exit(1)
    endif

    if (all <= 0)then
        print *, "The Program is not working with these value of the points maps"
        call Exit(1)
    endif

! check exiting file and open it and if file doesn't exit then create the files

    inquire(file="allpoints.dat",exist=f1) ! for checking the existance of the file
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

! defining the square space use for program
    length = 2*radius

    inner = 0

! loop for generating the random points and check how many are inside the circle
    do i = 1,all
        call random_number(m)
        x = length*m ! this because random_number generate only value between 0 and 1
        call random_number(n)
        y = length*n
        write(1,*) x,y
        distance = (x-radius)**2 + (y-radius)**2
        if (distance <= radius**2) then 
            write(2,*) x,y
            inner = inner + 1
        endif
    enddo
    
!  printing the all result and calculating the error

    area = ((real(inner))/real(all))*(length**2)

    print *, "The area of the circle by Monti Carlo method is :: ",area
    print *, "Inner points :: ",inner
    area2 = pi*(radius**2)
    error = (area - area2)*100/area
    print *, "The area by simple formula is :: ", area2
    print *, "The percentage error :: ", error

    close(1) ! Closing the files
    close(2)

    stop
    end program 
