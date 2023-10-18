program main
    implicit none

    real , dimension(:) , allocatable :: x(:) , y(:) , diff(:)
    real :: result , p , d , val
    integer :: i , j , k , n , index , row , num_points
    logical :: f1,f2

    print*, "Enter the number of the given points : "
    read*, n
    if (n <= 0) stop "Invalid entry"

    allocate(x(n),y(n),diff(fact(n-1)))

    print*, "Enter the value of x and y : "
    print*, "x      y"
    read*, (x(i),y(i),i=1,n)

    print *, "Enter the number of points taking between x0 and xn :: "
    read*, num_points 
    if (num_points <= 0 ) stop "Invalid entry"

    inquire(file="givenpoints.dat",exist=f1)
    if(f1) then
        open(1,file="givenpoints.dat",status="replace")
    else
        open(1,file="givenpoints.dat",status="new",action="write")
    endif

    inquire(file="calpoints.dat",exist=f2)
    if(f2) then
        open(2,file="calpoints.dat",status="replace")
    else
        open(2,file="calpoints.dat",status="new",action="write")
    endif

    do i=1,n
        write(1,*)x(i),y(i)
    enddo

    row = 0
    do i = 1,n-1
        do j = 1,n-i
            if(i==1) then
                diff(j) = y(j+1) - y(j)
            else
                diff(row+j) = diff((row-n+i-1)+j+1) - diff((row-n+i-1)+j)
            endif
        enddo
        row = row + n - i
    enddo

    d = (x(n) - x(1))/real(num_points)

    do k = 1,num_points-1
        val = x(1) + k*d
        index = cal_x0_index(val)
        p = (val - x(index))/(x(2) - x(1))
        result = y(index)
        row = 0
        do i = 1, n-index
            result = result + p*diff(row+index)/real(fact(i))
            p = p*(p-i)
            row = row + n-i
        enddo
        write(2,*)val,result
    enddo

    print*, "Simulation is complete"

    stop

contains 
    integer function fact(valu)
        integer , intent(in) :: valu
        integer :: l , res 
        res = 1
        if (valu == 1) then
            fact = 1
            return
        endif
        do l = 2, valu
            res= res*l
        enddo
        fact = res
        return 
    end function fact

    integer function cal_x0_index(value)
        real , intent(in) :: value
        integer :: k
        do k = 1,n
            if (x(k) > value) then
                cal_x0_index = k-1
                return 
            endif
        enddo
        stop "Value of x  is over the upper limit"
    end function cal_x0_index
end program