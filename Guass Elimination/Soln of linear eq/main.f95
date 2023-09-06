program equations
    
    implicit none


    real , dimension(:,:),allocatable :: matrix
    real , dimension(:) , allocatable :: variable 
    integer :: order , i , j , k , povit_status
    real :: determinent , max_lim , min_lim
    logical :: povit_half , povit_full

    print *, "Enter the number of the variable :: "
    read *, order 

    if(order<=0) then
        stop "Invalid number of the variable"
    endif

    allocate(matrix(order,order+1))

    allocate(variable(order))

    do i = 1,order
        variable[i] = i
    enddo

    print *, "Enter the equation in the form of the matrix :: "
    do i = 1, order
        do j = 1,order+1
            if (j > order) then 
                print *, "Enter the constant term in "i"th equation :: "
                read *, matrix(i,order+1)
            endif
            print *, "Enter ",i,j,"th element :: "
            read *, matrix(i,j)
        enddo
    enddo

    print *, "Enter the status of the povit /n for no povit chosse 0,for half choose 1, for full choose 2"
    read *, povit_status

    if(povit == 0) then 
        povit_half = false
        povit_full = false
    else if (povit == 1) then 
        povit_half = true
        povit_full = false
    else if (povit == 2) then 
        povit_half = false
        povit_full = true
    else
        stop "Invalid Povit option "
    endif


    do i = 1,order-1
1       if (matrix(i,i) == 0) then
            do k = i+1,order 
                if (matrix(k,i) .ne. 0) then
                    call flip(k,i)
                    sign = sign*(-1)
                    goto 1
                endif
            enddo
        endif
        do j = i+1,order
            mul = matrix(j,i)/matrix(i,i)
            do k = i,order
                matrix(j,k) = matrix(j,k) - (mul)*matrix(i,k)
            enddo
        enddo
    enddo


    stop
    
    contains 
        subroutine flip(row1,row2)
            integer ,intent(in)::row1,row2 
            real :: temp_row
            integer :: i
            do i = 1,order
                temp_row = matrix(row1,i)
                matrix(row1,i) = matrix(row2,i)
                matrix(row2,i) = temp_row
            enddo
            return  
            end subroutine

    end program
    


        

