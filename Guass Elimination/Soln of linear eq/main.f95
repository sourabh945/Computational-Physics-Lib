program equations
    
    implicit none


    real , dimension(:,:),allocatable :: matrix
    real , dimension(:) , allocatable ::  solution , variable
    integer :: order , i , j , k , povit_status
    real :: determinent , sol , mul


    print *, "Enter the number of the variable :: "
    read *, order 

    if(order<=0) then
        stop "Invalid number of the variable"
    endif

    allocate(matrix(order,order+1))

    allocate(variable(order))

    do i = 1,order
        variable(i) = i
    enddo

    print *, "Enter the equation in the form of the matrix :: "
    do i = 1, order
        do j = 1,order+1
            if (j > order) then 
                print *, "Enter the constant term in ",i,"th equation :: "
                read *, matrix(i,order+1)
            else
                print *, "Enter ",i,j,"th element :: "
                read *, matrix(i,j)
            endif
        enddo
    enddo

    print *, "Enter the status of the povit /n for no povit chosse 0,for half choose 1, for full choose 2"
    read *, povit_status

    if(povit_status < 0 .or. povit_status > 2) then 
        stop "Invalid Povit option "
    endif


    do i = 1,order-1
 1      if (matrix(i,i) == 0) then
            do k = i+1,order 
                if (matrix(k,i) .ne. 0) then
                    call flip(k,i)
                    goto 1
                endif
            enddo
        endif
        call povit(i)
        do j = i+1,order
            mul = matrix(j,i)/matrix(i,i)
            do k = i+1,order+1
                matrix(j,k) = matrix(j,k) - (mul)*matrix(i,k)
            enddo
        enddo
    enddo

    determinent = 1

    do i = 1,order
        determinent = determinent*matrix(i,i)
    enddo

    if (determinent == 0) then
        stop "No Solution is exist for these equation "
    endif

    allocate(solution(order))

    do i = order,1,-1
        sol = matrix(i,order + 1)
        do j = i,order
            sol = sol - matrix(i,j)*solution(j)
        enddo
        solution(i) = sol/matrix(i,i)
    enddo

    deallocate(matrix)

    print *, "The values of the variable are :: "

    do i = 1,order
        print *, "Value of ",i,"th element is :: "
        do j = 1,order
            if (i == variable(j)) then 
                print *, solution(j)
            endif
        enddo
    enddo

    stop
    
    contains 
        subroutine flip(row1,row2)
            integer ,intent(in)::row1,row2 
            real :: temp_element
            integer :: i
            do i = 1,order+1
                temp_element = matrix(row1,i)
                matrix(row1,i) = matrix(row2,i)
                matrix(row2,i) = temp_element
            enddo
            return  
            end subroutine flip

        subroutine flip_col(col1,col2)   
            integer , intent(in) :: col1,col2
            real :: temp_element
            integer :: i 
            do i = 1,order
                temp_element = matrix(i,col1)
                matrix(i,col1) = matrix(i,col2)
                matrix(i,col2) = temp_element
            enddo
            temp_element = variable(col1)
            variable(col1) = variable(col2)
            variable(col2) = temp_element
            return 
            end subroutine flip_col

        subroutine povit(row)
            integer , intent(in) :: row
            real :: povit_value 
            integer :: row_max_povit , i , col_max_povit , j
            if (povit_status == 0) then 
                return 
            else if (povit_status == 1) then
                povit_value = matrix(row,row)
                do i = row,order
                    if (matrix(i,row) > povit_value) then
                        row_max_povit = i
                        povit_value = matrix(i,row)
                    endif
                enddo
                call flip(row,row_max_povit)
                return 
            else if (povit_status == 2) then
                povit_value = matrix(row,row)
                do i = row,order
                    do j = row,order
                        if (povit_value < matrix(i,j)) then
                            povit_value = matrix(i,j)
                            row_max_povit = i 
                            col_max_povit = j
                        endif
                    enddo
                enddo
                call flip(row,row_max_povit)
                call flip_col(row,col_max_povit)
                return
            endif
            return
            end subroutine povit
                
end program