program guass_elmination
    
    implicit none

    ! declaring the variables

    real , dimension(:,:),allocatable :: matrix
    real , dimension(:) , allocatable ::  solution , variable
    integer :: order , i , j , k , povit_status
    real :: determinent , sol , mul

    ! getting the number of the variable and equation and validating it

    print *, "Enter the number of the variable :: "
    read *, order 

    if(order<=0) then
        stop "Invalid number of the variable"
    endif

    ! allocate the matrix and the variable array in the heap memory

    allocate(matrix(order,order+1),variable(order)) 

    ! the variable array use to track the flip column operation and find which variable is where

    ! defining the variable array

    do i = 1,order
        variable(i) = i
    enddo

    ! getting the equation the form of the matrix

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

    ! getting the povit status from the user and validate it 

    print *, "Enter the status of the povit /n for no povit chosse 0,for half choose 1, for full choose 2"
    read *, povit_status

    if(povit_status < 0 .or. povit_status > 2) then 
        stop "Invalid Povit option "
    endif

    ! making the upper triangular matrix from the given matrix by row operation

    do i = 1,order-1
1       if (matrix(i,i) == 0) then
            do k = i+1,order ! check that the povit element is zero or not
                if (matrix(k,i) .ne. 0) then
                    call flip(k,i) ! changing the povit element by flipping the row
                    goto 1
                endif
            enddo
        endif
        call povit(i) ! doing poviting in the matrix with the status user define
        do j = i+1,order
            mul = matrix(j,i)/matrix(i,i)
            do k = i+1,order+1
                matrix(j,k) = matrix(j,k) - (mul)*matrix(i,k)
            enddo
        enddo
    enddo

    ! find the determinent of the upper triangular matrix and check it is not equal to zero

    determinent = 1

    do i = 1,order
        determinent = determinent*matrix(i,i)
    enddo

    if (determinent == 0) then
        stop "No Solution is exist for these equation "
    endif

    ! allocating the solution array in which we store the solution for equation

    allocate(solution(order))

    ! here find the solution and store it into solution array

    do i = order,1,-1
        sol = matrix(i,order + 1)
        do j = i,order
            sol = sol - matrix(i,j)*solution(j)
        enddo
        solution(i) = sol/matrix(i,i)
    enddo

    ! deallocating the matrix array to clear heap memory

    deallocate(matrix)

    print *, "The values of the variable are :: "

    ! finding the match for the variable and solution of the equaitons

    do i = 1,order
        print *, "Value of ",i,"th element is :: "
        do j = 1,order
            if (i == variable(j)) then 
                print *, solution(j)
            endif
        enddo
    enddo

    ! clearing the heap memory by deallocating the all array stored

    deallocate(variable,solution)

    stop
    
    ! subroutine use for the operations

    contains 
        subroutine flip(row1,row2) ! this subroutine use to flip the rows 
            
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

        subroutine flip_col(col1,col2) ! this subroutine use to flip the column   
            
            integer , intent(in) :: col1,col2 
            real :: temp_element
            integer :: i 

            do i = 1,order 
                temp_element = matrix(i,col1)
                matrix(i,col1) = matrix(i,col2)
                matrix(i,col2) = temp_element
            enddo
            temp_element = variable(col1) ! this is also flip the corresponding variables in the variable array
            variable(col1) = variable(col2)
            variable(col2) = temp_element

            return 
        end subroutine flip_col

        subroutine povit(row) ! this subroutine is use for to do poviting in the matrix per user define paviting status ( pavit_status)
            
            integer , intent(in) :: row
            real :: povit_value 
            integer :: row_max_povit , i , col_max_povit , j

            if (povit_status == 0) then ! for no poviting 
                return 

            else if (povit_status == 1) then ! for half poviting 
                povit_value = matrix(row,row)
                do i = row,order
                    if (matrix(i,row) > povit_value) then
                        row_max_povit = i
                        povit_value = matrix(i,row)
                    endif
                enddo
                call flip(row,row_max_povit) ! only flip the row which have maximum povit element
                return 

            else if (povit_status == 2) then ! for do full poviting in the matrix
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
                call flip(row,row_max_povit) ! here we check the element in the whole remaining matrix
                call flip_col(row,col_max_povit)
                return
            endif

            return
        end subroutine povit
                
end program