program main

    logical :: f1

    inquire(file="data.dat",exist=f1)
    if(f1) then 
        open(1,file="data.dat",action="write",position="append")
    else
        open(1,file="data.dat",status="new",action="write")
    endif
    do i = 100,200
        write(1,*)i
    enddo

    stop

    contains
        integer function func()
            return
        end function func
 
end program 