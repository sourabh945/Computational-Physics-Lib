program simulation 

    ! Refrence for the documantions :: https://www.ijeast.com/papers/86-90,Tesma505,IJEAST.pdf
    
    implicit none

    ! Decleartion of the variables

    real :: decay_prob , atoms_prob , halflife
    integer :: num_atoms , count , interval , i
    logical :: f1
    
    ! Getting the user input for half life time period and intial atoms

    print *, "Enter the half life time period :: "
    read *, halflife
    print *, "Enter the number atoms at start :: "
    read *, num_atoms

    !validing the parameters

    if (halflife <= 0 .or. num_atoms <=0) then
        print *, "input parameters are not correct please enter valid parameters"
        stop "Invalid Parameter"
    endif

    ! Checking and opening the file with status new or replace

    inquire(file="decay.dat",exist=f1) ! for checking the existance of the file
    if (f1) then
        open(1,file="decay.dat",status="replace")
    else
        open(1,file="decay.dat",status="new",action="write")
    endif

    ! Calculating the probability those atoms doesn't decay in this interval but decay in future

    decay_prob = 1 - exp(-(0.693)/halflife)

    ! taking time to zero and write the initial points to the file

    interval = 0

    write(1,*)interval,num_atoms

    ! generating the random-number as atoms_prob and checking for those whoes are less than decay_prob
    ! and count the decay element and write number of left atoms into the file until the zero atoms are left

    do while(num_atoms .ne. 0)
        count = 0
        do i = 1,num_atoms
            call random_number(atoms_prob)      
            if (atoms_prob <= decay_prob) then
                count = count + 1
            endif
        enddo
        interval = interval + 1
        num_atoms = num_atoms - count
        write(1,*)interval,num_atoms
    enddo 

    ! closing the file

    close(1)

    print *, "simulation is complete "
    
    stop

end program