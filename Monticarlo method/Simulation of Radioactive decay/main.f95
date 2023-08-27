program simulation

    integer steps , num_atoms , count
    real halflife , totaltime , intervals , decay_prob , atom_prob 
    logical f1


    print *, "Enter the value of the half life time period of the material"
    read(*,*)halflife
    print *, "Enter the number of the atoms initially"
    read(*,*)num_atoms
    print *, "Enter the steps for calculation"
    read(*,*)steps

    inquire(file="decay.dat",exist=f1) ! for checking the existance of the file
    if (f1) then
        open(1,file="decay.dat",status="replace")
    else
        open(1,file="decay.dat",status="new",action="write")
    endif


    totaltime = 10*halflife
    intervals = totaltime/(real(steps))

    write(1,*) intervals*0 , num_atoms

    do i = 1,steps
        count = 0
        decay_prob = (log(2.0)/halflife)*(intervals*i)
        print *, decay_prob
        do j = 1, num_atoms
            call random_number(atom_prob)
            if (atom_prob <= decay_prob) then
                count = count + 1
            endif
        enddo
        num_atoms = num_atoms - count
        write(1,*) intervals*i , num_atoms
    enddo

    close (1)

    call execute_command_line("gnuplot -presist -e 'plot "decay.dat"  using 1:2' ")

    print *, "simulation is end"

    stop
    end program





