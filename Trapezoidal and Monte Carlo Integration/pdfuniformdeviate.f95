program random_distribution
    implicit none
    integer, parameter :: n = 100000 ! Number of random numbers to generate
    real(8) :: random_num
    integer :: i
    open(unit=10, file="random_numbers.dat")  ! File to store random numbers

    ! Initialize the random number generator
    call random_seed()
    
    ! Generate n random numbers and write them to the file
    do i = 1, n
        call random_number(random_num)  ! Generate a random number between 0 and 1
        write(10,*) random_num  ! Write the number to the file
    end do

    close(10)
    print *, "Random numbers have been generated and saved to random_numbers.dat"
end program random_distribution
