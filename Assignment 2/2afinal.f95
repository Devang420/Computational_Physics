program random_distribution
    implicit none
    integer, parameter :: n = 100 ! Number of random numbers to generate
    real(8) :: random_num(n),dx,mini,maxi,fmin
    integer :: i,bin,j
    integer, allocatable :: freq(:)
    open(unit=10, file="random_numbers.dat")  ! File to store random numbers
    
    call random_number(random_num)  ! Generate a random number between 0 and 1
    mini=minval(random_num)
    maxi=maxval(random_num)
    print*,"What is the bin size ?"
    read*,dx
    bin=1/dx
    allocate(freq(bin))
    freq=0
    fmin=mini
    do i=1,bin
        do j=1,n
        if((fmin<=random_num(j)).and.(random_num(j)<(fmin+dx))) then
          freq(i)=freq(i)+1
        end if
        end do
        fmin=fmin+dx
    enddo
    do i=1,bin
        print*,mini," - ",mini+dx," - ",freq(i)
          write(10,*)(mini+mini+dx)/2," - ",freq(i)," - ",freq(i)/(100.00*dx)
          mini=mini+dx
        end do
    close(10)
    print *, "Random numbers have been generated and saved to random_numbers.dat"
end program random_distribution