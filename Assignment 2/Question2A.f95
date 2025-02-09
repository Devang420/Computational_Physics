program module2
    implicit none
    integer::i,rn,k,j!loop variables
    real*8::sums,sumsq,avg,avgsq,sigma,corr!a for average , s for sum , r for random numb
    real*8,allocatable::r(:)
    open(1,file='Correlationfunction.dat')
    open(9,file='Scatterplot.dat')
    print*,"How many random numbers are needed?"
    read*, rn
    allocate(r(rn))
    call random_number(r)
    sums=sum(r)
    sumsq=sum(r*r)
    avg=sums/real(rn)
    avgsq=sumsq/real(rn)
    sigma=sqrt(avgsq-(avg*avg))
    print*,"Value of mean ",avg
    print*,"Value of Standard Deviation about mean is  ",sigma
    sums=0.0d0
    do j=1,rn
        k=j-1
        do i=1,rn-k
            sums=sums+(r(i)*r(i+k))
        end do
        sums=sums/real(rn-k)
        corr=(sums-(avg*avg))/(sigma*sigma)
        write(1,*)k,"-",corr
    end do
    do i=1,rn-1
        write(9,*)r(i),r(i+1)
    end do
end program module2