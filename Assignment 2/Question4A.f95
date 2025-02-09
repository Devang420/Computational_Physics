program name
    implicit none
    integer :: i,j,bin
    real*8::ran(100000),max,min,range,fmin,dx
    integer, allocatable :: freq(:)
    open(1,file = "Exponential.dat")
    call random_number(ran)
    do i=1,100000
        ran(i)=0.5*log(1/sqrt(1-(2*ran(i))))
    end do
    
    max=maxval(ran)
    min=minval(ran)
    range=max-min
    print*, max, min,range
    print*,"What is the bin size ?"
    read*,dx
    bin =(1)+(max-min)/dx 
print *, "Number of bins  = ",bin
allocate(freq(bin))
freq=0
fmin=min
do i=1,bin
  do j=1,100000
    if((fmin<=ran(j)).and.(ran(j)<(fmin+dx))) then
      freq(i)=freq(i)+1
    end if
  end do
  fmin=fmin+dx
 end do
 do i=1,bin
 print*,min," - ",min+dx," - ",freq(i)
  write(1,*)(min+min+dx)/2," - ",freq(i)," - ",freq(i)/(100000.00*dx)
  min=min+dx
end do
end program name