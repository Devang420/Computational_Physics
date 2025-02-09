program name
  implicit none
  integer :: i,j,bin,n,m
  real*8::num1,num2,max,min,range,fmin,dx,r
  real*8, allocatable :: x1(:),x2(:),ran(:)
  integer, allocatable :: freq(:)

  open(1,file = "Exponential1.dat")
  open(2,file = "Exponential2.dat")
  print*,"Enter the number of points"
  read*,n
  allocate(x1(n))
  allocate(x2(n))
  allocate(ran(n))

  do i=1,n
   call random_number(num1)
   call random_number(num2)
   num1=(2*num1)-1
   num2=(2*num2)-1
   r=sqrt(num1*num1+num2*num2)
   x1(i)=2*sqrt(-2*log10(r*r))*num1/r
   x2(i)=2*sqrt(-2*log10(r*r))*num2/r
  end do
 do m=1,2
  if (m==1)then
    ran=x1
  endif
  if (m==2)then
    ran=x2
  endif
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
  do j=1,n
  if((fmin<=ran(j)).and.(ran(j)<(fmin+dx))) then
    freq(i)=freq(i)+1
  end if
  end do
  fmin=fmin+dx
  end do
  do i=1,bin
   print*,min," - ",min+dx," - ",freq(i)
   if (m==1)then
    write(1,*)(min+min+dx)/2," - ",freq(i)," - ",freq(i)/(100000.00*dx)
  endif
  if (m==2)then
    write(2,*)(min+min+dx)/2," - ",freq(i)," - ",freq(i)/(100000.00*dx)
  endif
   min=min+dx
  end do
  deallocate (freq)
 end do
end program name