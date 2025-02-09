program sum10000
  implicit none
  integer::i,j,imax,imin,size          ! i,j are loop variables, imax and imin store floored and cieled values of max and min
  real*8::r,max,min,fmin,dx,temp          ! max and min are understood,fmin is same as min but used in  loop,dx is the bin size
  real*8,dimension(10000)::sum
  integer, allocatable :: freq(:),binwidth(:)        
open(10,file="Sumof10,000RandomNumbers,10,000Times.dat")
do i=1,10000
  do j=1,10000
      call random_number(r)
      sum(i)=sum(i)+r    
  enddo
enddo

max=maxval(sum)
min=minval(sum)
imin=floor(min)
imax=floor(max)+1.0

!!sorting the sum array
do i = 1, 10000
  do j = 1, 10000-i
      if (sum(j) > sum(j+1)) then
          temp = sum(j)
          sum(j) = sum(j+1)
          sum(j+1) = temp
      end if
  end do
end do

max=real(imax)
min=real(imin)
print*,"Enter the bin size "
read*,dx
size =1+(max-min)/dx 
print *, "Number of bins  = ",size
allocate(freq(size))
freq=0
fmin=min
do i=1,size
  do j=1,10000
    if((fmin<=sum(j)).and.(sum(j)<(fmin+dx))) then
      freq(i)=freq(i)+1
    end if
  end do
  fmin=fmin+dx
end do
do i=1,size
print*,min," - ",min+dx," - ",freq(i)
  write(11,*)(min+min+dx)/2," - ",freq(i)," - ",freq(i)/(10000.00*dx)
  min=min+dx
end do
close(10)
end program sum10000