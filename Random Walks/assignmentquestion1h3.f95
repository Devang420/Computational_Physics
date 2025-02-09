program h3
    implicit none
    integer:: i,j,c,imax,imin,size        
    real*8::r,max,min,fmin,dx,temp,total        
    integer, allocatable :: freq(:),binwidth(:)  
    real, allocatable :: sumarray(:) 

    print*,"For how many times do you want the sum?" 
    read*,c
    allocate (sumarray(c))
    sumarray=0
    do i=1,c
      do j=1,10000
        call random_number(r)
        r=(2*r)-1
        sumarray(i)=sumarray(i)+r
      end do
    end do
    total=sum(sumarray)
    !print*,sumarray
    max=maxval(sumarray)
    min=minval(sumarray)
    imin=floor(min)
    imax=floor(max)+1.0

!!sorting the sumarray array
do i = 1, c
  do j = 1, c-i
      if (sumarray(j) > sumarray(j+1)) then
          temp = sumarray(j)
          sumarray(j) = sumarray(j+1)
          sumarray(j+1) = temp
      end if
  end do
end do
max=real(imax)
min=real(imin)
dx=2 !bin size=2
size =1+(max-min)/dx 
allocate(freq(size))
freq=0
fmin=min
do i=1,size
  do j=1,c
    if((fmin<=sumarray(j)).and.(sumarray(j)<(fmin+dx))) then
      freq(i)=freq(i)+1
    end if
  end do
  fmin=fmin+dx
end do
open(11,file="Normalised_distribution_for_10,000_h3")
do i=1,size
print*,min," - ",min+dx," - ",freq(i)
  write(11,*)(min+min+dx)/2," - ",freq(i)," - ",freq(i)/(10000*dx)
  min=min+dx
end do
close(11)
end program h3