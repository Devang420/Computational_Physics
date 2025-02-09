program assigni
    implicit none
    integer:: i,j,c,imax,imin,size       
    real::r,max,min,fmin,temp,dx        
    integer, allocatable :: freq(:)
    real, allocatable :: sumarray(:) 
  
    open(11,file="Lnormsum")
    print*,"For how many times do you want the sum?" 
    read*,c  ! for k input 10^5
    allocate (sumarray(c))
    sumarray=0
    do i=1,c
      do j=1,100000!!for l use 10^5
        call random_number(r)
        if(mod(int(r*10),2)==0) then
        r=-1
        end if
        if(mod(int(r*10),2)/=0) then
            r=1
            end if
        sumarray(i)=sumarray(i)+r
      end do
    end do
    print*,sumarray
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
print*,"write the bin size for which you need the distribution" 
read*, dx
 size =1+(max-min)/dx
 print *, "Number of bins  = ",size
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

 do i=1,size
 print*,min," - ",min+dx," - ",freq(i)
   write(11,*)(min+min+dx)/2," - ",freq(i)," - ",freq(i)/(100000.00*dx)
   min=min+dx
 end do
 close(11)
end program assigni