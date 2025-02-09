program assigni
    implicit none
    integer:: i,j,c,imax,imin,size,d       
    real::r,max,min,fmin,temp, store_min       
    integer, allocatable :: freq(:)  
    real, allocatable :: sumarray(:) 
    real:: dx(4) 
    open(1,file="i_Normalised_sumdistribution",action='write')
    open(2,file="j_bin2_evenbin_Normalised_sumdistribution",action='write')
    open(8,file="j_bin2_oddbin_Normalised_sumdistribution",action='write')
    open(3,file="j_bin5_evenbin_Normalised_sumdistribution",action='write')
    open(4,file="j_bin10_evenbin_Normalised_sumdistribution",action='write')


    print*,"For how many times do you want the sum?" 
    read*,c  ! for k input 10^5
    allocate (sumarray(c))
    sumarray=0
    do i=1,c
      do j=1,10000 !!for l use 10^5
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
    max=real(imax)
    min=real(imin)
    print*,min
    print*,max
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
print*,"write the bin sizes for which you need the distribution" 
read*, dx
do d=1,4

 if (dx(d)==2.0) then 
        store_min=min-1
        
    size =int(1+(max-min)/dx(d))
   print *, "Number of bins  = ",size
   allocate(freq(size))
   freq=0
 
   do i=1,size
     do j=1,c
       if((store_min<=sumarray(j)).and.(sumarray(j)<(store_min+dx(d)))) then
         freq(i)=freq(i)+1
       end if
     end do
     store_min=store_min+dx(d)  
   end do
   store_min=min-1
   open(11,file="i_Normalised_sumdistribution")
   do i=1,size
     print*,store_min," - ",store_min+dx(d)," - ",freq(i)
     write(8,*)(store_min+store_min+dx(d))/2," - ",freq(i)," - ",freq(i)/(dx(d)*10000.00)
     store_min=store_min+dx(d)
    end do
    deallocate(freq)
 end if


   size =int(1+(max-min)/dx(d))
   print *, "Number of bins  = ",size
   allocate(freq(size))
   freq=0
   fmin=min
 
   do i=1,size
     do j=1,c
       if((fmin<=sumarray(j)).and.(sumarray(j)<(fmin+dx(d)))) then
         freq(i)=freq(i)+1
       end if
     end do
     fmin=fmin+dx(d)  !j add one to make bins odd
   end do
   fmin=min
   open(11,file="i_Normalised_sumdistribution")
   do i=1,size
     print*,fmin," - ",fmin+dx(d)," - ",freq(i)
     write(d,*)(fmin+fmin+dx(d))/2," - ",freq(i)," - ",freq(i)/(dx(d)*10000.00)
     fmin=fmin+dx(d)
    end do
    deallocate(freq)
    print*,"flag"

end do
close(1)
close(2)
close(3)
close(4)
end program assigni