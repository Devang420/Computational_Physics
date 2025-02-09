program random1final
    implicit none
    integer::i,j !loop variables
    real*8::r,s1,a1,s2,a2,s3,a3,s4,a4,d1,d2,d3,d4 !a for average , s for sum , r for random number
    REAL:: arr(10,10)
    integer, dimension(1:8)::dtseed,seed
    open (unit=1,file='test_ran.dat',action='write')
    !Q1a,b
    write (1,*) 'Generating 10 random numbers'
    print*, 'Generating 10 random numbers'
    do i=1,10
        call random_number(r)
        print*,r
        write(1,*)r
    end do
    write (1,*) '  '
    print*, '  '
    !Q1c
    write (1,*) 'Changing seed and generating 10 new random numbers'
    print*, 'Changing seed and generating 10 new random numbers'
    !Q1d
    call date_and_time(values=dtseed)
    call random_seed(put=dtseed)
    do i=1,10
      call random_number(r)
      write (1,*)r
      print*,r
    end do
    write (1,*) '  '
    print*, '  '
     print*,"Changing seed 10 times and generating 10 random nos each time"
     open(unit=2,file='test_ran_10_seeds.dat',action='write')
     call date_and_time(values=seed)
     seed(5)=seed(5)+10
     call random_seed(put=seed)
     do i=1,10
         do j=1,10
           call random_number(r)
           arr(i,j)= r
         end do
         seed(1)=seed(1)+10
     end do
         write(2,*)"Changing seed 10 times and generating 10 random nos each time"
         write (2,1)arr
         write (*,1)arr
         1 format(10f12.10)
     close(2)
   
    write (1,*) '  '
    print*, '  '
    !Q1e
    write (1,*) 'Now calculating average of the 10 random numbers'
    print*,'Now calculating average of the 10 random numbers'
    do i=1,10
        call random_number(r)
        s1=s1+r
    end do
    a1=s1/i
    print*,"Average of 10 random numbers = ",a1
    write(1,*)"Average of 10 random numbers = ",a1
    write (1,*) '  '
    print*, '  '
    !Q1f
   ! write (1,*) 'Now calculating average of the 100 random numbers'
   ! print*,'Now calculating average of the 100 random numbers'
    do i=1,100
        call random_number(r)
        s2=s2+r
    end do
    a2=s2/i
    print*,"Average of 100 random numbers = ",a2
    write(1,*)"Average of 100 random numbers = ",a2
    write (1,*) '  '
    print*, '  '

  !  write (1,*) 'Now calculating average of the 10,000 random numbers'
  !  print*,'Now calculating average of the 10,000 random numbers'
    do i=1,10000
        call random_number(r)
        s3=s3+r
    end do
    a3=s3/i
    print*,"Average of 10,000 random numbers = ",a3
    write(1,*)"Average of 10,000 random numbers = ",a3
    write (1,*) '  '
    print*, '  '

   ! write (1,*) 'Now calculating average of the 10,00,000 random numbers'
   ! print*,'Now calculating average of the 10,00,000 random numbers'
    do i=1,1000000
        call random_number(r)
        s4=s4+r
    end do
    a4=s4/i
    print*,"Average of 10,00,000 random numbers = ",a4
    write(1,*)"Average of 10,00,000 random numbers = ",a4
    write (1,*) '  '
    print*, '  '
    !Q1g
    d1=abs(0.50d0-a1)
    d2=abs(0.50d0-a2)
    d3=abs(0.50d0-a3)
    d4=abs(0.50d0-a4)
    
    write (1,*) 'Deviation in average from 0.5 for 10 numbers ',d1
    print*, 'Deviation in average from 0.5 for 10 numbers ',d1
    write (1,*) 'Deviation in average from 0.5 for 100 numbers ',d2
    print*, 'Deviation in average from 0.5 for 100 numbers ',d2
    write (1,*) 'Deviation in average from 0.5 for 10,000 numbers ',d3
    print*, 'Deviation in average from 0.5 for 10,000 numbers ',d3
    write (1,*) 'Deviation in average from 0.5 for 10,00,000 numbers ',d4
    print*, 'Deviation in average from 0.5 for 10,00,000 numbers ',d4
    close(1)
    print*,"   "
end program random1final
