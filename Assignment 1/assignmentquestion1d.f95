program randomfinal
    implicit none
    integer::i,j
    real::r
    REAL:: arr(10,10)
    open(unit=2,file='test_ran_10_seeds.dat',action='write')
    do i=1,10
       do j=1,10
          call random_number(r)
          arr(i,j)= r
        end do
    end do
        write (2,1)arr
        write (*,1)arr
        1 format(10f14.10)
  close(2)
end program randomfinal