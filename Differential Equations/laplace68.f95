program laplace
implicit none

integer, parameter :: lx=68, ly=68 ! BOUNDARIES at x,y =0 and x,y=68.
real*8 :: temp(1:lx,1:ly), old_temp(1:lx,1:ly)  !selection of array from 1 to lx etc.
integer :: i,j,ii,jj,kk
real*8 :: bound_temp, increment_temp, dx, dy, prefactor

integer ::  ci, test, counter

bound_temp=0.0d0
increment_temp=0.05d0

old_temp=0.0d0

do i=1, ly
    old_temp(1,i) = 3.70d0
    old_temp(lx,i) = 0.40d0
enddo

do i=2, lx-1
    old_temp(i,1) = old_temp(i-1,1) - increment_temp
    old_temp(i,ly) = old_temp(i-1,ly) -increment_temp
enddo


temp=old_temp

ci=0

open(12, file="old_tempq68.dat",status='replace') ! WRITE DOWN THE BOUNDARY CONDITIONS at ZEROTH ITERATION
do ii=1, lx
    do jj=1, ly
        write(12,*) ii, jj, old_temp(ii,jj)
    enddo
enddo
close(12)

dx=0.05d0
dy=0.05d0
test=0
counter=0
prefactor = (0.5d0*dx*dx*dy*dy)/(dx*dx + dy*dy)

do ! LOOP OVER ITERATIONS
    counter = counter +1
    test=0

    do jj=2, ly-1 ! UPDATE STEP
        do ii=2, lx-1
            temp(ii,jj) = 0.25d0*( old_temp(ii-1,jj) + old_temp(ii+1,jj) + old_temp(ii,jj-1) + old_temp(ii,jj+1) )
        enddo
    enddo

    do jj=2, ly-1 ! CHECKING FOR CONVERGENCE AT EACH LATTICE SITE
        do ii=2, lx-1
            if((abs(temp(ii,jj) - old_temp(ii,jj))).gt.0.0001d0) test=1
        enddo
    enddo

    if(test.eq.0) exit ! EXIT CONDITION
    old_temp = temp ! AFTER TEST CONDITION
enddo

write(12,*) 'counter', counter

! WRITE THE CONVERGED RESULT SOLUTION TO THE EQN with BOUNDARY CONDITIONS


open(10, file="pdeq68_output.dat" ,status='replace')
do ii=lx, 1,-1
   
        write(10,*) temp(1:68,i)
    enddo
print*,"Temperature at 20,20 = ",temp(20,20)
print*,"Temperature at 40,40 = ",temp(40,40)
close(10)
endprogram laplace