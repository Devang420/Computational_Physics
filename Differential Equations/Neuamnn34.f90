program laplace_neumann
    implicit none
    
    integer, parameter :: lx=34, ly=34 ! BOUNDARIES at x,y =0 and x,y=33.
    real*8 :: old_temp(1:lx,1:ly), temp(1:lx,1:ly)
    integer :: i,j,jj,ii
    real*8 :: dx,dy, prefactor,shift_temp
    real*8 :: A(1:ly),B(1:ly),C(1:lx),D(1:lx)
    

    integer :: test,counter
    
    A=-70.0d0; B=-40.0d0; C=20.0d0; D=-10.0d0
    
    old_temp=0.0d0 ! at zeroth iteration
    
    dx=1.0d0; dy=dx
    test=0; counter = 0; prefactor = (0.5d0*dx*dx*dy*dy)/(dx*dx + dy*dy)
    
    do ! LOOP OVER ITERATIONS
        counter = counter +1
        test=0
    
        ! UPDATE THE BOUNDARIES: NEUMANN CONDITIONS : LEAVING OUT CORNERS.
        do j=2,ly-1
            temp(1,j) = 0.25d0*( 2.0d0*old_temp(2,j) - 2.0d0*dx*A(j) + old_temp(1,j+1) + old_temp(1,j-1) )
            temp(lx,j) = 0.25d0*( 2.0d0*old_temp(lx-1,j) + 2.0d0*dx*B(j) + old_temp(lx,j+1) + old_temp(lx,j-1) )
        enddo
    
        do i=2,lx-1
            temp(i,1) = 0.25d0*( old_temp(i+1,1) + old_temp(i-1,1) + 2.0d0*old_temp(i,2) - 2.0d0*dx*C(i) )
            temp(i,ly) = 0.25d0*( old_temp(i+1,ly) + old_temp(i-1,ly) + 2.0d0*old_temp(i,ly-1) + 2.0d0*dx*D(i) )
        enddo
    
        ! update THE VALUES AT 4 CORNERS
        temp(1,1) = 0.5d0*( old_temp(1,2) - dx*C(1) + old_temp(2,1) - dx*A(1) )
        temp(1,ly) = 0.5d0*( old_temp(1,ly-1) + dx*D(1) + old_temp(2,ly) - dx*A(ly) )
        temp(lx,1) = 0.5d0*( old_temp(lx-1,1) + dx*B(1) + old_temp(lx,2) -dx*c(lx) )
        temp(lx,ly) = 0.5d0*( old_temp(lx-1,ly) + dx*B(ly) + old_temp(lx,ly-1) + dx*D(lx) )
    
        do jj=2,ly-1 ! UPDATE STEP: THE INTERIOR of the LATTICE
            do ii=2,lx-1
                temp(ii,jj) = 0.250d0*( old_temp(ii-1,jj) + old_temp(ii+1,jj) + old_temp(ii,jj-1) + old_temp(ii,jj+1) )
            enddo
        enddo
    
        do jj=1,ly ! CHECKING FOR CONVERGENCE AT EACH LATTICE SITE
            do ii=1,lx
                if(abs(temp(jj,ii) - old_temp(jj,ii)).gt.0.00001d0) test=1
            enddo
        enddo
    
        if(test.eq.0) exit ! EXIT CONDITION.
    
        shift_temp = 2000.0d0 - temp(1,1) 
        
        temp = temp + shift_temp
    
        old_temp= temp ! AFTER TEST CONDITION
    enddo
    
    write(*,*) 'counter', counter
    
    open(10, file="pdeq4_34output.dat" ,status='replace')
do ii=  1 ,lx 
        write(10,*)  temp(1:34,ii)
    enddo
close(10)
write(*,*) "your file is ready "

write(*,*) temp(10,10)
endprogram laplace_neumann