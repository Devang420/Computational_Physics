program name
    implicit none
    integer :: i,k,nop,cond,ll=0
    real*8 :: dx=0.010d0, limit=0.000010d0,initialpt=0.0d0, finalpt=1.0d0 
    real*8, allocatable :: x(:), y(:), y_ini(:)
    real*8 :: a,b,c,d 
    nop=int((finalpt-initialpt)/dx)
    print*,nop 
    allocate(x(nop))
    allocate(y(nop))
    allocate(y_ini(nop))
    x=0.0d0 
    x(nop)=1.0d0 
    y(1)=0.0d0 
    y(nop)=2.0d0
    do i=2,nop-1
        x(i)=x(i-1)+dx
        y(i)=(y(nop)-y(1))*x(i)/x(nop)-x(1)
    end do
    open(file='finitediffinitial.dat',unit=2)
    do i=1,nop 
        write(2,*) x(i), y(i)
    end do
    k=0
    a=1.0d0/(2.0d0-10.0d0*dx*dx)
    b=1.0d0-(5.0d0*dx/2.0d0)
    c=1.0d0+(5.0d0*dx/2.0d0)
    d=10.0d0*dx*dx
    do
        ll=ll+1
        k=k+1
        if(cond==1) exit
        y_ini=y 
        do i=2,nop-1
            y(i)=a*(b*y_ini(i+1)+c*y_ini(i-1)-d*x(i))
        end do
        cond=1
        do i=2,nop-1
            if(abs(y(i)-y_ini(i)).ge.limit) cond=0
        end do
    end do
    open(file='finitedifffinal.dat', unit=3)
    do i=1,nop
        write(3,*) x(i), y(i)
    end do
    print*, "Number of iterations required = ",ll
end program name