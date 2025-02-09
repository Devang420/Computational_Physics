program inttrap
    IMPLICIT NONE
    real*8:: x,a,b,fa,fb,func,pi
    integer :: i,j
    real*8 :: h(4),n(4),trap_sum(4),diff(4)
    pi = 4.0D0 * ATAN(1.0D0)

    !asking for vlues for integration
    print*,"Enter the initial and final limits a and b :"
    read*,  a,b
    print*,"Enter the values of step size :"
    read*,h
    !calcuating step size
    do i=1,4
     n(i)=(b-a)/h(i)
     fa= func(a)/2.0d0
     fb= func(b)/2.0d0
     trap_sum(i)=0.0d0

     !loop for integration
       do j=1,int(n(i))-1
        x=a+h(i)*j
        trap_sum(i)=(trap_sum(i)+ func(x))
       enddo
     trap_sum(i)=h(i)*(trap_sum(i)+fa+fb)
     print*,"value of integral for ",n(i)," bins is : ",trap_sum(i)
     diff(i)=1.0d0-trap_sum(i)
     print*,"error :", diff(i)
    end do
end program inttrap
real*8 function func(x)
implicit none
real*8:: x,pi
pi = 4.0D0 * ATAN(1.0D0)
func=exp((-x*x)/2)/sqrt(2*pi)
end function func