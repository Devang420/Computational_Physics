program inttrap
    IMPLICIT NONE
    real*8:: x,h,fa,fb,trap_sum,n,func,diff,pi
    integer :: i
    pi = 4.0D0 * ATAN(1.0D0)
    print*,"Enter the value of n :"
    read*,n
    !calcuating step size
    h=pi/n
    fa= func(0.0d0)/2.0d0
    fb= func(pi)/2.0d0
    trap_sum=0.0d0
    !loop for integration
    do i=1,int(n)-1
        x=h*i
        trap_sum=(trap_sum+ func(x))
    enddo

  trap_sum=h*(trap_sum+fa+fb)
  print*,"value of integral is : ",trap_sum
  print*,n
  diff=2-trap_sum
  print*,"error :", diff
end program inttrap   
real*8 function func(x)
implicit none
real*8:: x
func=sin(x)
end function func