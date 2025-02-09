program inttrap
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    real*8:: x,a,b,h,fa,fb,trap_sum,n
    integer :: i
    real*8:: func
    real*8::diff
    real*8 :: pi

    pi = 4.0D0 * ATAN(1.0D0)

    !asking for vlues for integration
    print*,"Enter the initial and final limits a and b :"
    read*,  a,b
    print*,"Enter the value of n :"
    read*,n
    !calcuating step size
    h=(b-a)/n
    fa= func(a)/2.0d0
    fb= func(b)/2.0d0
    trap_sum=0.0d0

    !loop for integration
    do i=1,int(n)-1
        x=a+h*i
        trap_sum=(trap_sum+ func(x))

    enddo
 trap_sum=h*(trap_sum+fa+fb)
 print*,"value of integral is : ",trap_sum
 diff=pi-trap_sum
 print*,"error :", diff
end program inttrap
real*8 function func(x)
implicit none
real*8:: x
func=4.00/(1.0d0+(x*x))
end function func