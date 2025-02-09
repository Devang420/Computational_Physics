program name
    implicit none
    integer:: i,j,n
    real*8::x(6),p(6),y,func,mc,var,sigma,length,volume
    length=5.0d0
    volume=(2*length)**6
    open(unit= 1,file="Montecarlo brute force")
    print*,"Enter the value of n"
    read*,n
 7  mc=0.0d0
    var=0.0d0
    sigma=0.0d0
    do i=1,n
        call random_number(p)
        do j=1,6
         x(j)=2.0d0*length*p(j)-length
        end do
        mc=mc+func(x)
        sigma=sigma+(func(x)*func(x))
    end do
    mc=mc/real(n)
    sigma=sigma/real(n)
    var=sigma-(mc*mc)
    mc=volume*mc
    sigma=volume*sqrt(var/real(n))
    write(1,*)n," ",mc," ",sigma

    n=n*10
    if(n<1000000000) goto 7
end program name
real*8 function func(x)
implicit none
real*8::x(6),xx,yy,xy
real*8::a,b
a=1.0d0
b=0.5d0
xx=x(1)*x(1)+x(2)*x(2)+x(3)*x(3)
yy=x(4)*x(4)+x(5)*x(5)+x(6)*x(6)
xy=(x(1)-x(4))**2+(x(2)-x(5))**2+(x(3)-x(6))**2
func=exp(-a*xx-a*yy-b*xy)
end function