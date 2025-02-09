program name
    implicit none
    integer:: i,j,n
    real*8::x(6),p(2),y,func,mc,var,sigma,length,volume,sqrt2,gauss_dev
    length=5.0d0
    volume=acos(-1.0d0)**3
    sqrt2=1.0d0/sqrt(2.0d0)
    open(unit= 1,file="Montecarlo importance sampling method")
    print*,"Enter the value of n"
    read*,n
7   mc=0.0d0
    var=0.0d0
    sigma=0.0d0
    do i=1,n
        call random_number(p)
        do j=1,6
         x(j)=gauss_dev(p)*sqrt2
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
 real*8::x(6),xy
 real*8::a,b
 a=0.5d0
 xy=(x(1)-x(4))**2+(x(2)-x(5))**2+(x(3)-x(6))**2
 func=exp(-a*xy)
 end function
 real*8 function gauss_dev(x)
 implicit none
 real*8::fact, sqr,p,x1,x2,x(2)

7 call random_number(p)
 x1=2.0d0*p-1.0d0
 call random_number(p)
 x2=2.0d0*p-1.0d0

 sqr= x1*x1+x2*x2

 if (sqr.ge.1.0d0.or.sqr.eq.0.0d0) goto 7
 fact=sqrt(-2.0d0*log(sqr)/sqr)
 gauss_dev=x2*fact
end function