program ising_2d
    implicit none
    integer::i,j,k,L,p,a,b,c,d,g,f,niter,time,mm,nn,oo,N
real  ::r,E,M,mag,Ei,dE,u,Ef,h,mav=0.0,eav=0.0
real::T=3.9,J_ising=1.0
integer,dimension(:,:,:),allocatable::spin
integer::seed
seed=4485
print*,'enter the number of lattice points in one dimension'
read*,L
print*,'enter the number of iterations'
read*,niter                               


allocate(spin(L,L,L))
E=0.0
M=0.0
N=L*L*L

call random_seed
p=0
do i=1,L
    do j=1,L
        do k=1,L
        call random_number(r)
       ! spin(k,j,i)=1
       if(r<0.5) then 
            spin(k,j,i)=-1
        else 
            spin(k,j,i)=+1
        end if
    end do
    end do
end do

 

 do i=1,L
  do j=1,L
    do k=1,L
    a=i+1;b=i-1;c=j+1;d=j-1;g=k+1;f=k-1   
    if(i==L) a=1
    if(i==1) b=L            !periodic boundary condition
    if(j==1) d=L
    if(j==L) c=1
    if(k==1) f=L
    if(k==L) g=1
    M=M+spin(i,j,k)
    E=E-J_ising*float((spin(k,j,i))*(spin(a,j,k)+spin(b,j,k)+spin(i,c,k)+spin(i,d,k)+spin(i,j,g)+spin(i,j,f)))
    end do
  end do
 end do
mag=M/(float(N))
E=E*0.5            

open(10,file='Q6_ising_kbt3.9_L10_init_random.dat')

do time=1,niter
    do mm=1,L
        do nn=1,L
            do oo=1,L
            call random_number(r); i=int(r*float(L))+1       !choose random lattice in x direction 
            call random_number(r); j=int(r*float(L))+1       !choose random lattice in y direction
            call random_number(r); k=int(r*float(L))+1       !choose random lattice in z direction
            a=i+1;b=i-1;c=j+1;d=j-1;g=k+1;f=k-1  

            if(i==L) a=1
            if(i==1) b=L            !periodic boundary condition
            if(j==1) d=L
            if(j==L) c=1
            if(k==1) f=L
            if(k==L) g=1
            !intial energy
            Ei=-J_ising*float((spin(i,j,k))*(spin(a,j,k)+spin(b,j,k)+spin(i,c,k)+spin(i,d,k)+spin(i,j,g)+spin(i,j,f)))
            !trial flip 
            spin(i,j,k)=-spin(i,j,k)
            Ef=-J_ising*float((spin(i,j,k))*(spin(a,j,k)+spin(b,j,k)+spin(i,c,k)+spin(i,d,k)+spin(i,j,g)+spin(i,j,f)))

            dE=Ef-Ei

            if(dE<=0.0)then
               
                E=E+dE    
                M=M+(2.0*float(spin(i,j,k)))  
            else
                u=exp(-dE/(T))
                call random_number(h)
                if(h<u) then                      
                    E=E+dE
                    M=M+(2.0*float(spin(i,j,k)))
                else
                    spin(i,j,k)=-spin(i,j,k)    
                end if
            end if
        end do
    end do
end do
    write(10,*)time,E/float(N),M/float(N)
    mav=mav+M/float(N)
    eav=eav+E/float(N)
end do
mav=mav/niter
eav=eav/niter
print*, 'M per spin fluctuates about ',mav
print*, 'E per spin fluctuates about ',eav
end program ising_2d