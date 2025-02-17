program ising_2d
    implicit none
    integer::i,j,k,L,p,a,b,c,d,g,f,N
real  ::E,M
integer,dimension(:,:,:),allocatable::spin
print*,'enter the number of lattice points in one dimension'
read*,L
allocate(spin(L,L,L))
E=0.0
M=0.0
N=L*L*L
p=0
do i=1,L
    do j=1,L
     do k=1,L
        
       spin(k,j,i)=-1
       
        end do
    end do
 end do

 !now we are calculating initial magnetization and energy

 do i=1,L
  do j=1,L
    do k=1,L

     ! identify the neighbour

     a=i+1;b=i-1;c=j+1;d=j-1;g=k+1;f=k-1   

     if(i==L) a=1
     if(i==1) b=L            !periodic boundary condition
     if(j==1) d=L
     if(j==L) c=1
     if(k==1) f=L
     if(k==L) g=1
     M=M+spin(i,j,k)
     end do
  end do
 end do

 print*,'intial magnetisation M =',M
 end program ising_2d
    
