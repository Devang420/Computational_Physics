program ising_2d
    implicit none
    integer::i,j,k,L,p,a,b,c,d,g,f,N
 real  ::E
 real::J_ising=1.0
 integer,dimension(:,:,:),allocatable::spin
 print*,'enter the number of lattice points in one dimension'
 read*,L

 allocate(spin(L,L,L))
 E=0.0
 N=L*L*L
 p=0
 do i=1,L
    do j=1,L
        do k=1,L
        spin(k,j,i)=1      
    end do
    end do
 end do

 !calculating initial energy

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
    E=E-J_ising*float((spin(k,j,i))*(spin(a,j,k)+spin(b,j,k)+spin(i,c,k)+spin(i,d,k)+spin(i,j,g)+spin(i,j,f)))
    end do
   end do
 end do
E=E*0.5             !extra counting occurs therefore we divide by 2

print*,'intial energy E',E
 
end program ising_2d