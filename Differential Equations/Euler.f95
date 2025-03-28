program euler_method
    implicit none
    real*8 :: x, y, h, x_end,err
    integer :: n, i
    character(len=20) :: filename1,filename2
    integer :: unit
    real(8), external :: f

    ! Main program
    x = 0.0d0   ! Initial condition x0
    y = 0.0d0    ! Initial condition y0
    h = 0.001d0   ! Step size
    x_end = 1.55d0 ! Final x value
    filename1 = "euler_results.dat"
    filename2 = "tan_results.dat"
    unit = 10  ! File unit number

    n = int((x_end - x) / h)  ! Calculate number of steps


    ! Open the file for writing
    open(unit, file=filename1, status="replace", action="write")
    open(12, file=filename2,status="replace", action="write")

    print *, "Solving using Euler's method with h = 0.01..."
    print *, "Results will be saved to euler_results.dat"
    print *, "--------------------------------"
    print *, "  x        y"
    print *, "--------------------------------"

    ! Write header to file
    write(unit, *) "  x        y"

    ! Euler's method loop
    do i = 0, n
        print*, x, y
        write(unit, *) x, y !,tan(x) ! Write to file
        write(12,*)x,tan(x)
        y = y + h * f(x, y)
        x = x + h
        if(i==n)then
            err=48.078-y
            print*,"error = ",err
            endif
    end do

    print *, "--------------------------------"
    print *, "Computation finished. Results saved"

    ! Close the file
    close(unit)
      ! Define the function f(x, y) = y^2 + 1
    
 end program euler_method

    real*8 function f(x, y)
        implicit none
        real*8, intent(in) :: x, y
        f = y**2.0d0 + 1.0d0
    end function f