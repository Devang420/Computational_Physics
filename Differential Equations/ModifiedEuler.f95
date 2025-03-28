program modified_euler
    implicit none
    real*8 :: x, y, h, x_final, y_mid,err
    integer :: i, n
    real*8, external :: f2
    character(len=30) :: filename
    filename = "modified_euler_results.dat"

    ! Initial conditions
    x = 0.0d0
    y = 0.0d0
    h = 0.001d0
    x_final = 1.55d0

    ! Number of steps
    n = int((x_final - x) / h)

    ! Open file to save results
    open(unit=10, file=filename, status="replace")

    ! Write header
    write(10,*) "x, y"
    write(10,*) x, y

    ! Modified Euler (Midpoint Method) iterations
    do i = 1, n
        y_mid = y + (h / 2.0d0) * f2(x, y)  ! Midpoint predictor
        y = y + h * f2(x + h/2.0d0, y_mid)  ! Corrected step
        x = x + h

        ! Save results to file
        write(10,*) x, y
        if(i==n)then
            err=48.078-y
            print*,"error = ",err
            endif
    end do

    ! Close file
    close(10)

    print *, "Results saved to ", filename
end program modified_euler

    ! Function definition: dy/dx = y^2 + 1
    real*8 function f2(x, y)
        implicit none
        real*8, intent(in) :: x, y
        f2 = y**2.0d0 + 1.0d0
    end function f2

