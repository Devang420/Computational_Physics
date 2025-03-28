program improved_euler
    implicit none
    real(8) :: x, y, h, x_final, y_pred,err
    integer :: i, n
    character(len=30) :: filename
    real(8), external :: f
    filename = "improved_euler_results.dat"

    ! Function declaration
 

    ! Initial conditions
    x = 0.0_8
    y = 0.0_8
    h = 0.001_8
    x_final = 1.55_8

    ! Number of steps
    n = int((x_final - x) / h)

    ! Open file to save results
    open(unit=10, file=filename, status="replace")

    ! Write header
    write(10,*) "x, y (Improved Euler)"
    write(10,*) x, y

    ! Improved Euler iterations
    do i = 1, n
        y_pred = y + h * f(x, y)  ! Predictor (Euler step)
        y = y + (h / 2.0_8) * (f(x, y) + f(x + h, y_pred))  ! Corrector (averaging slopes)
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
end program improved_euler
 ! Function definition: dy/dx = y^2 + 1
real(8) function f(x, y)
        implicit none
        real(8), intent(in) :: x, y
        f = y**2 + 1
    end function f
