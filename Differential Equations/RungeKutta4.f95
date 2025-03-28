program rk4_method
    implicit none
    real(8) :: x, y, h, x_final,err
    integer :: i, n,j
    character(len=30) :: filename
    filename = "rk4_results.dat"
    ! Step size and initial conditions
    x = 0.0d0
    y = 0.0d0
    h = 0.01d0
    x_final = 1.55_8
    ! Number of steps
    n = int((x_final - x) / h)
    ! Open file to save results
    open(unit=10, file=filename, status="replace")
    write(10,*) "x, y (RK4 Method)"
    write(10,"(F10.5, F15.8)") x, y
    ! RK4 Iterations
    !write(10,*),0,0
    do i = 1, n
        call rk4_step(x, y, h)
       ! do j =1,9
        !  write(10,*)"",""
        !end do
        write(10,"(F10.5, F15.8)") x, y 
        if(i==n)then
            err=48.078-y
            print*,"error = ",err
            endif
    end do
    ! Close file
    close(10)
    print *, "Results saved to ", filename
contains
    ! Function for dy/dx = y^2 + 1
    real(8) function f(x, y)
        real(8), intent(in) :: x, y
        f = y**2 + 1.0_8
    end function f
    ! RK4 Step function
    subroutine rk4_step(x, y, h)
        real(8), intent(inout) :: x, y
        real(8), intent(in) :: h
        real(8) :: k1, k2, k3, k4

        k1 = h * f(x, y)
        k2 = h * f(x + h/2.0_8, y + k1/2.0_8)
        k3 = h * f(x + h/2.0_8, y + k2/2.0_8)
        k4 = h * f(x + h, y + k3)

        y = y + (k1 + 2.0_8*k2 + 2.0_8*k3 + k4) / 6.0_8
        x = x + h
    end subroutine rk4_step
end program rk4_method
