program runge_kutta
    implicit none
    integer, parameter :: n_steps = 5000
    real(8), parameter :: dt = 0.001_8
    real(8) :: t, x, v, k1x, k1v, k2x, k2v, k3x, k3v, k4x, k4v
    real(8) :: energy, kinetic, potential, force
    integer :: i
  
    ! Initial conditions
    t = 0.0_8
    x = 0.0_8
    v = 2.0_8
  
    ! Open file to store results
    open(unit=10, file='Q5x=0.00,v=0.1.dat', status='replace')
    
    ! Compute initial values
    force = -sin(x)
    kinetic = 0.5_8 * v**2
    potential = -cos(x)  ! Potential energy from force = -sin(x)
    energy = kinetic + potential
    
    !write(10, '(F10.5, 5F15.8)') t, x, v, force, kinetic, potential, energy
  
    ! Runge-Kutta 4th order integration
    do i = 1, n_steps
      ! Compute RK4 coefficients
      k1x = dt * v
      k1v = dt * (-sin(x))
      
      k2x = dt * (v + 0.5_8 * k1v)
      k2v = dt * (-sin(x + 0.5_8 * k1x))
      
      k3x = dt * (v + 0.5_8 * k2v)
      k3v = dt * (-sin(x + 0.5_8 * k2x))
      
      k4x = dt * (v + k3v)
      k4v = dt * (-sin(x + k3x))
      
      ! Update variables
      x = x + (k1x + 2.0_8*k2x + 2.0_8*k3x + k4x) / 6.0_8
      v = v + (k1v + 2.0_8*k2v + 2.0_8*k3v + k4v) / 6.0_8
      t = t + dt
  
      ! Compute force and energy
      force = -sin(x)
      kinetic = 0.5_8 * v**2
      potential = -cos(x)
      energy = kinetic + potential
  
      ! Write results to file
      write(10,*) t, x, v, force, kinetic, potential, energy
    end do
  
    close(10)
    print *, 'Simulation complete. Results saved to solution.dat'
  
  end program runge_kutta
  