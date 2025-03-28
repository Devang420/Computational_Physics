program particle_simulation
    implicit none
    integer, parameter :: N = 50, niter = 2000
    real(8), parameter :: R = 5.0, dt = 0.02, kappa_m = 1.0
    integer :: i, t, i_next, i_prev
    real(8) :: yim(N), vim(N), x(N), z(N)
    real(8) :: fy0(N), fy1(N), fy2(N), fy3(N)
    real(8) :: fv0(N), fv1(N), fv2(N), fv3(N)
    real(8) :: ytemp1(N), ytemp2(N), ytemp3(N)
    real(8) :: vtemp1(N), vtemp2(N), vtemp3(N)

    ! Initialize ring with all particles at y=0 except for two displaced ones
    yim = 0.0
    vim = 0.0
    yim(1) = 0.8
    yim(26) = 0.8

    ! Position on ring (x, z)
    do i = 1, N
        x(i) = R * cos(2.0 * 3.141592653589793 * i / N)
        z(i) = R * sin(2.0 * 3.141592653589793 * i / N)
    end do

    ! Open file for OVITO visualization
    open(10, file='particles_final.xyz', status='replace')

    ! Simulation loop
    do t = 1, niter

        ! Compute RK4 first step (fy0, fv0)
        do i = 1, N
            i_prev = mod(i-2+N, N) + 1
            i_next = mod(i, N) + 1
            fy0(i) = vim(i)
            fv0(i) = kappa_m * (yim(i_next) + yim(i_prev) - 2.0 * yim(i))
        end do

        ! RK4 Step 2
        do i = 1, N
            ytemp1(i) = yim(i) + fy0(i) * dt / 2.0
            vtemp1(i) = vim(i) + fv0(i) * dt / 2.0
        end do
        do i = 1, N
            i_prev = mod(i-2+N, N) + 1
            i_next = mod(i, N) + 1
            fy1(i) = vtemp1(i)
            fv1(i) = kappa_m * (ytemp1(i_next) + ytemp1(i_prev) - 2.0 * ytemp1(i))
        end do

        ! RK4 Step 3
        do i = 1, N
            ytemp2(i) = yim(i) + fy1(i) * dt / 2.0
            vtemp2(i) = vim(i) + fv1(i) * dt / 2.0
        end do
        do i = 1, N
            i_prev = mod(i-2+N, N) + 1
            i_next = mod(i, N) + 1
            fy2(i) = vtemp2(i)
            fv2(i) = kappa_m * (ytemp2(i_next) + ytemp2(i_prev) - 2.0 * ytemp2(i))
        end do

        ! RK4 Step 4
        do i = 1, N
            ytemp3(i) = yim(i) + fy2(i) * dt
            vtemp3(i) = vim(i) + fv2(i) * dt
        end do
        do i = 1, N
            i_prev = mod(i-2+N, N) + 1
            i_next = mod(i, N) + 1
            fy3(i) = vtemp3(i)
            fv3(i) = kappa_m * (ytemp3(i_next) + ytemp3(i_prev) - 2.0 * ytemp3(i))
        end do

        ! Final RK4 update
        do i = 1, N
            yim(i) = yim(i) + dt * (fy0(i) + 2.0 * (fy1(i) + fy2(i)) + fy3(i)) / 6.0
            vim(i) = vim(i) + dt * (fv0(i) + 2.0 * (fv1(i) + fv2(i)) + fv3(i)) / 6.0
        end do

        ! Print y_1 position at t=40
        if (t == 2000) then
            print *, "Position of particle 1 at t=40: ", yim(1)
        end if

        ! Write to OVITO every 10 steps
        if (mod(t, 10) == 0) then
            write(10,*) N
            write(10,*) "Timestep:", t
            do i = 1, N
                write(10,*) i, x(i), yim(i), z(i)
            end do
        end if

    end do

    close(10)
    print *, "Simulation complete. Check 'particles_corrected.xyz' in OVITO."

end program particle_simulation
