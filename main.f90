program main
    use seisDeconv
    implicit none
    integer, parameter :: sp = 50, sr = 1000
    integer :: zp, zx, zf, sf
    real(kind=8), dimension(:), allocatable :: Ref
    real(kind=8), dimension(:), allocatable :: Psi
    real(kind=8), dimension(:), allocatable :: x
    real(kind=8), dimension(:), allocatable :: f

    allocate(Ref(sr))
    
    Ref = genReflect(sr)

    call writeSignal(Ref, 1, sr, 'bins/reflect.data')

    allocate(Psi(sp))

    call genPulse(Psi, zp, 1.d0, 0.03d0, 1.d0/20.d0, sp)

    call writeSignal(Psi, zp, sp, 'bins/pulse.data')

    call conv(Psi, Ref, zp, 1, sp, sr, x, zx)

    call writeSignal(x, zx, sp + sr - 1, 'bins/signal.data')

    allocate(f(sp))

    call deconv(Psi, zp, sp, f, zf, sf)

    deallocate(f)
    deallocate(Psi)
    deallocate(Ref)
    deallocate(x)

end program main