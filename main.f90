program main
    use seisDeconv
    implicit none
    integer, parameter :: sp = 52, sr = 200
    integer :: zp, zx, zrd, zf, srd, sf
    real(kind=8), dimension(:), allocatable :: Ref
    real(kind=8), dimension(:), allocatable :: Psi
    real(kind=8), dimension(:), allocatable :: x
    real(kind=8), dimension(:), allocatable :: rDec
    real(kind=8), dimension(:), allocatable :: f

    allocate(Ref(sr))
    
    Ref = genReflect(sr)

    call writeSignal(Ref, 1, sr, 'bins/reflect.data')

    allocate(Psi(sp))

    call genPulse(Psi, zp, 1.d0, 0.03d0, 1.d0/20.d0, sp)

    call writeSignal(Psi, zp, sp, 'bins/pulse.data')

    call conv(Psi, Ref, zp, 1, sp, sr, x, zx)

    call writeSignal(x, zx, sp + sr - 1, 'bins/signal.data')

    call deconv(x, psi, zx, zp, sp + sr - 1, sp, rDec, zrd, srd, f, zf, sf)

    deallocate(f)
    deallocate(rDec)
    deallocate(Psi)
    deallocate(Ref)
    deallocate(x)

end program main