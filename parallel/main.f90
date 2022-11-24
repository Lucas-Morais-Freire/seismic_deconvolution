program main
    use omp_lib
    use seisDeconv
    implicit none
    integer, parameter :: sp = 51, sr = 1000
    integer :: zp, zx, zf, sf, zd, zc
    real(kind=8), dimension(:), allocatable :: Ref
    real(kind=8), dimension(:), allocatable :: Psi
    real(kind=8), dimension(:), allocatable :: x
    real(kind=8), dimension(:), allocatable :: f
    real(kind=8), dimension(:), allocatable :: deconvRef
    real(kind=8), dimension(:), allocatable :: scf

    call omp_set_num_threads(4)

    call initRandom()

    allocate(Ref(sr))
    
    Ref = genReflect(sr, 2.5d0, 0.d0)
    
    call writeSignal(Ref, 1, sr, 'bins/reflect.data')
    
    allocate(Psi(sp))
    
    call genPulse(Psi, zp, 1.d0, 0.03d0, 1.d0/20.d0, sp)
    
    call writeSignal(Psi, zp, sp, 'bins/pulse.data')
    
    call conv(Psi, Ref, zp, 1, sp, sr, x, zx)

    call writeSignal(x, zx, sp + sr - 1, 'bins/signal.data')

    allocate(f(sp))

    call inverseFilter(Psi, zp, sp, f, zf, sf)

    call writeSignal(f, zf, sf, 'bins/iFilter.data')

    call conv(x, f, zx, zf, sp + sr - 1, sf, deconvRef, zd)

    call writeSignal(deconvRef, zd, sp + sr - 2 + sf, 'bins/deconv.data')

    call conv(psi, f, zp, zf, sp, sf, scf, zc)

    call writeSignal(scf, zc, sp + sf - 1, 'bins/scf.data')

    deallocate(Ref)
    deallocate(Psi)
    deallocate(x)
    deallocate(f)
    deallocate(deconvRef)
    deallocate(scf)

end program main