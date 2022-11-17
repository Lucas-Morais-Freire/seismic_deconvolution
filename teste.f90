program teste
    use seisDeconv
    implicit none

    real(kind=8), allocatable :: samples(:)
    real(kind=8), allocatable :: f(:)
    real(kind=8), allocatable :: scf(:)
    integer :: zs, ns = 51 , zf, nf = 51, zc

    call initRandom()

    allocate(samples(ns))

    call genPulse(samples, zs, 1.d0, 0.03d0, 1.d0/20.d0, ns)

    call writeSignal(samples, zs, ns, 'bins/pulse.data')

    allocate(f(nf))

    call deconv(samples, zs, ns, f, zf, nf)

    call writeSignal(f, zf, nf, 'bins/iFilter.data')

    call conv(samples, f, zs, zf, ns, nf, scf, zc)

    call writeSignal(scf, zc, ns + nf - 1, 'bins/scf.data')

    deallocate(samples)
    deallocate(f)

end program