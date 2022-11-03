program main
    use seisDeconv
    implicit none
    integer, parameter :: ps = 501, rs = 1000
    integer :: pz
    real(kind=8), dimension(:), allocatable :: Ref
    real(kind=8), dimension(:), allocatable :: Psi

    allocate(Ref(rs))
    
    Ref = genReflect(rs)

    call writeSignal(Ref, 1, rs, 'reflect.data')

    deallocate(Ref)

    allocate(Psi(ps))

    call genPulse(Psi, pz, 1.d0, 0.005d0, 1.d0/200.d0, ps)

    call writeSignal(Psi, pz, ps, 'pulse.data')

    deallocate(Psi)

end program main