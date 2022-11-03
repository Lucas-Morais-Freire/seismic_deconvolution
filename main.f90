program main
    use seisDeconv
    implicit none
    integer, parameter :: ps = 500, rs = 1000
    integer :: si
    real(kind=8), dimension(:), allocatable :: Ref
    real(kind=8), dimension(:), allocatable :: Psi

    allocate(Ref(rs))
    
    Ref = genReflect(rs)

    call writeSignal(Ref, 0, rs, 'reflect.data')

    deallocate(Ref)

    allocate(Psi(ps))

    call genPulse(Psi, si, 1.d0, 0.005d0, 1.d0/200.d0, ps)

    call writeSignal(Psi, si, ps, 'pulse.data')

    deallocate(Psi)

end program main