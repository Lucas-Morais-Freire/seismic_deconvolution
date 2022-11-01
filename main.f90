program main
    use seisDeconv
    implicit none
    integer :: ns = 1000, i
    integer, dimension(:), allocatable :: Ref
    real(kind=8), dimension(:), allocatable :: Psi

    allocate(Ref(ns))
    
    Ref = genReflect(ns)

    open(1, file = 'reflect.data', status = 'old')
    do i = 1, ns
        write(1,'(I3)') Ref(i)
    end do
    close(1)

    deallocate(Ref)

    allocate(Psi(ns))

    Psi = genPulse(1.d0, 0.005d0, 1.d0/200.d0, ns)

    open(1, file = 'pulse.data', status = 'old')
    do i = 1, ns
        write(1,'(F13.6)') Psi(i)
    end do
    close(1)

    deallocate(Psi)

end program main