program main
    use seisDeconv
    implicit none
    integer :: ns = 1000, i
    integer, dimension(:), allocatable :: Ref

    allocate(Ref(ns))

    Ref = genReflect(ns)

    open(1, file = 'reflect.data', status = 'old')
    do i = 1, ns
        write(1,'(I3)') Ref(i)
    end do
    close(1)

    deallocate(Ref)

end program main