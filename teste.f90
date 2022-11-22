program teste
    use seisDeconv
    use omp_lib
    implicit none

    integer :: nx, nh = 1000000, zy
    real(kind=8), allocatable :: x(:), h(:), y(:)
    real(kind=8) :: start, finish

    nx = 51

    call omp_set_num_threads(4)

    call initRandom()
    
    allocate(x(nx))
    allocate(h(nh))
    
    x = genReflect(nx, 2.5d0, 0.d0)
    h = genReflect(nh, 2.5d0, 0.d0)

    start = omp_get_wtime()
    call conv(x, h, 1, 1, nx, nh, y, zy)
    finish = omp_get_wtime()

    deallocate(y)

    deallocate(x)
    deallocate(h)

    write(*,'(F25.13)') finish - start
end program