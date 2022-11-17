program teste
    use seisDeconv
    use omp_lib
    implicit none

    integer :: n = 100000, zy
    real(kind=8), allocatable :: x(:), h(:), y(:)
    real(kind=8) :: start, finish

    call omp_set_num_threads(2)

    call initRandom()

    allocate(x(n))
    allocate(h(n))

    x = genReflect(n, 2.5d0, 0.d0)
    h = genReflect(n, 2.5d0, 0.d0)

    start = omp_get_wtime()
    call conv(x, h, 1, 1, n, n, y, zy)
    finish = omp_get_wtime()

    deallocate(x)
    deallocate(h)
    deallocate(y)

    print*, finish - start
end program