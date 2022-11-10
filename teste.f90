program teste
    use seisDeconv
    implicit none

    real(kind=8), allocatable :: samples(:)
    integer zs, ss, i
    real(kind=8) :: r

    call initRandom()

    allocate(samples(401))

    samples = 0
    zs = 201
    ss = 401

    do i = 0, 100000
        r = randGauss(50.d0, 201.d0)
        if (r < 1.d0) then
            r = 1.d0
        else if (r > 401) then
            r = 401.d0
        end if
        samples(floor(r)) = samples(floor(r)) + 1
    end do

    call writeSignal(samples, 0, ss, 'bins/signal.data')

end program