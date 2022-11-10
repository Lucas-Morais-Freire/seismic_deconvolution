program teste
    use seisDeconv
    implicit none

    real(kind=8), dimension(2) :: s = (/1.d0, -0.5d0/)
    real(kind=8), dimension(2) :: f
    integer :: zf, sf

    call deconv(s, 1, 2, f, zf, sf)

    call writeSignal(f, zf, sf, 'bins/iFilter.data')

end program