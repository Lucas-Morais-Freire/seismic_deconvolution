program main
    use seisDeconv
    implicit none
    integer, parameter :: nss = 51, nsr = 1000 ! number of samples of source and reflectivity
    integer :: zs  ! position of t = 0 in source
    integer :: nsx ! number of samples of trace
    integer :: zx  ! position of t = 0 in trace
    integer :: nsd ! number of samples of deconvolved trace
    integer :: zd  ! position of t = 0 in deconvolved trace
    integer :: nsp ! number of samples of approximate unit pulse
    integer :: zp  ! position of t = 0 in approximate unit pulse
    integer :: nsf ! number of samples of inverse filter
    integer :: zf ! position of t = 0 in inverse filter
    real(kind=8), dimension(:), allocatable :: e
    real(kind=8), dimension(:), allocatable :: s
    real(kind=8), dimension(:), allocatable :: x
    real(kind=8), dimension(:), allocatable :: f
    real(kind=8), dimension(:), allocatable :: dec_e
    real(kind=8), dimension(:), allocatable :: p

    call initRandom()  ! initialize RNG

    allocate(e(nsr))
    
    call genReflect(nsr, e) ! generate random reflectivity e

    call addNoise(e, nsr, 0.d0, 0.5d0) ! add noise to reflectivity
    
    call writeSignal(e, 1, nsr, 'bins/reflect.data') ! write e
    
    allocate(s(nss))
    
    call genPulse(s, zs, 1.d0, 0.03d0, 1.d0/20.d0, nss) ! generate source signature s (ricker)
    
    call writeSignal(s, zs, nss, 'bins/source.data') ! write s

    call conv(s, e, zs, 1, nss, nsr, x, zx, nsx)  ! generate trace x by convolving pulse with reflectivity
    
    call writeSignal(x, zx, nsx, 'bins/signal.data')  ! write x
    
    nsf = 2*nss - 1
    zf  = nsf/2 + 1

    allocate(f(nsf))

    call inverseFilter(s, zs, nss, f, zf, nsf)      ! compute inverse filter f

    call writeSignal(f, zf, nsf, 'bins/inv_filter.data') ! write f

    call conv(x, f, zx, zf, nsx, nsf, dec_e, zd, nsd) ! convolve inverse filter with trace to get reflectivity dec_e

    call writeSignal(dec_e, zd, nsd, 'bins/deconv.data') ! write dec_e

    call conv(s, f, zs, zf, nss, nsf, p, zp, nsp) ! convolve inverse filter with signature to get an unit pulse p

    call writeSignal(p, zp, nsp, 'bins/pulse.data') ! write pulse p

    deallocate(e)
    deallocate(s)
    deallocate(x)
    deallocate(f)
    deallocate(dec_e)
    deallocate(p)

end program main