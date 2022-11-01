module seisDeconv
    implicit none
    real(kind = 8) :: pi = 4.d0*atan(1.d0)
    !integer(kind = 1), private :: sp = 4, dp = 8

    contains

        function genReflect(ns) result(Ref)
            implicit none
            integer, intent(in) :: ns
            integer :: i, seed
            integer, dimension(ns) :: Ref
            !real :: f

            seed = time()
            call srand(seed)

            do i = 1, ns
                Ref(i) = 0
            end do
            
            i = mod(irand(), 20)
            do while (i <= ns)
                Ref(i) = mod(irand(), 100) - 50
                i = i + mod(irand(), 100) - 50 + 100
            end do
        end function genReflect

        function genPulse(amp, a, f, ns) result(Psi)
            integer, intent(in) :: ns
            integer :: i
            real(kind=8), intent(in) :: a, amp, f
            real(kind=8), dimension(-ns/2:ns/2) :: Psi
            
            do i = -ns/2, ns/2
                Psi(i) = amp*(1 - 2*(pi*a*i)**2)*exp(-(pi*f*i)**2)
            end do            
        end function

        function conv(x, h, nx, nh, sx, sh) result(y)
            integer, intent(in) :: nx, nh, sx, sh
            real(kind=8), dimension(sx:sx + nx - 1), intent(in) :: x
            real(kind=8), dimension(sh:sh + nh - 1), intent(in) :: h
            real(kind=8), dimension(0:nx + nh - 2) :: y

            integer :: i, j            

        end function

end module seisDeconv