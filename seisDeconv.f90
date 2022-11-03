module seisDeconv
    implicit none
    real(kind = 8) :: pi = 4.d0*atan(1.d0)
    !integer(kind = 1), private :: sp = 4, dp = 8

    contains

        function genReflect(ns) result(Ref)
            implicit none
            integer, intent(in) :: ns
            integer :: i, seed
            real(kind=8), dimension(ns) :: Ref
            !real :: f

            seed = time()
            call srand(seed)

            do i = 1, ns
                Ref(i) = 0.d0
            end do
            
            i = mod(irand(), 20)
            do while (i <= ns)
                Ref(i) = real(mod(irand(), 100) - 50, kind=8)
                i = i + mod(irand(), 100) - 50 + 100
            end do
        end function genReflect

        subroutine genPulse(Psi, si, amp, a, f, ns)
            integer, intent(in) :: ns !number of samples
            integer :: i, si
            real(kind=8), intent(in) :: a, amp, f
            real(kind=8), dimension(-ns/2 + 1:ns/2) :: Psi
            si = -ns/2 + 1
            
            do i = si, si + ns - 1
                Psi(i) = amp*(1 - 2*(pi*a*i)**2)*exp(-(pi*f*i)**2)
            end do            
        end subroutine

        subroutine writeSignal(s, si, ns, file)
            integer :: si, ns, i
            real(kind=8), dimension(ns) :: s
            character(*) :: file

            open (1, file = file, status='old')
            write(1,*) si
            do i = 1, ns
                write(1,'(F13.6)') s(i)
            end do
            close(1)
        end subroutine

end module seisDeconv