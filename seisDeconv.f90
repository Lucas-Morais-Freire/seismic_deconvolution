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

        subroutine genPulse(Psi, zi, amp, a, f, ns)
            integer :: ns, i, zi
            real(kind=8) :: a, amp, f
            real(kind=8), dimension(ns) :: Psi
            zi = ns/2 + 1

            do i = 1, ns
                Psi(i) = amp*(1 - 2*(pi*a*(i - ns/2 - 1))**2)*exp(-(pi*f*(i - ns/2 - 1))**2)
            end do            
        end subroutine

        subroutine writeSignal(s, zi, ns, file)
            integer :: zi, ns, i
            real(kind=8), dimension(ns) :: s
            character(*) :: file

            open (1, file = file, status='old')
            write(1,*) zi
            do i = 1, ns
                write(1,'(F13.6)') s(i)
            end do
            close(1)
        end subroutine

        ! function conv(x, h, zx, zh, sx, sh) result(y)
        !     integer, intent(in) :: zx, zh, sx, sh
        !     real(kind = 8), dimension(:), allocatable, intent(in) :: x
        !     real(kind = 8), dimension(:), allocatable, intent(in) :: h
        !     real(kind = 8), dimension(:), allocatable :: y

        !     allocate(x(1:3))
        !     allocate(h(1:3))
        !     allocate(y(1:3))

        !     deallocate(x)
        !     deallocate(h)
        !     deallocate(y)

        ! end function

end module seisDeconv