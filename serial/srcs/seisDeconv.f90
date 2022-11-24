module seisDeconv
    implicit none
    real(kind = 8) :: pi = 4.d0*atan(1.d0)
    
    contains
        subroutine initRandom()
            integer :: n
            integer, allocatable :: seed(:)

            call random_seed(size=n)
            allocate(seed(n))
            seed = time()
            call random_seed(put=seed)
            deallocate(seed)
        end subroutine

        subroutine writeSignal(s, zi, ns, file)
            integer :: zi, ns, i
            real(kind=8), dimension(ns) :: s
            character(*) :: file

            open (1, file = file, status='old')
            write(1,*) zi
            do i = 1, ns
                write(1,'(F25.15)') s(i)
            end do
            close(1)
        end subroutine

        subroutine printMat(A, n, m)
            integer, intent(in) :: n, m
            real(kind=8), dimension(n,m), intent(in) :: A
            integer :: i, j
            
            do i = 1, n
                do j = 1, m
                    write(*,'(F13.6)', advance="no") A(i, j)
                end do
                write(*,*)
            end do
            write(*,*)
        end subroutine printMat

        function randGauss(sDev, mean) result(y)
            real(kind=8), intent(in) :: sDev, mean
            real(kind=8) :: y, u1, u2

            call random_number(u1)
            u1 = 1.d0 - u1
            call random_number(u2)
            u2 = 1.d0 - u2

            y = sDev*sqrt(-2*log(u1))*cos(2*pi*u2) + mean

        end function

        function genReflect(ns, sDev, mean) result(Ref)
            integer, intent(in) :: ns
            real(kind=8), intent(in) :: sDev, mean
            integer :: i
            real(kind=8), dimension(ns) :: Ref
            !real :: f

            do i = 1, ns
                Ref(i) = randGauss(sDev, mean)
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


        subroutine conv(x, h, zx, zh, sx, sh, y, zy)
            integer :: zx, zh, sx, sh, sy, zy, i, j
            real(kind = 8), dimension(sx) :: x
            real(kind = 8), dimension(sh) :: h
            real(kind = 8), dimension(:), allocatable :: y
            real(kind = 8) :: pSum

            sy = sx + sh - 1
            
            allocate(y(sy))

            do i = 1, sy
                pSum = 0
                do j = max(1, -sh + i + 1), min(sx, i)
                    pSum = pSum + x(j)*h(i - j + 1)
                end do
                y(i) = pSum
            end do

            zy = zx + zh - 1
        end subroutine

        subroutine solveLSE(A, b, n, x)
            integer, intent(in) :: n
            real(kind=8), dimension(n, n), intent(in) :: A
            real(kind=8), dimension(n), intent(in) :: b
            real(kind=8), dimension(n) :: x
            real(kind=8), dimension(n, n + 1) :: C
            real(kind=8), dimension(n + 1) :: aux
            integer :: i, j

            C(:, 1:n) = A
            C(:, n+1) = b

            j = 1
            do while (j <= n)
                i = j
                do while (C(i, j) == 0.d0)
                    if (i > n) then
                        print*, 'matrix not invertible'
                        exit
                    end if
                    i = i + 1
                end do

                aux = C(j,:)
                C(j,:) = C(i,:)
                C(i,:) = aux

                C(j,:) = C(j,:)/C(j, j)

                do i = j + 1, n
                    C(i,:) = -C(i, j)*C(j,:) + C(i,:)
                end do
            j = j + 1
            end do

            j = n
            do while (j > 1)
                i = j - 1
                do while (i >= 1)
                    C(i,:) = -C(i, j)*C(j,:) + C(i,:)
                    i = i - 1
                end do
                j = j - 1
            end do

            x = C(:, n + 1)
        end subroutine

        subroutine inverseFilter(s, zs, ss, f, zf, sf)
            integer :: zs, ss, zf, sf, i, j
            real(kind=8), dimension(ss) :: s
            real(kind=8), dimension(ss) :: f
            real(kind=8), dimension(2*ss - 1, ss) :: A
            real(kind=8), dimension(ss, ss) :: B
            real(kind=8), dimension(ss) :: v
            real(kind=8), dimension(2*ss - 1) :: d

            do i = 1, 2*ss - 1
                do j = 1, ss
                    if (i < j .or. j <= i - ss) then
                        A(i, j) = 0
                    else
                        A(i, j) = s(i - j + 1)
                    end if
                end do
            end do

            d = 0.d0

            d(2*zs - 1) = 1.d0

            v = matmul(transpose(A), d)

            B = matmul(transpose(A), A)
            
            call solveLSE(B, v, ss, f)
                
            sf = ss
            zf = zs
        end subroutine
end module seisDeconv