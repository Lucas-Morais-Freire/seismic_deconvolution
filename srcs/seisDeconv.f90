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

        subroutine inverse(a,c,n)
            !============================================================
            ! Inverse matrix
            ! Method: Based on Doolittle LU factorization for Ax=b
            ! Alex G. December 2009
            !-----------------------------------------------------------
            ! input ...
            ! a(n,n) - array of coefficients for matrix A
            ! n      - dimension
            ! output ...
            ! c(n,n) - inverse matrix of A
            ! comments ...
            ! the original matrix a(n,n) will be destroyed 
            ! during the calculation
            !===========================================================
            implicit none 
            integer :: n, i, j, k
            real(kind = 8), dimension(n,n) :: a, c, L, U
            real(kind = 8), dimension(n) :: b, d, x
            real(kind = 8) :: coeff
            
            ! step 0: initialization for matrices L and U and b
            ! Fortran 90/95 aloows such operations on matrices
            L = 0.d0
            U = 0.d0
            b = 0.d0
            
            ! step 1: forward elimination
            do k=1, n-1
               do i=k+1,n
                  coeff=a(i,k)/a(k,k)
                  L(i,k) = coeff
                  do j=k+1,n
                     a(i,j) = a(i,j)-coeff*a(k,j)
                  end do
               end do
            end do
            
            ! Step 2: prepare L and U matrices 
            ! L matrix is a matrix of the elimination coefficient
            ! + the diagonal elements are 1.0
            do i=1,n
              L(i,i) = 1.0
            end do
            ! U matrix is the upper triangular part of A
            do j=1,n
              do i=1,j
                U(i,j) = a(i,j)
              end do
            end do
            
            ! Step 3: compute columns of the inverse matrix C
            do k=1,n
              b(k)=1.0
              d(1) = b(1)
            ! Step 3a: Solve Ld=b using the forward substitution
              do i=2,n
                d(i)=b(i)
                do j=1,i-1
                  d(i) = d(i) - L(i,j)*d(j)
                end do
              end do
            ! Step 3b: Solve Ux=d using the back substitution
              x(n)=d(n)/U(n,n)
              do i = n-1,1,-1
                x(i) = d(i)
                do j=n,i+1,-1
                  x(i)=x(i)-U(i,j)*x(j)
                end do
                x(i) = x(i)/u(i,i)
              end do
            ! Step 3c: fill the solutions x(n) into column k of C
              do i=1,n
                c(i,k) = x(i)
              end do
              b(k)=0.0
            end do
            end subroutine inverse

        subroutine deconv(x, s, zx, zs, sx, ss, ref, zr, sr, f, zf, sf)
            integer :: zx, zs, sx, ss, zr, sr, zf, sf, i, j
            real(kind=8), dimension(sx) :: x
            real(kind=8), dimension(ss) :: s
            real(kind=8), dimension(:), allocatable :: ref
            real(kind=8), dimension(:), allocatable :: f
            real(kind=8), dimension(2*ss - 1, ss) :: A
            real(kind=8), dimension(ss, ss) :: B
            real(kind=8), dimension(ss, ss) :: C
            real(kind=8), dimension(ss) :: v
            real(kind=8) :: temp

            do i = 1, 2*ss - 1
                do j = 1, ss
                    if (i < j .or. j <= i - ss) then
                        temp = 0
                    else
                        temp = s(i - j + 1)
                    end if
                    A(i, j) = temp
                end do
            end do

            v(1) = s(1)
            do i = 2, ss
                v(i) = 0.d0
            end do

            B = matmul(transpose(A), A)

            call inverse(B, C, ss)

            allocate(f(ss))
            f = matmul(C, v)

            sf = ss
            sr = sx + sf - 1
            zf = 1
            
            call writeSignal(f, 1, sf, 'bins/iFilter.data')
            call writeSignal(x, zx, sx, 'bins/signal.data')
            
            call conv(x, f, zx, zf, sx, sf, ref, zr)

            call writeSignal(ref, zr, sx + sf - 1, 'bins/deconv.data')
        end subroutine

end module seisDeconv