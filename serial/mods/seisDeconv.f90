module seisDeconv
    implicit none
    real(kind = 8) :: pi = 4.d0*atan(1.d0)
    
    contains
        ! initialize random seed
        subroutine initRandom()
            integer :: n
            integer, allocatable :: seed(:)

            call random_seed(size=n)
            allocate(seed(n))
            seed = time()
            call random_seed(put=seed)
            deallocate(seed)
        end subroutine

        ! writes a signal to a file
        subroutine writeSignal(s, zi, ns, file)
            integer, intent(in) :: zi, ns
            real(kind=8), dimension(ns), intent(in) :: s
            character(*), intent(in) :: file
            integer :: i

            open (1, file = file, status='replace', action='write')
            write(1,*) zi
            do i = 1, ns
                write(1,'(F25.15)') s(i)
            end do
            close(1)
        end subroutine
        
        ! prints a matrix
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

        ! returns a random number from a gaussian distribuition
        subroutine randGauss(sDev, mean, y)
            real(kind=8), intent(in) :: sDev, mean

            real(kind=8), intent(out) :: y
            
            real(kind=8) :: u1, u2

            call random_number(u1)
            u1 = 1.d0 - u1
            call random_number(u2)
            u2 = 1.d0 - u2

            y = sDev*sqrt(-2*log(u1))*cos(2*pi*u2) + mean

        end subroutine
        
        ! generates a random reflectivity
        subroutine genReflect(ns, sDev, mean, Ref)
            integer, intent(in) :: ns              ! number of samples
            real(kind=8), intent(in) :: sDev, mean ! standard dev and mean

            real(kind=8), dimension(ns), intent(out) :: Ref     ! reflectivity

            integer :: i                           ! iterator

            do i = 1, ns
                call randGauss(sDev, mean, Ref(i)) ! generate noise
            end do

            i = mod(irand(), 20) ! generate random integer from 0 to 19
            do while (i <= ns)
                ! for this index, the reflectivity will be a number from -50 to 49
                Ref(i) = real(mod(irand(), 100) - 50, kind=8)
                ! the next index will be the current one plus a random integer from 50 to 149
                i = i + mod(irand(), 100) + 50
            end do

        end subroutine genReflect

        ! generates a ricker pulse
        subroutine genPulse(Psi, zi, amp, a, f, ns)
            integer :: ns, i, zi               ! number of samples, iterator and position of the zero
            real(kind=8) :: a, amp, f          ! parameters of ricker pulse
            real(kind=8), dimension(ns) :: Psi ! output
            zi = ns/2 + 1 ! the t = zero will be located at the middle of the array
            
            ! fill the array
            do i = 1, ns
                Psi(i) = amp*(1 - 2*(pi*a*(i - ns/2 - 1))**2)*exp(-(pi*f*(i - ns/2 - 1))**2)
            end do            
        end subroutine

        ! convolve two signals
        subroutine conv(x, h, zx, zh, nsx, nsh, y, zy, nsy)
            integer, intent(in) :: zx   ! position of t = 0 in signal x
            integer, intent(in) :: zh   ! position of t = 0 in signal h
            integer, intent(in) :: nsx  ! number of samples of x
            integer, intent(in) :: nsh  ! number of samples of h
            real(kind = 8), dimension(nsx), intent(in) :: x
            real(kind = 8), dimension(nsh), intent(in) :: h
            
            integer, intent(out) :: zy   ! position of t = 0 for output signal
            integer, intent(out) :: nsy  ! number of samples of y
            real(kind = 8), dimension(:), allocatable, intent(out) :: y ! output of the convolution

            integer :: i, j        ! iterators
            real(kind = 8) :: pSum ! holds partial sum for convolution
            
            allocate(y(nsx + nsh - 1))

            nsy = nsx + nsh - 1
            do i = 1, nsy
                pSum = 0
                do j = max(1, -nsh + i + 1), min(nsx, i)
                    pSum = pSum + x(j)*h(i - j + 1)
                end do
                y(i) = pSum
            end do

            zy = zx + zh - 1
        end subroutine

        ! solves a linear system of equations A*x = b
        subroutine solveLSE(A, b, n, x)
            integer, intent(in) :: n                       ! order of the system
            real(kind=8), dimension(n, n), intent(in) :: A ! matrix A
            real(kind=8), dimension(n), intent(in) :: b    ! vector b
            real(kind=8), dimension(n) :: x                ! vector x
            real(kind=8), dimension(n, n + 1) :: C         ! holds the augmented matrix [A:b]
            real(kind=8), dimension(n + 1) :: aux          ! holds a line from the augmented matrix C for processing
            integer :: i, j                                ! iterators

            C(:, 1:n) = A ! fill left side with A  (C = [A:--])
            C(:, n+1) = b ! fill right side with b (C = [A:b])
            
            ! solving the LSE via gaussian elimination
            j = 1
            do while (j <= n)
                i = j
                do while (C(i, j) == 0.d0) ! skips this row if it has a zero in this colmn
                    if (i > n) then
                        print*, 'matrix A is not invertible'
                        exit
                    end if
                    i = i + 1
                end do
                
                ! this C_ij element will be the pivot of this column, therefore:
                aux = C(j,:)    ! put the ith row below the last row that received a pivot (at the jth row)
                C(j,:) = C(i,:)
                C(i,:) = aux

                C(j,:) = C(j,:)/C(j, j) ! divide row by pivot

                do i = j + 1, n
                    C(i,:) = -C(i, j)*C(j,:) + C(i,:) ! nullify all positions below by doing row operations
                end do
            j = j + 1
            end do
            
            ! now zero all elements above the diagonal using row operations
            j = n
            do while (j > 1)
                i = j - 1
                do while (i >= 1)
                    C(i,:) = -C(i, j)*C(j,:) + C(i,:)
                    i = i - 1
                end do
                j = j - 1
            end do
            
            ! x will be the last column of the augmented matrix
            x = C(:, n + 1)
        end subroutine

        ! computes the inverse filter given source signature
        subroutine inverseFilter(s, zs, nss, f, zf, nsf)
            real(kind=8), dimension(nss), intent(in) :: s ! input source signature
            integer, intent(in) :: zs                     ! position of t = 0 in source signature
            integer, intent(in) :: nss                    ! number of samples of source signature
            
            real(kind=8), dimension(nss), intent(out) :: f ! output filter
            integer, intent(out) :: zf                     ! position of t = 0 in output filter
            integer, intent(out) :: nsf                    ! number of samples in output filter
            
            real(kind=8), dimension(2*nss - 1, nss) :: A ! convolution matrix
            real(kind=8), dimension(nss, nss) :: B       ! (A^T)*A
            real(kind=8), dimension(nss) :: v            ! (A^T)*d
            real(kind=8), dimension(2*nss - 1) :: d      ! kronecker delta at 2*zs - 1
            integer :: i, j                              ! iterators
            
            ! fill convolution matrix
            do i = 1, 2*nss - 1
                do j = 1, nss
                    if (i < j .or. j <= i - nss) then
                        A(i, j) = 0
                    else
                        A(i, j) = s(i - j + 1)
                    end if
                end do
            end do
            
            ! construct kronecker delta
            d = 0.d0
            d(2*zs - 1) = 1.d0 ! the position of the spike will be at 2*zs - 1 because both s and f have the zero at zs.
            
            ! given e : reflectivity, s : source signature, x : observed signal, f : inverse filter
            ! conv(s, e) = x => conv(f, s, e) = conv(f, x) => e = conv(f, x)
            ! conv(f, s) = d => A*f = d => (A^T)*A*f = (A^T)*d => B*f = v

            v = matmul(transpose(A), d) ! v = (A^T)*d

            B = matmul(transpose(A), A) ! B = (A^T)*A
            
            call solveLSE(B, v, nss, f)  ! solve for f.
                
            nsf = nss
            zf = zs
        end subroutine
end module seisDeconv