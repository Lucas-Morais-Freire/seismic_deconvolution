program teste
    use seisDeconv
    implicit none

    real(kind=8), dimension(3,3) :: A
    real(kind=8), dimension(3) :: b = (/1, 4, 0/)
    real(kind=8), dimension(3) :: x

    A = reshape((/0, -1, 3, 2, 3, 9, 3, -2, -1/), shape(A))

    call solveLSE(A, b, 3, x)

    print*, x

end program