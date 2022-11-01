module seisDeconv
    implicit none
    !integer(kind = 1), private :: sp = 4, dp = 8

    contains

    function genReflect(ns) result(Ref)
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

    end function

end module seisDeconv