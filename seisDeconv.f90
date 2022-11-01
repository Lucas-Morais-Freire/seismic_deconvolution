module seisDeconv
    implicit none
    !integer(kind = 1), private :: sp = 4, dp = 8

    contains

    function genReflect(ns) result(Ref)
        integer, intent(in) :: ns
        integer, dimension(ns) :: Ref
        integer :: i, seed

        seed = time()
        call srand(seed)

        do i = 1, ns
            if (mod(irand(), 100) >= 95) then
                Ref(i) = mod(irand(), 100) - 50
            else
                Ref(i) = 0
            end if
        end do
    end function

end module seisDeconv