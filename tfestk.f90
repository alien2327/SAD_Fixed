module ophash
    use tfstk
    implicit none
    integer(kind=4), parameter :: nhash = 4
    integer(kind=4) iophash(nhash, 0:63)
    logical(kind=4), save :: opini = .true.
    integer(kind=8) ktfcode(0:ntfarg)
    character(len=4), save :: opcode(0:mtfnopc) = (/ &
        '    ','    ','    ','+   ','-   ', &
        '*   ','/   ','    ','^   ','>   ', &
        '>=  ','<=  ','<   ','==  ','<>  ', &
        '&&  ','||  ','~   ','=== ','<=> ', &
        '//  ','[   ',']   ','{   ','}   ', &
        ':=  ','=   ','    ','(   ',')   ', &
        ',   ',';   ','&   ',':   ','->  ', &
        ':>  ','/.  ','//. ','^=  ','^:= ', &
        '=.  ','?   ','?   ','#   ','##  ', &
        '.   ','|   ','/@  ','//@ ','@@  ', &
        '..  ','... ','    ','+=  ','-=  ', &
        '*=  ','/=  ','++  ','--  ','[[  ', &
        '@   ','::  ','/:  ','(*  ','*)  ', &
        '    ','    '/)
    !   Alternatives is temporarily set .false. due to possible reduction.
    logical(kind=4) :: constop(0:mtfnopc) = (/ &
        .false.,.false.,.false.,.false.,.false., &
        .false.,.false.,.false.,.false.,.false., &
        .false.,.false.,.false.,.false.,.false., &
        .false.,.false.,.false.,.false.,.false., &
        .false.,.false.,.false.,.true. ,.false., &
        .false.,.false.,.true., .false.,.false., &
        .false.,.false.,.true. ,.true., .true., &
        .true., .false.,.false.,.false.,.false., &
        .false.,.true. ,.false.,.false.,.false., &
        .false.,.false. ,.false.,.false.,.false., &
        .true. ,.true. ,.false.,.false.,.false., &
        .false.,.false.,.false.,.false.,.false., &
        .false.,.true., .true. ,.false.,.false., &
        .true., .false. /)

    contains
    subroutine tfopcodehash
        integer(kind=4) i, j, k
        character(len=4) oper1
        iophash = -1
        LOOP_I: do i = 0, mtfnopc
            if (opcode(i) .ne. ' ') then
                oper1 = opcode(i)
                j = ichar(oper1(1:1)) + ichar(oper1(2:2)) &
                    + ichar(oper1(3:3)) + ichar(oper1(4:4))
                j = iand(j, 63)
                do k = 1, nhash
                    if (iophash(k, j) .lt. 0) then
                        iophash(k, j) = i
                        cycle LOOP_I
                    end if
                end do
                write (*,*) 'tfopcode hash implementation error. ', opcode(i)
                stop
            end if
        end do LOOP_I
        opini = .false.
        return
    end subroutine tfopcodehash
end module ophash

module opdata
    use tfstk
    implicit none
    integer(kind=4) :: iprior(0:mtfnopc) = (/  &
        9999,  &
        10,  20,  50,  50,  40,  40,  15,  15,  100, 100, &
        100, 100, 100, 100, 160, 170, 150, 120, 120, 80, &
        6,   3000,9999,3000,250, 250, 7000,9999,8000,9000, &
        1000,220, 180, 190, 190, 200, 200, 250, 250, 900, &
        4,   3,   3,   3,   10,  175, 9,   9,   9,   172, &
        172, 130, 210, 210, 210, 210, 7,   7,   6,   5, &
        2,   240, 9999,9999, 1,   9999 /)
    !   null
    !   m    i    +    -    *    /    v    ^    >    >=
    !   <=   <    ==   <>   &&   ||   ~    ===  <=>  //
    !   [    ]    {    }    :=   =   
    !   ;    &    :    ->   :>   /.   //.  ^=   ^:=  =.
    !   ?    flg  #    ##   .    |    /@   //@  @@   ..
    !   ...  ineq +=   -=   *=   /=   ++   --   [[   @
    !   msgn /:   (*   *)   Hold z
    logical(kind=4), parameter :: T = .true., F = .false.
    logical(kind=4) :: nullfirst(0:mtfnopc) = (/ &
    .true., &
    .true., .true., .false., .false., .false., .false., .false., .false., .false., .false., &
    .false., .false., .false., .false., .false., .false., .true., .false., .false., .false., &
    .true., .true., .true., .true., .false., .false., .false., .true., .true., .true., &
    .true., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
    .true., .true., .true., .true., .false., .false., .false., .false., .false., .false., &
    .false., .false., .false., .false., .false., .false., .true., .true., .false., .false., &
    .false., .false., .false., .false., .false., .false./)
    logical(kind=4) :: lastfirst(0:mtfnopc) = (/ &
    .false., &
    .false., .false., .false., .false., .false., .false., .false., .true., .false., .false., &
    .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
    .false., .false., .false., .false., .true., .true., .false., .false., .false., .false., &
    .false., .false., .false., .true., .true., .false., .false., .false., .false., .false., &
    .false., .false., .false., .false., .false., .false., .true., .true., .true., .false., &
    .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
    .false., .false., .false., .false., .false., .false./)
end module opdata

module tfform
    use tfstk
    type (sad_descriptor), save :: iaxform, iaxpagewidth
    data iaxform%k,iaxpagewidth%k /0,0/
    type (sad_symbol), pointer, save :: symform, sympw
    contains
    subroutine tfforminit
        implicit none
        iaxform = kxsymbolz('System`$FORM', 12)
        iaxpagewidth = kxsymbolz('System`Pagewidth', 16)
        call descr_sym(iaxform, symform)
        call descr_sym(iaxpagewidth, sympw)
    end subroutine tfforminit
end module tfform

subroutine tfestk(isp0, iprior, lastfirst, irtc)
    use tfstk
    use ophash
    use eexpr
    implicit none
    type (sad_descriptor) kx, kh, tfeval1
    type (sad_dlist), pointer :: klx
    integer(kind=4), intent(in) :: isp0, iprior(0:mtfnopc)
    integer(kind=4), intent(out) :: irtc
    integer(kind=4) iop, iop1, isp1, i, itgetfpe, itfmessage
    logical(kind=4), intent(in) :: lastfirst(0:mtfnopc)
    logical(kind=4) ineq
    ineq(iop1)=iop1 .ge. mtfgreater .and. iop1 .le. mtfunequal &
        .or. iop1 .eq. mtfsame .or. iop1 .eq. mtfunsame
    irtc = 0
    do while (isp .gt. isp0)
        iop = itastk2(1, isp)
        if (iop .eq. mtfnull .or. iop .eq. mtfleftparen .or. iop .eq. mtflist) then
            return
        end if
        isp1 = isp - 1
        iop1 = itastk2(1, isp1)
        if (iop .eq. mtfrightbra) then
            if (iop1 .eq. mtfrightbra) then
                if (ktastk(isp) .eq. ktfoper + mtfnull) then
                    isp = isp - 1
                    do i = isp, isp0, -1
                        if (itastk2(1, i) .eq. mtfpart) then
                            isp1 = i
                            kx = kxmakelist(isp1 - 1, klx)
                            klx%head%k = ktfoper + mtfpart
                            iop = mtfnull
                            go to 1010
                        end if
                    end do
                end if
                go to 9010
            else if (iop1 .eq. mtfcomma .or. iop1 .eq. mtfleftbra) then
                do i = isp1, isp0, -1
                    if (itastk2(1, i) .eq. mtfleftparen .or. itastk2(1, i) .eq. mtflist) then
                        go to 9010
                    else if (itastk2(1, i) .eq. mtfpart) then
                        return
                    else if (itastk2(1, i) .eq. mtfleftbra) then
                        isp1 = i
                        if (isp .eq. isp1+1 .and. ktastk(isp) .eq. ktfoper + mtfnull) then
                            isp = isp1
                        end if
                        kh = dtastk(i)
                        call tfcomposefull(i, kh, kx, irtc)
                        if (irtc .ne. 0) then
                            return
                        end if
                        iop = mtfnull
                        go to 1010
                    end if
                end do
                go to 9010
            else if (iop1 .eq. mtfpart) then
                return
            end if
        else if (iop .eq. mtfrightbrace .and. (iop1 .eq. mtfcomma .or. iop1 .eq. mtflist)) then
            do i = isp1, isp0, -1
                if (itastk2(1, i) .eq. mtfleftparen .or. itastk2(1, i) .eq. mtfleftbra &
                    .or. itastk2(1, i) .eq. mtfpart) then
                    go to 9010
                else if (itastk2(1, i) .eq. mtflist) then
                    isp1 = i
                    kx = kxmakelist(isp1)
                    iop = mtfnull
                    go to 1010
                end if
            end do
            go to 9010
        else if (iop .eq. mtfrightparen .and. iop1 .eq. mtfleftparen) then
            ktastk(isp1) = ktastk(isp)
            itastk2(1, isp1) = mtfnull
            isp = isp1
            return
        else if (iop1 .eq. mtffun .and. ktastk(isp) .eq. ktfoper + mtfnull) then
            kx = kxpfaloc(dtastk(isp - 1))
            go to 1010
        end if
        if (iprior(iop) .lt. iprior(iop1)) then
            return
        else if (iprior(iop) .eq. iprior(iop1) .and.lastfirst(iop)) then
            return
        end if
        if (iop1 .eq. mtfcomma) then
            if (iop .ne. mtfcomma) then
                go to 9020
            end if
            return
        else if (iop1 .eq. mtfpower) then
            if (ktastk(isp) .eq. ktfoper + mtfnull) then
                return
            end if
        else if (iop1 .eq. mtfleftbra .or. iop1 .eq. mtfpart) then
             return
        end if
        if (ineq(iop1)) then
            call tfeinequal(dtastk(isp1), dtastk(isp), kx, iop1, ineq(iop))
        else
            if (iop1 .eq. mtfneg) then
                iop1 = mtftimes
            else if (iop1 .eq. mtfnot) then
                if (ktastk(isp1) .ne. ktfoper + mtfnull) then
                    go to 9030
                end if
            end if
            if (tfconstq(ktastk(isp1)) .and..not. tfheldqd(dtastk(isp1)) &
                 .and. tfconstq(ktastk(isp)) .and..not. tfheldqd(dtastk(isp)) &
                 .and. ktfimmediateq(klist(ifunbase + iop1))) then
                kx = tfeval1(dtastk(isp1), dtastk(isp), iop1, irtc)
                if (irtc .ne. 0) then
                    return
                end if
                if (itgetfpe() .gt. 0) then
                    irtc = itfmessage(9, 'General::fpe', '""')
                    return
                end if
            else
                kx = tfeexpr(dtastk(isp1), dtastk(isp), iop1)
            end if
        end if
    1010 isp = isp1
            itastk2(1, isp) = iop
            dtastk(isp) = kx
        end do
        return
    9010 irtc = itfmessage(9999, 'General::mismatch', '"'//opcode(iop)//'"')
        return
    9020 irtc = itfmessage(9999, 'General::incomplete', '""')
        return
    9030 irtc = itfmessage(9, 'General::narg', '"1 for ~"')
        return
end subroutine tfestk

subroutine tfeinequal(k1, k2, kx, iop1, nextrel)
    use tfstk
    use eeval
    implicit none
    type (sad_descriptor), intent(in) :: k1, k2
    type (sad_descriptor), intent(out) :: kx
    type (sad_dlist), pointer :: kl1, kl2
    type (sad_dlist), pointer :: klx
    integer(kind=4), intent(in) :: iop1
    integer(kind=4) i, m1, m2, irtc
    logical(kind=4), intent(in) :: nextrel
    logical(kind=4) tfconstlistqo
    if (tfinequalityq(k1)) then
        call loc_sad(ktfaddrd(k1), kl1)
        m1 = kl1%nl
        if (tfinequalityq(k2)) then
            call loc_sad(ktfaddrd(k2), kl2)
            m2 = kl2%nl
            kx = kxadaloc(-1, m1 + m2 + 1, klx)
            klx%head%k = ktfoper + mtfinequality
            do i = 1, m1
                klx%dbody(i) = dtfcopy(kl1%dbody(i))
            end do
            klx%dbody(m1 + 1)%k = ktfoper + iop1
            do i = 1, m2
                klx%dbody(i + m1 + 1) = dtfcopy(kl2%dbody(i))
            end do
        else
            kx = kxadaloc(-1, m1 + 2, klx)
            klx%head%k = ktfoper + mtfinequality
            do i = 1, m1
                klx%dbody(i) = dtfcopy(kl1%dbody(i))
            end do
            klx%dbody(m1 + 1)%k = ktfoper + iop1
            klx%dbody(m1 + 2) = dtfcopy(k2)
        end if
    else
        if (tfinequalityq(k2)) then
            call loc_sad(ktfaddrd(k2), kl2)
            m2 = kl2%nl
            kx = kxadaloc(-1, m2 + 2, klx)
            klx%head%k = ktfoper + mtfinequality
            klx%dbody(1) = dtfcopy(k1)
            klx%dbody(2)%k = ktfoper + iop1
            do i = 1, m2
                klx%dbody(i + 2) = dtfcopy(kl2%dbody(i))
            end do
        else
            if (nextrel) then
                kx = kxadaloc(-1, 3, klx)
                klx%head%k = ktfoper + mtfinequality
                klx%dbody(1) = dtfcopy(k1)
                klx%dbody(2)%k = ktfoper + iop1
                klx%dbody(3) = dtfcopy(k2)
            else
                kx = kxadaloc(-1, 2, klx)
                klx%head%k = ktfoper + iop1
                klx%dbody(1) = dtfcopy(k1)
                klx%dbody(2) = dtfcopy(k2)
            end if
        end if
    end if
    if (.not. nextrel) then
        if (tfconstlistqo(klx)) then
            kx = tfleval(klx, .true., irtc)
        end if
    end if
    return
end subroutine tfeinequal

subroutine tfinequality(isp1, kx, irtc)
    use tfstk
    use eeval
    implicit none
    type (sad_descriptor) , intent(out):: kx
    integer(kind=4), intent(in):: isp1
    integer(kind=4), intent(out):: irtc
    type (sad_descriptor) tfeval1, kxi
    integer(kind=8) ka
    integer(kind=4) i, narg, itfmessage, iop1
    logical(kind=4) ineq
    ineq(iop1) = iop1 .ge. mtfgreater .and. iop1 .le. mtfunequal &
        .or. iop1 .eq. mtfsame .or. iop1 .eq. mtfunsame
    narg = isp-isp1
    if (narg .lt. 3 .or. narg .ne. (narg / 2) * 2 + 1) then
        irtc = itfmessage(9, 'General::narg', '"3 or more"')
        return
    end if
    kx%k = ktftrue
    do i = isp1 + 1, isp, 2
        dtastk(i) = tfeevalref(dtastk(i), irtc)
        if (irtc .ne. 0) then
            return
        end if
        if (i .gt. isp1 + 1) then
            if (ktfoperq(dtastk(i-1)) .and. ineq(int(ktfaddr(ktastk(i-1))))) then
                ka = ktfaddr(ktastk(i-1))
            else
                irtc = itfmessage(9, 'General::wrongtype', '"==, <>, <, >, <=, >=, ===, <=>"')
                return
            end if
            kxi = tfeval1(dtastk(i-2), dtastk(i), int(ka), irtc)
            if (irtc .ne. 0) then
                return
            end if
            if (kxi%k .eq. 0) then
                kx = kxi
                return
            else if (ktfnonrealq(kxi)) then
                kx = tfeval1(kx, kxi, mtfand, irtc)
                if (irtc .ne. 0) then
                    return
                end if
            end if
        end if
    end do
    return
end subroutine tfinequality

function tfcompose(isp1, kh, irtc) result (kx)
    use tfstk
    implicit none
    type (sad_descriptor) kx
    integer(kind=4), intent(in) :: isp1
    integer(kind=4), intent(out) :: irtc
    type (sad_descriptor), intent(in) :: kh
    type (sad_descriptor) tfcomposefun, tfcomposeoper
    integer(kind=8) kah
    integer(kind=4) iah, isp0
    if (rlist(iaximmediate) .ne. 0.d0) then
        if (ktfoperq(kh, kah)) then
            iah = int(kah)
            kx = merge(tfcomposefun(isp1, iah, .false., irtc), &
                tfcomposeoper(isp1, iah, .true., isp0, irtc), iah .gt. mtfend)    
            if (irtc .eq. 0) then
                return
            end if
        else if (ktfstringq(kh)) then
            if (isp .eq. isp1 + 1 .or. isp .eq. isp1 + 2) then
                if (ktfrealq(dtastk(isp1 + 1)) .and. ktfrealq(dtastk(isp))) then
                    kx = kxsubstring(kh, isp1 + 1, isp)
                    irtc = 0
                    return
                end if
            end if
        end if
    end if
    kx = kxcrelistm(isp-isp1, ktastk(isp1 + 1:isp), kh)
    irtc = 0
    return
end function tfcompose

subroutine tfcomposefull(isp1, kh, kx, irtc)
    use tfstk
    use efun
    implicit none
    type (sad_descriptor), intent(out) :: kx
    integer(kind=4), intent(in) :: isp1
    integer(kind=4), intent(out) :: irtc
    type (sad_descriptor), intent(in) :: kh
    type (sad_descriptor) tfcomposefun, tfcomposeoper
    integer(kind=8) kah
    integer(kind=4) i, isp0, iah
    if (isp .ne. isp1 .and. rlist(iaximmediate) .ne. 0.d0) then
        if (ktfoperq(kh, kah)) then
            iah = int(kah)
            if (iah .gt. mtfend) then
                kx = tfcomposefun(isp1, iah, .true., irtc)
                if (irtc .eq. 0) then
                    return
                else if (irtc .gt. 0) then
                    go to 10
                end if
                if (.not. ktfimmediateq(klist(ifunbase + iah))) then
                    go to 10
                end if
            else
            kx = tfcomposeoper(isp1, iah, .true., isp0, irtc)
            if (irtc .eq. 0) then
                return
            else if (irtc .gt. 0) then
                if (iah .eq. mtfcomplex) then
                go to 1
                end if
                go to 10
            end if
        end if
    else if (ktfstringq(kh)) then
        if (isp .eq. isp1 + 1 .or. isp .eq. isp1 + 2) then
            if (ktfrealq(ktastk(isp1 + 1)) .and. ktfrealq(ktastk(isp))) then
                kx = kxsubstring(kh, isp1 + 1, isp)
                irtc = 0
                return
            end if
        end if
        go to 10
    else
        go to 10
    end if
    1 isp0 = isp
    isp = isp + 1
    dtastk(isp) = kh
    do i = isp1 + 1, isp0
        if (tfconstq(ktastk(i))) then
            if (ktastk(i) .eq. ktfoper + mtfnull) then
                isp = isp0
                go to 10
            else if (tfheldqd(dtastk(i))) then
                isp = isp0
                go to 10
            end if
            isp = isp + 1
            ktastk(isp) = ktastk(i)
        else
            isp = isp0
            go to 10
        end if
    end do
    kx = tfefunref(isp0 + 1, .true., irtc)
    isp = isp0
    return
    end if
    10 kx = kxcrelistm(isp-isp1, ktastk(isp1 + 1:isp), kh)
    irtc = 0
    return
end subroutine tfcomposefull

function tfcomposeoper(isp1, iah, comp, isp0, irtc) result(kx)
    use tfstk
    use ophash
    use eexpr
    implicit none
    type (sad_descriptor) kx, tfpart, tfecmplxl
    integer(kind=4), intent(in):: isp1, iah
    integer(kind=4), intent(out):: irtc, isp0
    type (sad_dlist), pointer :: klx
    integer(kind=8) iee0
    integer(kind=4)i, isp00, iv
    logical(kind=4), intent(in):: comp
    real(kind=8) vx
    irtc = 1
    kx = dxnullo
    if (constop(iah)) then
        return
    else if (isp1 + 2 .eq. isp) then
        select case (iah)
        case (mtfset, mtfsetdelayed, mtfmap, mtfapply, mtfmapall, mtfunset, mtfmessagename)
            return
        case (mtfplus, mtftimes)
            if (tfconstq(ktastk(isp1 + 1))) then
                if (tfconstq(ktastk(isp))) then
                    kx = tfplus(isp1, iah, irtc)
                    return
                end if
            end if
        case (mtfpower, mtfrevpower)
            if (tfconstq(ktastk(isp1 + 1))) then
                if (tfconstq(ktastk(isp))) then
                    kx = tfecmplxl(dtastk(isp1 + 1), dtastk(isp), mtfpower)
                    return
                end if
            end if
        case (mtfequal)
            if (iand(ktrmask, ktastk(isp1 + 1)) .ne. ktfnr &
                .and. iand(ktrmask, ktastk(isp)) .ne. ktfnr ) then
                kx%k = merge(ktftrue, ktffalse, rtastk(isp1 + 1) .eq. rtastk(isp))
                irtc = 0
                return
            else if (ktftype(ktastk(isp1 + 1)) .eq. ktfstring &
                .and. ktftype(ktastk(isp)) .eq. ktfstring) then
                kx%k = merge(ktftrue, ktffalse, tfsamestringq(ktastk(isp1 + 1), ktastk(isp)))
                irtc = 0
                return
            end if
        case (mtfunequal)
            if (iand(ktrmask, ktastk(isp1 + 1)) .ne. ktfnr &
                .and. iand(ktrmask, ktastk(isp)) .ne. ktfnr ) then
                kx%k = merge(ktffalse, ktftrue, rtastk(isp1 + 1) .eq. rtastk(isp))
                irtc = 0
                return
            else if (ktftype(ktastk(isp1 + 1)) .eq. ktfstring &
                .and. ktftype(ktastk(isp)) .eq. ktfstring) then
                kx%k = merge(ktffalse, ktftrue, tfsamestringq(ktastk(isp1 + 1), ktastk(isp)))
                irtc = 0
                return
            end if
        case (mtfsame)
            if (tfconstq(ktastk(isp1 + 1)) .and. tfconstq(ktastk(isp))) then
                kx%k = merge(ktftrue, ktffalse, tfsameq(ktastk(isp1 + 1), ktastk(isp)))
                irtc = 0
                return
            end if
        case (mtfunsame)
            if (tfconstq(ktastk(isp1 + 1)) .and. tfconstq(ktastk(isp))) then
                kx%k = merge(ktffalse, ktftrue, tfsameq(ktastk(isp1 + 1), ktastk(isp)))
                irtc = 0
                return
            end if
        case (mtfgreater)
            if (iand(ktrmask, ktastk(isp1 + 1)) .ne. ktfnr &
                .and. iand(ktrmask, ktastk(isp)) .ne. ktfnr ) then
                kx%k = merge(ktftrue, ktffalse, rtastk(isp1 + 1) .gt. rtastk(isp))
                irtc = 0
                return
            end if
        case (mtfgeq)
            if (iand(ktrmask, ktastk(isp1 + 1)) .ne. ktfnr &
                .and. iand(ktrmask, ktastk(isp)) .ne. ktfnr ) then
                kx%k = merge(ktftrue, ktffalse, rtastk(isp1 + 1) .ge. rtastk(isp))
                irtc = 0
                return
            end if
        case (mtfless)
            if (iand(ktrmask, ktastk(isp1 + 1)) .ne. ktfnr &
                .and. iand(ktrmask, ktastk(isp)) .ne. ktfnr ) then
                kx%k = merge(ktftrue, ktffalse, rtastk(isp1 + 1) .lt. rtastk(isp))
                irtc = 0
                return
            end if
        case (mtfleq)
            if (iand(ktrmask, ktastk(isp1 + 1)) .ne. ktfnr &
                .and. iand(ktrmask, ktastk(isp)) .ne. ktfnr ) then
                kx%k = merge(ktftrue, ktffalse, rtastk(isp1 + 1) .le. rtastk(isp))
                irtc = 0
                return
            end if
        case (mtfpart)
            if (ktastk(isp1 + 1) .eq. klist(iaxslotnull)) then
                if (ktfrealq(ktastk(isp))) then
                    iv = int(rtastk(isp))
                    if (iv .ge. -2 .and. iv .le. nslots) then
                        kx = dlist(iaxslotpart + iv + 2)
                        irtc = 0
                        return
                    end if
                end if
                return
            end if
        case (mtfatt)
            call tfclassmember(dtastk(isp1 + 1), dtastk(isp), kx, .false., irtc)
            return
        case (mtffun)
            if (ktastk(isp) .eq. ktfoper + mtfnull) then
                kx = kxpfaloc(dtastk(isp-1))
                irtc = 0
                end if
            return
        end select
    end if
    select case (iah)
    case (mtfpart)
        if (tfconstq(ktastk(isp1 + 1))) then
            if (tflistq(ktastk(isp1 + 1))) then
                do i = isp1 + 2, isp
                    if (ktfnonrealq(ktastk(i))) then
                        if (ktastk(i) .ne. ktfoper + mtfnull) then
                            return
                        end if
                    end if
                end do
                isp0 = isp
                iee0 = ierrorexp
                ierrorexp = 1
                kx = tfpart(isp1 + 1, .false., irtc)
                ierrorexp = iee0
                isp = isp0
                if (irtc .ne. 0) then
                    irtc = 1
                    return
                end if
            end if
        end if
        return
    case (mtfplus)
        vx = 0.d0
        isp0 = isp
        isp = isp + 1
        do i = isp1 + 1, isp0
            if (ktfnonrealq(ktastk(i))) then
                isp = isp + 1
                ktastk(isp) = ktastk(i)
            else
                vx = vx + rtastk(i)
            end if
        end do
        if (isp .eq. isp0 + 1) then
            kx = dfromr(vx)
            irtc = 0
            return
        else if (vx .eq. 0.d0) then
            if (isp-isp0-1 .ne. isp0-isp1) then
                isp = isp0
                return
            end if
            isp00 = isp0
            isp0 = isp0 + 1
            go to 110
        else
            rtastk(isp0 + 1) = vx
            isp00 = isp0
            go to 110
        end if
    case (mtftimes)
        vx = 1.d0
        isp0 = isp
        isp = isp + 1
        do i = isp1 + 1, isp0
            if (ktfnonrealq(ktastk(i))) then
                isp = isp + 1
                ktastk(isp) = ktastk(i)
            else
                vx = vx * rtastk(i)
            end if
        end do
        if (isp .eq. isp0 + 1) then
            kx = dfromr(vx)
            isp = isp0
            irtc = 0
            return
        else if (vx .eq. 1.d0) then
            if (isp-isp0-1 .ne. isp0-isp1) then
                isp = isp0
            return
            end if
            isp00 = isp0
            isp0 = isp0 + 1
            go to 110
        else
            rtastk(isp0 + 1) = vx
            isp00 = isp0
            go to 110
        end if
    case (mtfand)
        isp0 = isp
        do i = isp1 + 1, isp
            if (ktfnonrealq(ktastk(i))) then
                isp = isp + 1
                ktastk(isp) = ktastk(i)
            else if (rtastk(i) .eq. 0.d0) then
                kx%k = 0
                isp = isp0
                irtc = 0
                return
            end if
        end do
        if (isp .eq. isp0) then
            kx%k = ktftrue
            irtc = 0
            return
        else
            isp00 = isp0
            go to 110
        end if
    case (mtfor)
        isp0 = isp
        do i = isp1 + 1, isp
            if (ktfnonrealq(ktastk(i))) then
                isp = isp + 1
                ktastk(isp) = ktastk(i)
            else if (rtastk(i) .ne. 0.d0) then
                kx%k = ktftrue
                isp = isp0
                irtc = 0
                return
            end if
        end do
        if (isp .eq. isp0) then
            kx%k = 0
            irtc = 0
            return
        else
            isp00 = isp0
            go to 110
        end if
    case (mtfconcat)
        do i = isp1 + 1, isp
            if (ktfnonstringq(ktastk(i))) then
            go to 1
            end if
        end do
        call tfstringjoin(isp1, kx, irtc)
        return
    end select
    1 irtc = -1
    return
    110 irtc = 0
    if (isp .eq. isp0 + 1) then
        kx = dtastk(isp)
        if (ktfrealq(kx)) then
            isp = isp00
            return
        else if (ktflistq(kx, klx)) then
            if (klx%head%k .eq. ktfoper + mtfcomplex) then
                isp = isp00
                return
            end if
        end if
    end if
    if (comp) then
        kx = kxmakelist(isp0, klx)
        klx%head%k = ktfoper + iah
        isp = isp00
    else
        kx%k = ktfref
    end if
    return
end function tfcomposeoper

recursive function tfcomposefun(isp1, iah, full, irtc) result(kx)
    use tfstk
    use efun
    implicit none
    type (sad_descriptor) kx, tfmodule
    type (sad_descriptor) dh
    integer(kind=8) ka, kti, kai, i
    integer(kind=4), intent(in) :: isp1, iah
    integer(kind=4), intent(out) :: irtc
    integer(kind=4)narg, id, isp2, j, itfpmatc
    real(kind=8) rimmediate0, v
    logical(kind=4), intent(in) :: full
    logical(kind=4) re
    irtc = 1
    kx = dxnullo
    id = iget_fun_id(int8(iah))
    select case (id)
    case (nfunif)
        narg = isp-isp1
        if (narg .ge. 2 .and. narg .le. 4) then
            if (ktastk(isp1 + 1) .eq. 0) then
                kx = merge(dtastk(isp1 + 3), dxnullo, narg .ge. 3)
            else if (ktfrealq(ktastk(isp1 + 1))) then
                kx = dtastk(isp1 + 2)
            else
                kx%k = ktfoper + mtfnull
                return
            end if
            irtc = 0
            return
        end if
        return
    case (nfunwhich)
        if (mod(isp-isp1, 2) .eq. 0) then
            isp2 = isp1 + 1
            do j = isp1 + 1, isp-1, 2
                if (ktfrealq(ktastk(j), v)) then
                    if (v .ne. 0.d0) then
                        kx = dtastk(j + 1)
                        irtc = 0
                        return
                    else
                        isp2 = j + 2
                    end if
                else
                    isp2 = j
                    exit
                end if
            end do
            if (isp2 .gt. isp1 + 1) then
                dh%k = ktfoper + int8(iah)
                kx = kxcrelistm(isp-isp2 + 1, ktastk(isp2:isp), dh)
                irtc = 0
            end if
        end if
        return
    case (nfunswitch)
        if (mod(isp-isp1, 2) .eq. 1) then
            if (tfconstq(dtastk(isp1 + 1))) then
                isp2 = isp1 + 1
                do j = isp1 + 2, isp-1, 2
                    if (tfconstq(dtastk(j))) then
                        if (itfpmatc(dtastk(isp1 + 1), dtastk(j)) .ge. 0) then
                            kx = dtastk(j + 1)
                            irtc = 0
                            return
                        else
                            isp2 = j + 1
                        end if
                    else
                        isp2 = j-1
                        exit
                    end if
                end do
                if (isp2 .gt. isp1 + 1) then
                    dh%k = ktfoper + int8(iah)
                    dtastk(isp2) = dtastk(isp1 + 1)
                    kx = kxcrelistm(isp-isp2 + 1, ktastk(isp2:isp), dh)
                    irtc = 0
                end if
            end if
        end if
        return
    case (nfunmodule)
        if (full) then
        11 if (isp .eq. isp1 + 2) then
                kx = tfmodule(isp1, .true., .false., irtc)
                if (irtc .ne. 0) then
                    if (irtc .gt. 0 .and. ierrorprint .ne. 0) then
                        call tfreseterror
                    end if
                    irtc = 1
                end if
                isp = isp1 + 2
            else if (isp .gt. isp1 + 2) then
                dh = dtastk(isp1 + 1)
                dtastk(isp1 + 1) = dtastk(isp1)
                kx = tfcomposefun(isp1 + 1, iah, .true., irtc)
                dtastk(isp1 + 1) = dh
                if (irtc .ne. 0) then
                    if (irtc .gt. 0 .and. ierrorprint .ne. 0) then
                        call tfreseterror
                    end if
                    irtc = 1
                    return
                end if
                isp = isp1 + 2
                dtastk(isp) = kx
                go to 11
            end if
        end if
        return
    case (nfunwith)
        if (full) then
        21 if (isp .eq. isp1 + 2) then
                kx = kxcompose(isp1)
                irtc = 0
            else if (isp .gt. isp1 + 2) then
                dh = dtastk(isp1 + 1)
                dtastk(isp1 + 1) = dtastk(isp1)
                kx = tfcomposefun(isp1 + 1, iah, .true., irtc)
                dtastk(isp1 + 1) = dh
                if (irtc .ne. 0) then
                    if (irtc .gt. 0 .and. ierrorprint .ne. 0) then
                        call tfreseterror
                    end if
                    irtc = 1
                    return
                end if
                isp = isp1 + 2
                dtastk(isp) = kx
                go to 21
            end if
            return
        end if
    case (nfunlength)
        if (isp .eq. isp1 + 1) then
            if (ktftype(ktastk(isp)) .eq. ktflist) then
                ka = iand(ktamask, ktastk(isp))
                if (klist(ka) .eq. ktfoper + mtflist) then
                    if (iand(ilist(2, ka-3), lnonreallist) .ne. 0) then
                        do i = ka + 1, ka + ilist(2, ka-1)
                            kti = ktftype(klist(i))
                            if (kti .eq. ktfsymbol) then
                                return
                            else if (kti .eq. ktflist) then
                            kai = iand(ktamask, klist(i))
                                if (klist(kai) .eq. ktfoper + mtfnull .or. klist(kai) &
                                    .eq. ktfoper + mtfslotseq) then
                                    return
                                end if
                            end if
                        end do
                    end if
                    kx = dfromr(dble(ilist(2, ka-1)))
                    irtc = 0
                end if
            end if
        end if
        return
    end select
    irtc = -1
    kx%k = ktfoper + mtfnull
    if (ktfnumericq(klist(ifunbase + iah))) then
        rimmediate0 = rlist(iaximmediate)
        rlist(iaximmediate) = -1.d0
        if (isp .eq. isp1 + 1) then
            if (ktfrealq(ktastk(isp))) then
                kx = tfefunref(isp1, .false., irtc)
            else if (tfconstq(ktastk(isp))) then
                if (ktflistq(ktastk(isp))) then
                    if (ilist(2, ktfaddr(ktastk(isp))-1) .le. 15) then
                        kx = tfefunref(isp1, .true., irtc)
                    end if
                else
                    kx = tfefunref(isp1, .true., irtc)
                end if
            end if
        else
            re = .false.
            do i = isp1 + 1, isp
                if (ktfrealq(ktastk(i))) then
                else if (tfconstq(ktastk(i))) then
                    re = .true.
                else
                    rlist(iaximmediate) = rimmediate0
                    return
                end if
            end do
            kx = tfefunref(isp1, re, irtc)
        end if
        rlist(iaximmediate) = rimmediate0
    end if
    return
end function tfcomposefun

subroutine tfevalb(string, kx, irtc)
    use tfstk
    implicit none
    type (sad_descriptor) , intent(out) :: kx
    type (sad_descriptor) tfeval
    integer(kind=4), intent(out) :: irtc
    integer(kind=4) istop
    character(len=*) string
    levele = levele + 1
    kx = tfeval(string, 1, istop, .false., irtc)
    call tfconnect(kx, irtc)
    return
end subroutine tfevalb

subroutine tfevalc(string)
    use tfstk
    implicit none
    type (sad_descriptor) kx, tfeval
    integer(kind=4) irtc, istop, l, itfdownlevel
    character(len=*), intent(in) :: string
    levele = levele + 1
    kx = tfeval(string, 1, istop, .false., irtc)
    if(irtc .gt. 0 .and. ierrorprint .ne. 0)then
      call tfreseterror
    endif
    l = itfdownlevel()
    return
end subroutine tfevalc

subroutine tfevals(string, kx, irtc)
    use tfstk
    implicit none
    type (sad_descriptor), intent(out) :: kx
    type (sad_descriptor) tfeval
    integer(kind=4), intent(out) :: irtc
    integer(kind=4) istop
    character(len=*), intent(in) :: string
    levele = levele + 1
    kx = tfeval(string, 1, istop, .false., irtc)
    call tfconnect(kx, irtc)
    return
end subroutine tfevals

logical(kind=4) function tfreadevalbuf(istart, istop, l, ipr)
    use tfcsi
    implicit none
    integer(kind=4),  intent (in) :: ipr
    integer(kind=4),  intent(out) :: l, istart
    integer(kind=4),  intent(inout) :: istop
    integer(kind=4) ip1
    ip1 = ipoint
    ipoint = istop
    call tprmptget(ipr, .true.)
    if(rep)then
      ipoint = ip1
    else
      istop = ipoint
    endif
    tfreadevalbuf = ios .eq. 0
    if(tfreadevalbuf)then
      istart = istop
      l = lrecl
    endif
    return
end function tfreadevalbuf

character(len=*) function tfkname(k)
    use tfstk
    implicit none
    integer(kind=8), intent(in) :: k
    if(ktfrealq(k))then
      tfkname = 'Real'
    else
      select case(ktftype(k))
      case(ktfoper)
        tfkname = 'Function'
      case(ktflist)
        tfkname = 'List'
      case(ktfstring)
        tfkname = 'String'
      case(ktfsymbol)
        tfkname = 'Symbol'
      case(ktfpat)
        tfkname = 'Pattern'
      case(ktfref)
        tfkname = 'Reference'
      case default
        tfkname = 'Unknown'
      end select
    endif
    return
end function tfkname