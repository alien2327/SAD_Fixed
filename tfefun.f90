module eexpr
    contains
    recursive function tfeexpr(k1, k, iopc1) result(ke)
        use tfstk
        implicit none
        type (sad_descriptor), intent(in) :: k1, k
        type (sad_descriptor) ke, kx, ky, ky1, k2, kx2, tftake, tfcompose, tfjoin2
        type (sad_dlist), pointer :: listy, list1, listi, klx, kl1
        type (sad_rlist), pointer :: kle
        type (sad_symbol), pointer :: sym
        type (sad_string), pointer :: str
        type (sad_pat), pointer :: kp1
        type (sad_complex), pointer :: cx
        integer(kind=8) ks
        integer(kind=4), intent(in) :: iopc1
        integer(kind=4) m, irtc, i, isp0, iopc, itfcanonicalorder
        real(kind=8) vx1, vy, v2, vx, x
        logical(kind=4) eval
        iopc = iopc1
        ky = k
        select case (iopc)
        case(mtfplus, mtftimes)
            if (.not. tfnumberq(k1) .and. tfnumberq(ky)) then
                kx = ky
                ky = k1
            else
                kx = k1
            end if
            if (ktflistq(ky, listy)) then
                if (listy%head%k == ktfoper + iopc) then
                    ke = tfjoine(kx, ky, irtc)
                    if (irtc .ne. -1) then
                        return
                    end if
                    irtc = 0
                    m = listy%nl
                    if (tfnumberq(listy%dbody(1))) then
                        if (tfnumberq(kx)) then
                            ky1 = tfcmplx(listy%dbody(1), kx, iopc, irtc)
                            if (irtc .ne. 0) then
                                ke%k = ktfoper + mtfnull
                                return
                            end if
                            if (ktfrealq(ky1)) then
                                if (iopc == mtfplus) then
                                    if (ky1%k == 0) then
                                        ke = merge(listy%dbody(2), tftake(ky, dfromr(dble(-m + 1)), &
                                            .true., .false., irtc), m== 2)
                                        return
                                    end if
                                else
                                    if (ky1%k == ktftrue) then
                                        ke = merge(listy%dbody(2), tftake(ky, dfromr(dble(-m + 1)), &
                                            .true., .false., irtc), m == 2)
                                        return
                                    else if (ky1%k == 0 .and. redmath%value%k .ne. 0) then
                                        ke%k = 0
                                        return
                                    end if
                                end if
                            end if
                            listy => tfclonelist(listy)
                            call tfreplist(listy, 1, ky1, eval)
                            ke = sad_descr(listy)
                        else
                            ke = tfinsertsort(listy, kx)
                        end if
                        return
                    end if
                    if (ktfrealq(kx)) then
                        if (iopc == mtfplus .and. kx%k == 0) then
                            ke = ky
                            return
                        else if (iopc == mtftimes) then
                            if (kx%k == ktftrue) then
                                ke = ky
                                return
                            else if (kx%k == 0 .and.redmath%value%k .ne. 0) then
                                ke%k = 0
                                return
                            end if
                        end if
                    end if
                    ke = tfinsertsort(listy, kx)
                    return
                end if
            end if
            if (ktflistq(kx, klx)) then
                if (klx%head%k == ktfoper + iopc) then
                    ke = tfinsertsort(klx, ky)
                    return
                end if
            else if (ktfrealq(kx)) then
                if (iopc == mtfplus .and. kx%k == 0) then
                    ke = ky
                    return
                else if (iopc == mtftimes) then
                    if (kx%k == ktftrue) then
                        ke = ky
                        return
                    else if (kx%k == 0 .and. redmath%value%k .ne. 0) then
                        ke%k = 0
                        return
                    end if
                end if
            end if
            isp = isp + 3
            ktastk(isp-2) = ktfoper + iopc
            if (itfcanonicalorder(ky, kx) .ge. 0) then
                dtastk(isp-1) = kx
                dtastk(isp  ) = ky
            else
                dtastk(isp-1) = ky
                dtastk(isp  ) = kx
            end if
            ke = kxcompose(isp-2)
            isp = isp-3
            return
        case (mtfnot)
            if (ktflistq(ky, listy)) then
                if (listy%head%k == ktfoper + mtfnot) then
                    ke = listy%dbody(1)
                    return
                end if
            end if
            go to 5000
        case (mtfslot, mtfslotseq)
            if (ktfrealq(ky, vy)) then
                ks = int8(vy)
                if (dble(ks) .ne. vy) then
                    go to 5000
                end if
                if (ks .le. 0.or. ks .gt. nslots) then
                    go to 5000
                end if
            else if (ky%k .ne. ktfoper + mtfnull) then
                go to 5000
            else
                ks = 1
            end if
            ke = merge(dlist(iaxslotnull + (ks-1)*2), dlist(iaxslotnull &
                + (ks-1)*2 + 1), iopc == mtfslot)
            return
        case (mtfflag)
            go to 5000
        case (mtfcomp, mtfconcat, mtfand, mtfor, mtfalt, mtfmessagename)
            if (ktflistq(k1, list1)) then
                if (list1%head%k == ktfoper + iopc) then
                    if (ktflistq(ky, listy)) then
                        if (listy%head%k == ktfoper + iopc) then
                            ke = tfjoin2(k1, ky, .false., irtc)
                            return
                        end if
                    end if
                    ke = tfappend(k1, ky, .false., 0, irtc)
                    return
                end if
            end if
            if (ktflistq(ky, listy)) then
                if (listy%head%k == ktfoper + iopc) then
                    ke = tfappend(ky, k1, .false., 1, irtc)
                    return
                end if
            end if
        case (mtfset, mtfpower)
            if (ktflistq(ky, listy)) then
                if (listy%head%k == ktfoper + iopc) then
                    ke = tfappend(ky, k1, .false., 1, irtc)
                    return
                end if
            end if
            if (iopc == mtfpower) then
                if (ktfrealq(ky)) then
                    if (ky%k == ktftrue) then
                        ke = k1
                        return
                    else if (ky%k == 0 .and. redmath%value%k .ne. 0) then
                        ke%k = ktftrue
                        return
                    end if
                end if
                if (ktflistq(k1, list1)) then
                    if (list1%head%k == ktfoper + mtfpower) then
                        m = list1%nl
                        if (m == 1) then
                            kx = ky
                        else
                            k2 = merge(list1%dbody(2), tftake(k1, dfromr(dble(-m + 1)), &
                                .true., .false., irtc), m == 2)
                            if (ktfrealq(k2, v2) .and. ktfrealq(ky, vy)) then
                                kx = dfromr(v2*vy)
                            else
                                kx = tfeexpr(k2, ky, mtftimes)
                            end if
                        end if
                        ky = list1%dbody(1)
                        ke = tfeexpr(ky, kx, mtfpower)
                        return
                    else if (list1%head%k == ktfoper + mtftimes .and. ktfrealq(ky)) then
                        vx1 = 1.d0
                        kx2%k = ktftrue
                        m = list1%nl
                        isp0 = isp
                        isp = isp + 1
                        ktastk(isp) = ktfoper + mtftimes
                        do i = 1, m
                            isp = isp + 1
                            dtastk(isp) = list1%dbody(i)
                            if (ktfrealq(ktastk(isp))) then
                                vx1 = vx1*rtastk(isp)
                                isp = isp-1
                            else if (ktflistq(ktastk(isp), listi)) then
                                if (listi%head%k == ktfoper + mtfcomplex) then
                                    kx = tfeexpr(dtastk(isp), ky, mtfpower)
                                if (ktfrealq(kx, vx)) then
                                    vx1 = vx1*vx
                                else
                                    kx2 = tfeexpr(kx2, kx, mtftimes)
                                end if
                                isp = isp-1
                                end if
                            end if
                        end do
                        if (isp .ne. isp0 + m + 1) then
                            if (isp .gt. isp0 + 2) then
                                kx = tfcompose(isp0 + 1, ktfoper + mtftimes, irtc)
                            else if (isp == isp0 + 2) then
                                kx = dtastk(isp)
                            else
                                kx%k = ktftrue
                            end if
                            isp = isp0
                            ke = tfeexpr(kx, ky, mtfpower)
                            if (ktfrealq(kx2, v2)) then
                                vx1 = vx1*v2
                            else
                                ke = tfeexpr(kx2, ke, mtftimes)
                            end if
                            if (vx1 .ne. 1.d0) then
                                kx = tfcmplx(sad_descr(vx1), ky, mtfpower, irtc)
                                ke = tfeexpr(kx, ke, mtftimes)
                            end if
                            return
                        else
                            isp = isp0
                        end if
                    end if
                else if (ktfrealq(k1)) then
                    if (k1%k == ktftrue .and. redmath%value%k .ne. 0) then
                        ke%k = ktftrue
                        return
                    end if
                end if
            end if
        case (mtfreplace, mtfreplacerepeated)
            if (ktflistq(k1, kl1)) then
                if (kl1%head%k == ktfoper + iopc) then
                    ke = tfappend(k1, ky, .false., 0, irtc)
                    return
                end if
            end if
        case (mtfcolon)
            if (ktfsymbolq(k1, sym)) then
                call sym_symstr(sym, str)
                ke = kxpalocb(str%str, str%nch, ky, transfer(ktfref, k))
                return
            else if (ktfpatq(k1, kp1)) then
                kp1%default = dtfcopy(ky)
                ke%k = ktfpat + ktfaddrd(k1)
                return
            end if
        case (mtfrevpower)
            ke = tfeexpr(k, k1, mtfpower)
            return
        case (mtfatt)
            if (ktfnonrealq(k1)) then
                if (ktfrealq(k, x)) then
                    ke = kxavaloc(-1, 1, kle)
                    kle%rbody(1) = x
                    kle%head = dtfcopy(k1)
                    return
                else if ((ktfsymbolq(ky) .or. ktfoperq(ky)) .and. (ktfsymbolq(k1) &
                    .or. ktflistq(k1)) .or. ktfpatq(ky)) then
                    go to 4900
                else if (ktflistq(k1, kl1) .and. kl1%head%k == ktfoper + mtfatt) then
                    isp = isp + 1
                    isp0 = isp
                    ktastk(isp0) = ktfoper + mtfatt
                    call tfgetllstkall(kl1)
                    isp = isp + 1
                    dtastk(isp) = ky
                    ke = kxcompose(isp0)
                    isp = isp0-1
                    return
                else
                    isp = isp + 2
                    dtastk(isp-1) = k1
                    dtastk(isp  ) = ky
                    ke = kxcompose(isp-1)
                    isp = isp-2
                    return
                end if
            end if
        case (mtfcomplex)
            if (ktfrealq(k)) then
                if (k%k == 0) then
                    ke = k1
                    return
                end if
            else if (tfcomplexq(k, cx)) then
                ky = tfeexpr(cx%dbody(1), k1, mtfplus)
                ke = tfeexpr(ky, cx%dbody(2), mtfcomplex)
                return
            else if (tfcomplexq(k1, cx)) then
                ky = tfeexpr(cx%dbody(2), k, mtfplus)
                ke = tfeexpr(cx%dbody(1), ky, mtfcomplex)
                return
            end if
        case (mtffun)
            if (k%k == ktfoper + mtfnull) then
                ke = kxpfaloc(k1)
                return
            end if
        end select
        4900 isp = isp + 3
        ktastk(isp-2) = ktfoper + iopc
        dtastk(isp-1) = k1
        dtastk(isp  ) = ky
        ke = kxcompose(isp-2)
        isp = isp-3
        return
        5000 if (ktfrealq(ky)) then
            ke = kxavaloc(-1, 1, kle)
            kle%dbody(1) = ky
        else
            ke = kxadaloc(-1, 1, klx)
            call descr_rlist(ke, kle)
            klx%dbody(1) = dtfcopy(ky)
            end if
        kle%head%k = ktfoper + iopc
        return 
    end function tfeexpr

    function tfinsertsort(kl, ki) result(kx)
        use tfstk
        implicit none
        type (sad_descriptor) kx
        type (sad_descriptor), intent(in) :: ki
        type (sad_dlist), intent(inout) :: kl
        integer(kind=4) isp0, isp1, isp2, i, ispm, itfcanonicalorder, isp3
        isp0 = isp
        call tfgetllstkall(kl)
        isp1 = isp0 + 1
        isp2 = isp + 1
        do while(isp2 .gt. isp1)
            ispm = isp1 + (isp2-isp1)/2
            i = itfcanonicalorder(ki, dtastk(ispm))
            if (i .gt. 0) then
                isp1 = ispm + 1
            else if (i == 0) then
                isp1 = ispm
                isp2 = ispm
            else
                isp2 = ispm
            end if
        end do
        isp2 = isp
        isp3 = isp + isp1-isp0-1
        ktastk(isp + 1:isp3) = ktastk(isp0 + 1:isp1-1)
        isp = isp3 + 1
        dtastk(isp) = ki
        ktastk(isp + 1:isp + isp2-isp1 + 1) = ktastk(isp1:isp2)
        isp = isp + isp2-isp1 + 1
        kx = kxcrelistm(isp-isp2, ktastk(isp2 + 1:isp), kl%head)
        isp = isp0
        return
    end function tfinsertsort

    function tfappend(kl, k, eval, mode, irtc) result(kx)
        use tfstk
        use eeval
        implicit none
        type (sad_descriptor) kx
        type (sad_descriptor), intent(in) :: kl, k
        type (sad_dlist), pointer :: list, listx
        integer(kind=4), intent(out) :: irtc
        integer(kind=4), intent(in) :: mode
        integer(kind=4) m, itfmessage, i
        logical(kind=4), intent(in) :: eval
        logical(kind=4) ev
        if (.not. ktflistq(kl, list)) then
            kx = dxnull
            irtc = itfmessage(9, 'General::wrongtype', '"List or composition for #1"')
            return
        end if
        ev = eval .and. list%head%k .ne. ktfoper + mtflist .and. list%head%k &
            .ne. ktfoper + mtfalt .and. list%head%k .ne. ktfoper + mtfnull
        m = list%nl
        call loc_sad(ktaaloc(-1, m + 1), listx)
        listx%attr = list%attr
        if (ktfreallistq(list)) then
            if (mode == 0) then
                listx%dbody(1:m) = list%dbody(1:m)
                if (ktfrealq(k)) then
                    listx%dbody(m + 1) = k
                else
                    listx%dbody(m + 1) = dtfcopy(k)
                    listx%attr = ior(listx%attr, lnonreallist)
                end if
            else
                listx%dbody(2:m + 1) = list%dbody(1:m)
                if (ktfrealq(k)) then
                    listx%dbody(1) = k
                else
                    listx%dbody(1) = dtfcopy(k)
                    listx%attr = ior(listx%attr, lnonreallist)
                end if
            end if
        else
            if (mode == 0) then
                do i = 1, m
                    listx%dbody(i) = dtfcopy(list%dbody(i))
                end do
                listx%dbody(m + 1) = dtfcopy(k)
            else
                do i = 1, m
                    listx%dbody(i + 1) = dtfcopy(list%dbody(i))
                end do
                listx%dbody(1) = dtfcopy(k)
            end if
        end if
        listx%head = dtfcopy(list%head)
        if (iand(list%attr, kconstarg) .ne. 0) then
            if (.not. tfconstq(k%k)) then
                listx%attr = ior(listx%attr-kconstarg, knoconstarg + lnoconstlist)
            end if
        end if
        if (ev) then
            kx = tfleval(listx, .true., irtc)
        else
            kx = sad_descr(listx)
            irtc = 0
        end if
        return
    end function tfappend

    function tfnot(isp1, iopc, irtc) result(kx)
        use tfstk
        implicit none
        type (sad_descriptor) kx
        integer(kind=4) isp1, irtc, iopc, itfmessage
        if (isp .ne. isp1 + 1) then
            kx = dxnullo
            irtc = itfmessage(9, 'General::narg', '"1"')
            return
        end if
        if (tfnumberq(dtastk(isp))) then
            kx = tfcmplx(dfromr(0.d0), dtastk(isp), iopc, irtc)
        else if (tflistq(dtastk(isp))) then
            kx = tfearray(dfromr(0.d0), dtastk(isp), iopc, irtc)
        else
            kx = tfeexpr(dfromr(0.d0), dtastk(isp), iopc)
            irtc = 0
        end if
        return
    end function tfnot

    function tfplus(isp1, iopc, irtc) result(kx)
        use tfstk
        implicit none
        type (sad_descriptor) kx, k1, k, ki, tfecmplxl
        type (sad_dlist), pointer :: klx
        integer(kind=4), intent(in) :: isp1, iopc
        integer(kind=4), intent(out) :: irtc
        integer(kind=4) i, narg
        real(kind=8) v, v1, vx, vi
        kx = dxnullo
        narg = isp-isp1
        if (narg == 2) then
            k1 = dtastk(isp1 + 1)
            k  = dtastk(isp)
            if (ktfrealq(k1, v1) .and. ktfrealq(k, v)) then
                kx = merge(dfromr(v1 + v), dfromr(v1*v), iopc == mtfplus)    
                irtc = 0
            else
                if (tfnumberq(k) .and. tfnumberq(k1)) then
                    kx = tfcmplx(k1, k, iopc, irtc)
                else
                    kx = merge(tfecmplxl(k1, k, iopc), tfeexpr(k1, k, iopc), tflistq(k1) .or. tflistq(k))
                    irtc = 0
                end if
            end if
            return
        else if (narg == 0) then
            kx%k = merge(ktffalse, ktftrue, iopc == mtfplus)
        else if (narg == 1) then
            if (ktfsymbolq(dtastk(isp)) .or. ktfpatq(dtastk(isp))) then
                kx = kxmakelist(isp1, klx)
                klx%head%k = ktfoper + iopc
            else
                kx = dtastk(isp1 + 1)
            end if          
            irtc = 0
        else
            kx = dtastk(isp1 + 1)
            irtc = 0
            do i = isp1 + 2, isp
                ki = dtastk(i)
                if (ktfrealq(ki, vi) .and. ktfrealq(kx, vx)) then
                    kx = merge(dfromr(vx + vi), dfromr(vx*vi), iopc == mtfplus)    
                else
                    k1 = kx
                    if (tfnumberq(k1) .and. tfnumberq(ki)) then
                        kx = tfcmplx(k1, ki, iopc, irtc)
                        if (irtc .ne. 0) then
                            return
                        end if
                    else if (tflistq(k1) .or. tflistq(ki)) then
                        kx = tfecmplxl(k1, ki, iopc)
                        if (irtc .ne. 0) then
                            return
                        end if
                    else
                        kx = tfeexpr(k1, ki, iopc)
                    end if
                end if
            end do
        end if
        return
    end function tfplus

    function tfpower(isp1, irtc) result(kx)
        use tfstk
        implicit none
        type (sad_descriptor) kx, k1, ki, tfecmplxl
        integer(kind=4), intent(in) :: isp1
        integer(kind=4), intent(out) :: irtc
        integer(kind=4) i, itfmessage
        if (isp1 == isp) then
            kx = dxnullo
            irtc = itfmessage(9, 'General::narg', '"1 or more"')
            return
        end if
        kx = dtastk(isp)
        irtc = 0
        if (isp == isp1 + 1) then
            return
        end if
        do i = isp-1, isp1 + 1, -1
            ki = dtastk(i)
            k1 = kx
            if (tfnumberq(k1) .and. tfnumberq(ki)) then
                kx = tfcmplx(ki, k1, mtfpower, irtc)
                if (irtc .ne. 0) then
                    return
                end if
            else if (tflistq(k1) .or. tflistq(ki)) then
                kx = tfecmplxl(ki, k1, mtfpower)
                irtc = 0
            else
                kx = tfeexpr(ki, k1, mtfpower)
            end if
        end do
        return
    end function tfpower

    function tfrevpower(isp1, irtc) result(kx)
        use tfstk
        implicit none
        type (sad_descriptor) kx, k1, ki, tfecmplxl
        integer(kind=4), intent(in) :: isp1
        integer(kind=4), intent(out) :: irtc
        integer(kind=4) i, itfmessage
        if (isp1 == isp) then
            kx = dxnullo
            irtc = itfmessage(9, 'General::narg', '"1 or more"')
            return
        end if
        kx = dtastk(isp1 + 1)
        irtc = 0
        if (isp == isp1 + 1) then
            return
        end if
        do i = isp1 + 2, isp
            ki = dtastk(i)
            k1 = kx
            if (tfnumberq(k1) .and. tfnumberq(ki)) then
                kx = tfcmplx(ki, k1, mtfpower, irtc)
                if (irtc .ne. 0) then
                    return
                end if
            else if (tflistq(k1) .or. tflistq(ki)) then
                kx = tfecmplxl(ki, k1, mtfpower)
                irtc = 0
            else
                kx = tfeexpr(ki, k1, mtfpower)
            end if
        end do
        return
    end function tfrevpower

    function tfequal(isp1, iopc, irtc) result(kx)
        use tfstk
        implicit none
        type (sad_descriptor) kx
        integer(kind=4), intent(in) :: isp1, iopc
        integer(kind=4), intent(out) :: irtc
        integer(kind=4) itfmessage
        if (isp .lt. isp1 + 2) then
            kx = dxnullo
            irtc = itfmessage(9, 'General::narg', '"2 or more"')
            return
        end if
        if (isp == isp1 + 2 .and. ktfstringq(dtastk(isp)) .and. ktfstringq(dtastk(isp1 + 1))) then
            kx%k = merge(ktftrue, ktffalse, tfsamestringq(dtastk(isp), dtastk(isp1 + 1)))
            if (iopc == mtfunequal) then
                kx%k = ktftrue-kx%k
            end if
            irtc = 0
        else
            kx = tfrelation(isp1, iopc, irtc)
        end if
        return
    end function tfequal

    recursive function tfrelation(isp1, iopc, irtc) result(kx)
        use tfstk
        implicit none
        type (sad_descriptor) kx
        integer(kind=4), intent(in) :: isp1, iopc
        integer(kind=4), intent(out) :: irtc
        integer(kind=4) itfmessage, isp0, k
        kx = dxnullo
        if (isp .lt. isp1 + 2) then
            irtc = itfmessage(9, 'General::narg', '"2 or more"')
            return
        end if
        if (isp == isp1 + 2) then
            if (tfnumberq(dtastk(isp1 + 1)) .and. tfnumberq(dtastk(isp))) then
                kx = tfcmplx(dtastk(isp1 + 1), dtastk(isp), iopc, irtc)
            else if (tflistq(dtastk(isp1 + 1)) .or. tflistq(dtastk(isp))) then
                kx = tfearray(dtastk(isp1 + 1), dtastk(isp), iopc, irtc)
            else
                kx = tfeexpr(dtastk(isp1 + 1), dtastk(isp), iopc)
                irtc = 0
            end if
        else
            isp0 = isp
            do k = 1, isp0-isp1-1
                ktastk(isp0 + 1) = ktastk(isp1 + k)
                ktastk(isp0 + 2) = ktastk(isp1 + k + 1)
                isp = isp0 + 2
                kx = tfrelation(isp0, iopc, irtc)
                if (irtc .ne. 0) then
                    isp = isp0
                    return
                else if (ktfnonrealq(kx)) then
                    irtc = -1
                    return
                end if
                if (kx%k == 0) then
                    isp = isp0
                    return
                end if
            end do
            isp = isp0
        end if
        return
    end function tfrelation

    function tfecmplx1(k1, k2i, iopc1, c, d) result(kxi)
        use tfstk
        implicit none
        type (sad_descriptor) kxi, tfecmplxl
        type (sad_descriptor), intent(in) :: k1, k2i
        integer(kind=4), intent(in) :: iopc1
        logical(kind=4), intent(inout) :: c, d
        if (tflistq(k2i)) then
            kxi = dtfcopy(tfecmplxl(k1, k2i, iopc1))
            d = .true.
            c = c .and. tfconstq(kxi%k)
            kxi = dtfcopy(kxi)
        else
            kxi = tfeexpr(k1, k2i, iopc1)
            if (ktfnonrealq(kxi)) then
                c = c .and. tfconstq(kxi%k)
                d = .true.
                kxi = dtfcopy(kxi)                  
            end if
        end if
        return
    end function tfecmplx1

    function tfcmplx(k1, k2, iopc, irtc) result(kx)
        use tfstk
        implicit none
        type (sad_descriptor) kx
        type (sad_descriptor), intent(in) :: k1, k2
        type (sad_descriptor) tfenum, tfeval1
        type (sad_complex), pointer :: cx1, cx2
        integer(kind=8) ki1, ki2
        integer(kind=4), intent(out) :: irtc
        integer(kind=4), intent(in) :: iopc
        real(kind=8) v1, v2
        complex(kind=16) c1
        irtc = 0
        if (iopc == mtfnot) then
            if (ktfrealq(k2)) then
                if (k2%k == 0) then
                    kx%k = ktftrue
                else
                    kx%k = 0
                end if
            else
                go to 8000
            end if
            return
        end if
        if (ktfrealq(k1, v1)) then
            if (ktfrealq(k2, v2)) then
                if (iopc == mtfcomplex) then
                    kx = kxcalocv(-1, v1, v2)
                else if (iopc == mtfpower) then
                    if (k1%k .lt. 0) then
                        ki2 = int(v2)
                        if (ki2 .ne. v2) then 
                            c1 = cmplx(v1, 0.d0, kind=16)**v2
                            kx = kxcalocc(-1, c1)
                        else
                            kx = dfromr(v1**ki2)
                        end if
                        return
                    end if
                    kx = tfenum(k1%x(1), k2%x(1), mtfpower, irtc)
                else if (iopc == mtfrevpower) then
                    if (k2%k .lt. 0) then
                        ki1 = int(v1)
                        if (ki1 .ne. v1) then 
                            c1 = cmplx(v2, 0.d0, kind=16)**v1
                            kx = kxcalocc(-1, c1)
                        else
                            kx = sad_descr(v2**ki1)
                        end if
                        return
                    end if
                    kx = tfenum(v2, v1, mtfpower, irtc)
                else
                    kx = tfenum(k1%x(1), k2%x(1), iopc, irtc)
                end if
                return
            else if (tfcomplexq(k2, cx2)) then
                kx = tfcmplxmath(cmplx(v1, 0.d0, kind=16), cx2%cx(1), iopc, irtc)
                return
            else
                go to 8000
            end if
        else if (tfcomplexq(k1, cx1)) then
            if (ktfrealq(k2, v2)) then
                kx = tfcmplxmath(cx1%cx(1), cmplx(v2, 0.d0, kind=16), iopc, irtc)
                return
            else if (tfcomplexq(k2, cx2)) then
                kx = tfcmplxmath(cx1%cx(1), cx2%cx(1), iopc, irtc)
                return
            end if
        end if
        8000 kx = tfeval1(k1, k2, iopc, irtc)
        return
    end function tfcmplx

    function tfcmplxmath(c1, c2, iopc1, irtc) result(kx)
        use tfstk
        implicit none
        type (sad_descriptor) kx
        integer(kind=4), intent(in) :: iopc1
        integer(kind=4), intent(out) :: irtc
        complex(kind=16), intent(in) :: c1, c2
        complex(kind=16) cx
        if (iopc1 .gt. mtfunequal .and. iopc1 .ne. mtfcomplex) then
            kx = dxnullo
            irtc = -1
        else
            cx = tfcmplxmathv(c1, c2, iopc1)
            kx = kxcalocc(-1, cx)
            irtc = 0
        end if
        return
    end function tfcmplxmath

    complex(kind=16) function tfcmplxmathv(c1, c2, iopc1)
        use tfstk
        implicit none
        integer(kind=4), intent(in) :: iopc1
        integer(kind=8) i1, i2
        complex(kind=16), intent(in) :: c1, c2
        select case(iopc1)
        case (mtfneg)
            tfcmplxmathv = -c2
        case (mtfinv)
            tfcmplxmathv = 1.d0/c2
        case (mtfplus)
            tfcmplxmathv = c1 + c2
        case (mtftimes)
            tfcmplxmathv = c1*c2
        case (mtfrevpower)
            if (imag(c1) == 0.d0) then
                i1 = int8(c1)
                tfcmplxmathv = merge(merge(1.d0/c2, c2**i1, i1 == -1), c2**dble(c1), i1 == dble(c1))
            else
                tfcmplxmathv = c2**c1
            end if
        case(mtfpower)
            if (imag(c2) == 0.d0) then
                i2 = int8(c2)
                tfcmplxmathv = merge(merge(cmplx(1.d0/c1, kind=8), merge((1.d0, 0.d0), cmplx(c1**i2, kind=8), &
                    i2 == 0 .and. redmath%value%k .ne. 0), i2 == -1), cmplx(c1**dble(c2), kind=8), i2 == dble(c2))
            else
                tfcmplxmathv = c1**c2
            end if
        case (mtfequal)
            tfcmplxmathv = merge(1.d0, 0.d0, c1 == c2)
        case (mtfunequal)
            tfcmplxmathv = merge(1.d0, 0.d0, c1 .ne. c2)
        case (mtfcomplex)
            tfcmplxmathv = c1 + cmplx(-imag(c2), dble(c2), kind=16)
        case default
            tfcmplxmathv = 0.d0
        end select
        return
    end function tfcmplxmathv

    function tfearray(k1, k, iopc1, irtc) result(kx)
        use tfstk
        implicit none
        type (sad_descriptor), intent(in) :: k, k1
        type (sad_descriptor) kx
        type (sad_descriptor) ky, tfdot, tfecmplxl
        type (sad_dlist), pointer :: kl, kl1
        integer(kind=4), intent(out) :: irtc
        integer(kind=4) ne, ne1, i, iopc1, isp0
        logical(kind=4) list1, list
        irtc = 0
        do
            select case(iopc1)
            case (mtfplus:mtfless, mtfand:mtfnot, mtfcomplex)
                kx = tfecmplxl(k1, k, iopc1)
                return
            case (mtfsame:mtfunsame)
                exit
            end select
            if (ktflistq(k1, kl1)) then
                if (tfcomplexq(k1)) then
                    ne1 = 0
                    list1 = .false.
                else
                    if (tfexprq(k1)) then
                        exit
                    end if
                    ne1 = kl1%nl
                    list1 = .true.
                end if
            else
                ne1 = 0
                list1 = .false.
            end if
            if (ktflistq(k, kl)) then
                if (tfcomplexq(k)) then
                    list = .false.
                    ne = 0
                else
                    if (tfexprq(k)) then
                        exit
                    end if
                    if (iopc1 == mtfdot) then
                        kx = tfdot(k1, k, irtc)
                        return
                    end if
                    ne = kl%nl
                    list = .true.
                    if (list1) then
                        if (iopc1 == mtfequal) then
                            if (ne .ne. ne1) then
                                kx%k = 0
                            else
                                kx = tfecmplxl(k1, k, iopc1)
                            end if
                            return
                        else if (iopc1 == mtfunequal) then
                            if (ne .ne. ne1) then
                                kx%k = ktftrue
                            else
                                kx = tfecmplxl(k1, k, iopc1)
                            end if
                            return
                        !   else if (ne / =  ne1) then
                        else 
                            exit
                        end if
                    end if
                end if
            else
                list = .false.
                ne = 0
            end if
            if (list1) then
                if (list) then
                    if (iopc1 == mtfequal) then
                        kx%k = ktftrue
                        do i = 1, ne
                            ky = tfcmplx(kl1%dbody(i), kl%dbody(i), iopc1, irtc)
                            if (irtc .ne. 0) then
                                return
                            end if
                            if (ky%k == 0) then
                                kx%k = 0
                                return
                            else if (.not. ktfrealq(ky)) then
                                exit
                            end if
                        end do
                        return
                    else if (iopc1 == mtfunequal) then
                        kx%k = 0
                        do i = 1, ne
                            ky = tfcmplx(kl1%dbody(i), kl%dbody(i), iopc1, irtc)
                            if (irtc .ne. 0) then
                                return
                            end if
                            if (ky%k == ktftrue) then
                                kx%k = ktftrue
                                return
                            else if (.not. ktfrealq(ky)) then
                                exit
                            end if
                        end do
                        return
                    else
                        isp0 = isp
                        do i = 1, ne
                            isp = isp + 1
                            dtastk(isp) = tfcmplx(kl1%dbody(i), kl%dbody(i), iopc1, irtc)
                            if (irtc .ne. 0) then
                                kx = dxnullo
                                isp = isp0
                                return
                            end if
                        end do
                        kx = kxmakelist(isp0)
                        isp = isp0
                    end if
                else
                    if (iopc1 == mtfequal .or. iopc1 == mtfunequal) then
                        exit
                    end if
                    isp0 = isp
                    do i = 1, ne
                        isp = isp + 1
                        dtastk(isp) = tfcmplx(kl1%dbody(i), k, iopc1, irtc)
                        if (irtc .ne. 0) then
                            kx = dxnullo
                            isp = isp0
                            return
                        end if
                    end do
                    kx = kxmakelist(isp0)
                    isp = isp0
                end if
            else
                if (iopc1 == mtfequal .or. iopc1 == mtfunequal) then
                    exit
                end if
                isp0 = isp
                do i = 1, ne
                    isp = isp + 1
                    dtastk(isp) = tfcmplx(k1, kl%dbody(i), iopc1, irtc)
                    if (irtc .ne. 0) then
                        kx = dxnullo
                        isp = isp0
                        return
                    end if
                end do
                kx = kxmakelist(isp0)
                isp = isp0
            end if
            return
        end do
        kx = tfeexpr(k1, k, iopc1)
        return
    end function tfearray

    function tfjoine(k1, k2, irtc) result(kx)
        use tfstk
        implicit none
        type (sad_descriptor) kx, tfjoin2
        type (sad_descriptor), intent(in) :: k1, k2
        type (sad_descriptor) k10, k20, ky1
        type (sad_dlist), pointer ::kl1, kl2
        integer(kind=4), intent(out) :: irtc
        integer(kind=4) ma1, ma2, m, iopc, isp1
        if (ktfnonlistq(k1, kl1) .or. ktfnonlistq(k2, kl2) .or. .not. tfsameheadq(k1, k2)) then
            kx = dxnullo
            irtc = -1
            return
        end if
        irtc = 0
        iopc = int(ktfaddr(kl1%head%k))
        if (iopc == mtfplus .or. iopc == mtftimes) then
            if (.not. tfnumberq(kl1%dbody(1))) then
                kx = tfjoin2(k2, k1, .false., irtc)
                go to 1000
            end if
            if (.not. tfnumberq(kl2%dbody(1))) then
                kx = tfjoin2(k1, k2, .false., irtc)
                go to 1000
            end if
            ma1 = kl1%nl
            ma2 = kl2%nl
            m = ma1 + ma2-1
            k10 = kl1%dbody(1)
            k20 = kl2%dbody(1)
            ky1 = tfcmplx(k10, k20, iopc, irtc)
            if (irtc .ne. 0) then
                kx = dxnullo
                return
            end if
            if (ktfrealq(ky1)) then
                if (iopc == mtfplus) then
                    if (ky1%k == 0) then
                        m = m-1
                    end if
                else
                    if (ky1%k == 0) then
                        kx%k = 0
                        return
                    else if (ky1%k == ktftrue) then
                        m = m-1
                    end if
                end if
            end if
            if (m == 1) then
                kx = kl2%dbody(2)
                return
            end if
            isp1 = isp
            if (m == ma1 + ma2-2) then
                call tfgetllstk(kl1, 2, -1)
                call tfgetllstk(kl2, 2, -1)
            else
                isp = isp + 1
                dtastk(isp) = ky1
                call tfgetllstk(kl1, 2, -1)
                call tfgetllstk(kl2, 2, -1)
            end if
            kx = kxcrelistm(isp-isp1, ktastk(isp1 + 1:isp), k_descr(ktfoper + iopc))
            isp = isp1
        else
            kx = tfjoin2(k1, k2, .false., irtc)
            return
        end if
        1000 isp = isp + 1
        dtastk(isp) = kx
        call tfsort(isp-1, kx, 0, irtc)
        isp = isp-1
        return
    end function tfjoine
end module eexpr

module efun
    contains
    recursive function tfefunref(isp1, upvalue, irtc) result(kx)
    use tfstk
    use tfmem
    use tfshare
    use tfcsi, only:lfno
    use mathfun
    use eexpr
    use funs
    use eeval
    use readbuf
    use gammaf
    implicit none
    type (sad_descriptor) kx, k1, k, kh, tfmodule, tfsolvemember, tftable, &
      tfefun1, tfeval1, tfeintf, tfeintf2, tfget, tftake, tfeval1to, tfmap, &
      tfminmax, tfgetcommandline, tfreplacepart, tfpart, tfwrite, tftemporaryname, &
      tfmapfile, tfunmapfile, tfcmplxf, tfgaussiancoulomb, tfgaussiancoulombu, tfrange
    type (sad_dlist),  pointer :: kl, kl1, klx, klh
    type (sad_symbol),  pointer :: sym1
    type (sad_symdef),  pointer :: symd
    type (sad_string),  pointer :: str
    integer(kind=4) ka1, ka, kax, km, kop
    integer(kind=4), intent(in):: isp1
    integer(kind=4), intent(out):: irtc
    integer(kind=4) i, id, narg, nc, isp2, itfdepth, itfgetrecl, ltr0, iaf, &
      itfopenwrite, itfmessage, itfopenappend, itfopcode, isp0, nsize
    real(kind=4) vx, v1, v, f
    complex(kind=16) c1
    character(len=3) opcx
    character(len=4) char8
    logical(kind=4), intent(in):: upvalue
    logical(kind=4) euv, tfnearlysameqf, rep
    type (sad_descriptor),  save :: ilog2
    data ilog2%k /0/

!     DOUBLE specific math intrinsic function
!     from Fortran77    77   77   77
    intrinsic dabs, dsqrt, dexp, dlog
!     from Fortran77   77   77    77    77    77     77
    intrinsic dsin, dcos, dtan, dasin, dacos, datan, datan2
!     from Fortran 77    77    77
    intrinsic dsinh, dcosh, dtanh
!     from Fortran 08    08  <-- somehow cannot pass as an argument @ gfortran 5
!      intrinsic derf, derfc

!     DOUBLE COMPLEX specific math intrinsic function
!     from Fortran EX    EX     EX    EX    EX
    intrinsic cdsin, cdcos, cdsqrt, cdexp, cdlog

!     DOUBLE specific `t-prefix' math function proxy
!     for Fortran2008 generic function
    real(kind=4)   tasinh, tacosh, tatanh
    external tasinh, tacosh, tatanh

!     DOUBLE COMPLEX specific `t-prefix' math function proxy
!     for vendor extended math intrinsic function
    complex(kind=16) tctan
    external   tctan

!     DOUBLE COMPLEX specific math function implemented by SAD
!     real(kind=4),  external :: dlgama

!     real(kind=4),  external:: aloggamma1, factorial, gammaq, gammap, inverseerf, 
!     $     productlog, gamma0, ferf, ferfc
!      complex(kind=16),  external:: cloggamma1, cfactorial, cerfc, cerf, 
!     $     cproductlog
    if (upvalue) then
      LOOP_I: do i = isp1 + 1, isp
        k1 = dtastk(i)
12       if (ktflistq(k1, kl1)) then
          klh => kl1
          k1 = kl1%head
          do while(ktflistq(k1, kl1))
            k1 = kl1%head
          end do
          if (k1%k == ktfoper + mtfatt .or. k1%k == ktfoper + mtfslot) then
            k1 = tfsolvemember(klh, rep, irtc)
            if (irtc == 0) then
              dtastk(i) = k1
              go to 12
            else if (irtc .gt. 0) then
              return
            end if
          end if
        end if
        if (ktfsymbolq(k1, sym1)) then
          if (sym1%override == 0) then
            sym1 => tfsydef(sym1)
          end if
          call sym_symdef(sym1, symd)
          if (symd%upval .ne. 0) then
            call tfdeval(isp1, sad_loc(symd%sym%loc), kx, 0, .false., euv, irtc)
            if (euv) then
              return
            end if
          end if
        end if
      end do LOOP_I
    end if
    k1 = dtastk(isp1)
    narg = isp-isp1
    if (ktfoperq(k1, ka1)) then
      if (ka1 .le. mtfend) then
        go to 6000
      end if
      if (narg == 0) then
        narg = 1
        isp = isp + 1
        ktastk(isp) = ktfoper + mtfnull
      end if
      k = dtastk(isp)
      id = iget_fun_id(int(ka1, kind=8))
      irtc = -1
      go to (110,  120,  130,  140,  150,  160,  170,  180,  190,  200,  &
            210,  220,  230,  240,  240,  260,  270,  280,  290,  300, &
            310,  320,  330,  340,  350,  360,  370,  380,  380,  400, &
            410,  420,  430,  440,  440,  460,  470,  480,  490,  500, &
            510,  520,  530,  540,  550,  560,  570,  580,  590,  600, &
!              Sin  Cos  Tan  Sinh Cosh Tanh Exp  Log  Atan Det
!              Sqrt Flor Ceil Min  Max  Mod  StrL Arg  Sign Leng
!              Dims RplP ASin ACos ASh  ACh  ATh  Tabl Do   Attr
!              Peek Abs  Revs Modl Blck StrR SwiC Fltn If   Take
!              Selc Whil Join Apnd Ppnd Clr  Prot Unpr Drop MpAt
!
            610,  620,  630,  640,  650,  660,  670,  680,  690,  700, &
            700,  720,  730,  730,  730,  760,  770,  780,  790,  800, &
            810,  820,  830,  840,  850,  860,  870,  880,  890,  900, &
            910,  920,  930,  940,  950,  960,  970,  980,  990, 1000, &
           1010, 1020, 1030, 1040, 1050, 1060, 1070, 1080, 1090, 1100, &
!              Innr Trps SglV DiaM LinS IdtN Eigs Oper Posi Sum
!              Prod Rang Re   Im   Conj ToSt Dpth Levl Writ Get
!              OpnW OpnA Clos Flsh Prnt WStr Retn Head RLiQ Pttn
!              Thrw Ctch Thrd SetA MpId FrCh ToCh CmpQ Tr   SvSM
!              Stch Sort Uni1 Ordr MChk Scan Iden TimU NumQ VecQ
!
           1110, 1120, 1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, &
           1210, 1220, 1230, 1240, 1250, 1260, 1270, 1280, 1290, 1300, &
           1310, 1320, 1330, 1340, 1350, 1360, 1370, 1380, 1390, 1390, &
           1290, 1420, 1430, 1440, 1450, 1460, 1470, 1480, 1490, 1500, &
           1510, 1520, 1530, 1540, 1540, 1560, 1570, 1580, 1590, 1600, &
!              AtmQ Outr MatQ TrcP Defi READ Ints Cmpl Roun IErf
!              FrDt PolG ToIS ReaS OpnR ToEx StrM StrP ToUp Brek
!              Cont Goto Four IFou Chek Whic MapF UnmF GetU GetG
!              ToLo Unev Case DelC Vect LogG Nams GbCl LgG1 Fact
!              With WhiC Ovrr AppT PreT FndR GamR GmRP Erf  Erfc
!
           1610, 1620,  760, 1640, 1650, 1660, 1670, 1680, 1690, 1700, &
           1710, 1720, 1730, 1740, 1750, 1760, 1770, 1770, 1770, 1770, &
           1810, 1820, 1830, 1840, 1850, 1860, 1870, 1870, 1870, 1900, &
           1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1980, 1980, &
           2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2080, 2100, &
!              Fit  Symb SyNm Extr Read Skip TmpN Exit StrF Rstr
!              MM   Shrt $SOT Dir  SDir Wait BesJ BesY BesI BesK
!              BasF StrT S2S  Even OddQ DatS Inst Delt FlAt Repl
!              SetE Spl$ FInd SCnt SCnP ToCt Ctxt BAnd BOr  BXor
!              RepM MSca StdF Abrt ChkA RelH NaNQ MapT ScaT Last
!
           2100, 2100, 2100, 2140, 2150, 2160, 2170, 2180, 2190, 2200, &
           2210, 2220, 2230, 2240, 2250, 2260, 2270, 2280, 2290, 2300, &
           2310, 2320, 2330, 2340, 2350, 2360, 2370, 2380, 2390, 2400, &
           2410, 2420, 2430, 2440, 2450, 2460, 2470, 2480, 2490, 2500, &
           2510, 2520, 2530, 2540, 2550, 2560, 2570, 2580, 2590), id
!              Frst Scnd Thrd ObjS PrdL GauC ClMO MAll Dupl GCLn
!              Seek DigQ LetQ ReaQ NSmQ OpSh RdSh WrSh ShSz FBuQ
!              GaCU GaCF Rest RRt1 Diff Gam0 XSIn Poch Hg21 H21R 
!              Hg11 H11R Hg01 H01R Zeta PGmM DZet HZet DHZt PGmH
!              Gamm HgU  HgPQ HPQR PLog Beta HLcP LchP BerB  
      if (id .gt. 0) then
        if (id .le. 2000) then
          kx = tfefun1(isp1, id, .true., irtc)
          go to 6900
        else if (id .le. 3000) then
          call tfefun2(isp1, id, k, kx, irtc)
          go to 6900
        else if (id .le. 4000) then
          call tfefun3ep(isp1, id, kx, irtc)
          go to 6900
        else if (id .le. 5000) then
          call tfefunctbl8(isp1, id, kx, irtc)
          go to 6900
        end if
      end if
      go to 100
100    write(*, *)'Function implementation error: ', id
      call tfdebugprint(k1, 'tfefun', 1)
      irtc = 0
      go to 7000
110    if (narg == 1) then
        kx = tfeintf(dsin, cdsin, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
120    if (narg == 1) then
        kx = tfeintf(dcos, cdcos, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
130    if (narg == 1) then
        kx = tfeintf(dtan, tctan, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
140    if (narg == 1) then
        kx = tfeintf(dsinh, tcsinh, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
150    if (narg == 1) then
        kx = tfeintf(dcosh, tccosh, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
160    if (narg == 1) then
        kx = tfeintf(dtanh, tctanh, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
170    if (narg == 1) then
        kx = tfeintf(dexp, cdexp, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
180    if (narg == 1) then
        kx = tfeintf(dlog, cdlog, k, .true., 0.d0, dinfinity, irtc)
      else if (narg == 2) then
        if (ilog2%k == 0) then
          ilog2 = kxsymbolf('Log2$', 5, .true.)
        end if
        dtastk(isp1) = ilog2
        call tfefun(isp1, kx, .true., .false., irtc)
        isp = isp1 + 2
        dtastk(isp1) = k1
      else
        irtc = itfmessage(9, 'General::narg', '"1 or 2"')
      end if
      go to 6900
190    if (narg == 1) then
        kx = tfeintf(datan, tcatan, k, .true., -dinfinity, dinfinity, irtc)
      else if (narg == 2) then
        kx = tfeintf2(datan2, tcatan2, dtastk(isp-1), k, .true., irtc)
      else
        go to 6812
      end if
      go to 6900
200    call tfdet(isp1, kx, irtc)
      go to 6900
210    if (narg == 1) then
        kx = tfeintf(dsqrt, cdsqrt, k, .true., 0.d0, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
220    if (narg == 1) then
        kx = tfeintf(tfloor, tcfloor, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
230    if (narg == 1) then
        kx = tfeintf(tceiling, tcceiling, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
240    kx = tfminmax(isp1, id-13, irtc)
      go to 6900
260    call tfmod(isp1, kx, 0, irtc)
      go to 6900
270    if (narg == 1) then
        if (ktfstringq(k, str)) then
          kx = dfromr(dble(str%nch))
          go to 8000
        else
          irtc = itfmessage(9, 'General::wrongtype', '"Character-string"')
        end if
      else
        go to 6811
      end if
      go to 6900
280    if (narg == 1) then
        kx = tfeintf(tfarg, tfcarg, k, .false., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
290    if (narg == 1) then
        kx = tfeintf(tfsign, tfcsign, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
300    if (narg .ne. 1) then
        go to 6811
      end if
      kx%x(1) = merge(dble(kl%nl), 0.d0, ktflistq(k, kl))
      go to 8000
310    call tfdimensions(isp1, kx, irtc)
      go to 6900
320    kx = tfreplacepart(isp1, 0, irtc)
      go to 6900
330    if (narg == 1) then
        kx = tfeintf(dasin, tcasin, k, .true., -1.d0, 1.d0, irtc)
      else
        go to 6811
      end if
      go to 6900
340    if (narg == 1) then
        kx = tfeintf(dacos, tcacos, k, .true., -1.d0, 1.d0, irtc)
      else
        go to 6811
      end if
      go to 6900
350    if (narg == 1) then
        kx = tfeintf(tasinh, tcasinh, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
360    if (narg == 1) then
        kx = tfeintf(tacosh, tcacosh, k, .true., 1.d0, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
370    if (narg == 1) then
        kx = tfeintf(tatanh, tcatanh, k, .true., -1.d0, 1.d0, irtc)
      else
        go to 6811
      end if
      go to 6900
380    isp2 = isp
      kx = tftable(isp1, isp1 + 2, isp2, 29-id, irtc)
      go to 6900
400    call tfattributes(isp1, kx, irtc)
      go to 6900
410    if (narg == 2) then
        if (ktfnonrealq(dtastk(isp), f)) then
          go to 6812
        end if
      else if (narg .ne. 1) then
        go to 6811
      else
        f = 0.d0
      end if
      if (ktfnonrealq(k)) then
        irtc = itfmessage(9, 'General::wrongtype', '"Real number"')
      else
        ka = int8(rtastk(isp1 + 1))
        if (f == 0.d0 .and. .not. tfchecklastp(int(ka, kind=8))) then
          irtc = itfmessage(9, 'General::wrongnum', '"within allocated block"')
        else
          kx = kxadaloc(-1, 4, klx)
          klx%rbody(1) = rlist(ka)
          klx%rbody(2) = dble(klist(ka))
          klx%rbody(3) = dble(ilist(1, ka))
          klx%rbody(4) = dble(ilist(2, ka))
          klx%dbody(5) = kxsalocb(0, transfer(klist(ka), char8), 8)
          irtc = 0
        end if
      end if
      go to 6900
420    if (narg == 1) then
        kx = tfeintf(dabs, ccdabs, k, .false., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
430    call tfreverse(isp1, kx, irtc)
      go to 6900
440    kx = tfmodule(isp1, id == nfunmodule, .true., irtc)
      go to 6900
460    call tfstringreplace(isp1, kx, irtc)
      go to 6900
470    call tfswitchcases(isp1, kx, 0, irtc)
      go to 6900
480    call tfflatten(isp1, kx, irtc)
      go to 6900
490    call tfiff(isp1, kx, irtc)
      go to 6900
500    if (narg .ne. 2) then
        go to 6812
      else
        kx = tftake(dtastk(isp-1), k, .true., .true., irtc)
      end if
      go to 6900
510    call tfselect(isp1, kx, irtc)
      go to 6900
520    call tfwhile(isp1, kx, irtc)
      go to 6900
530    kx = tfjoin(isp1, .true., irtc)
      go to 6900
540    if (narg == 2) then
        kx = tfappend(dtastk(isp1 + 1), k, .true., 0, irtc)
      else if (narg .ne. 1) then
        go to 6812
      end if
      go to 6900
550    if (narg == 2) then
        kx = tfappend(dtastk(isp1 + 1), k, .true., 1, irtc)
      else if (narg .ne. 1) then
        go to 6812
      end if
      go to 6900
560    call tfclear(isp1, kx, irtc)
      go to 6900
570    call tfprotect(isp1, kx, .true., irtc)
      go to 6900
580    call tfprotect(isp1, kx, .false., irtc)
      go to 6900
590    if (narg .ne. 2) then
        go to 6812
      else
        kx = tftake(dtastk(isp-1), k, .false., .true., irtc)
      end if
      go to 6900
600    kx = tfreplacepart(isp1, 1, irtc)
      go to 6900
610    if (narg .ne. 4) then
        irtc = itfmessage(9, 'General::wrongnum', '"4"')
      else
        call tfinner(ktastk(isp-2), ktastk(isp-1), kx, k, ktastk(isp-3), irtc)
      end if
      go to 6900
620    if (narg .ne. 1) then
        go to 6811
      else
        call tftranspose(k, kx, irtc)
      end if
      go to 6900
630    call tfsingularvalues(isp1, kx, irtc)
      go to 6900
640    if (narg .ne. 1) then
        go to 6811
      else
        call tfdiagonalmatrix(k, kx, irtc)
      end if
      go to 6900
650    call tflinearsolve(isp1, kx, irtc)
      go to 6900
660    if (narg .ne. 1) then
        go to 6811
      else
        call tfidentitymatrix(k, kx, irtc)
      end if
      go to 6900
670    if (narg .ne. 1) then
        go to 6811
      else
        call tfeigensystem(k, kx, irtc)
      end if
      go to 6900
680    if (narg .ne. 1) then
        go to 6811
      else if (.not. ktfstringq(k)) then
        irtc = itfmessage(9, 'General::wrongtype', '"Character-string"')
      else
        ka = ktfaddr(k)
        nc = ilist(1, ka)
        if (nc == 0 .or. nc .gt. 3) then
          irtc = itfmessage(9, 'General::invop', ' ')
          go to 6900
        end if
        opcx = ' '
        call tmovb(ilist(1, ka + 1), opcx, nc)
        kax = itfopcode(opcx)
        if (kax .lt. 0) then
          irtc = itfmessage(9, 'General::invop', ' ')
        else
          kx%k = ktfoper + kax
          irtc = 0
        end if
      end if
      go to 6900
690    call tfposition(isp1, kx, 0, irtc)
      go to 6900
700    isp2 = isp
      kx = tftable(isp1, isp1 + 2, isp2, id-58, irtc)
      go to 6900
720    kx = tfrange(isp1, irtc)
      go to 6900
730    if (narg == 1) then
        kx = tfcmplxf(k, id-62, int(ka1), irtc)
      else
        go to 6811
      end if
      go to 6900
760    call tftostring(isp1, kx, id == 153, irtc)
      go to 6900
770    if (narg == 1) then
        kx = dfromr(dble(itfdepth(k)))
        go to 8000
      else
        go to 6811
      end if
780    if (narg == 2) then
        call tflevel(ktastk(isp1 + 1), k, kx, irtc)
      else
        go to 6812
      end if
      go to 6900
790    kx = tfwrite(isp1, irtc)
      go to 6900
800    if (narg .ne. 1) then
        go to 6811
      else
        kx = tfget(k, irtc)
      end if
      go to 6900
810    if (narg .ne. 1) then
        go to 6811
      else
        vx = itfopenwrite(k, irtc)
      end if
      go to 829
820    if (narg .ne. 1) then
        go to 6811
      else
        vx = itfopenappend(k, irtc)
      end if
829    if (irtc .ne. 0) then
        if (vx == -2.d0) then
          call tfaddmessage(' ', 0, lfno)
          irtc = 0
          kx%k = kxfailed
          return
        end if
        go to 6900
      end if
      kx = dfromr(dble(vx))
      go to 8000
830    if (narg .ne. 1) then
        go to 6811
      else
        call tfclosef(k, irtc)
      end if
      go to 8200
840    call tfflush(isp1, kx, irtc)
      go to 6900
850    call tfprintf(isp1, kx, irtc)
      go to 6900
860    call tfwritestring(isp1, kx, irtc)
      go to 6900
870    if (narg == 1) then
        call tfthrow(irtcret, k, irtc)
        return
      else
        go to 6811
      end if
      go to 6900
880    call tfhead(k, kx)
      go to 8100
890    if (narg == 1) then
        kx%k = merge(ktftrue, i00, tfreallistq(k))
        irtc = 0
      else
        irtc = itfmessage(9, 'General::narg', '"1"')
      end if
      go to 6900
900    call tfpartition(isp1, kx, irtc)
      go to 6900
910    if (narg == 1) then
        call tfthrow(irtcthrow, k, irtc)
        return
      else
        go to 6811
      end if
      go to 6900
920    call tfcatch(isp1, kx, irtc)
      go to 6900
930    call tfthread(isp1, kx, 0, irtc)
      go to 6900
940    call tfsetattributes(isp1, kx, irtc)
      go to 6900
950    kx = tfmap(isp1, 4, 1, irtc)
      go to 6900
960    call tffromcharactercode(isp1, kx, irtc)
      go to 6900
970    call tftocharactercode(isp1, kx, irtc)
      go to 6900
980    call tfcomplexlistqkf(isp1, kx, irtc)
      go to 6900
990    call tftr(isp1, kx, irtc)
      go to 6900
1000   call tfsavesharedmap()
      irtc = 0
      go to 6900
1010   call tfswitch(isp1, kx, irtc)
      go to 6900
1020   call tfsort(isp1, kx, 0, irtc)
      go to 6900
1030   call tfsort(isp1, kx, 1, irtc)
      go to 6900
1040   call tforder(isp1, kx, irtc)
      go to 6900
1050   call tfmemcheck(isp1, kx, irtc)
      go to 6900
1060   kx = tfmap(isp1, 1, 1, irtc)
      go to 6900
1070   if (narg == 1) then
        kx = k
        irtc = 0
        return
      else
        go to 6811
      end if
1080   if (narg == 1) then
        call cputime(vx, irtc)
        kx = dfromr(vx*1.d-6)
        go to 8000
      else
        go to 6811
      end if
      go to 6900
1090   if (narg == 1) then
        kx%k = merge(ktftrue, ktffalse, tfnumberq(k))
        go to 8000
      else
        go to 6811
      end if
      go to 6900
1100   call tfvectorqf(isp1, kx, irtc)
      go to 6900
1110   if (narg == 1) then
        kx%k = merge(merge(ktftrue, ktffalse, kl%head%k &
          == ktfoper + mtfcomplex), ktftrue, ktflistq(k, kl))
        go to 8000
      else
        go to 6811
      end if
      go to 6900
1120   call tfouter(isp1, kx, irtc)
      go to 6900
1130   call tfmatchqf(isp1, kx, irtc)
      go to 6900
1140   if (narg == 1) then
        if (ktflistq(k, kl)) then
          if (kl%head%k .ne. ktfoper + mtfcomp) then
            call tfprint1(k, 6, -itfgetrecl(), 4, .true., .true., irtc)
          end if
        else
          call tfprint1(k, 6, -itfgetrecl(), 4, .true., .true., irtc)
        end if
        ltr0 = ltrace
        ltrace = 6
        kx = tfeevalref(k, irtc)
        ltrace = ltr0
      else
        go to 6811
      end if
      go to 6900
1150   call tfdefinition(isp1, kx, irtc)
      go to 6900
1160   if (narg .ne. 1) then
        go to 6811
      else
        call nfread(k, kx, irtc)
      end if
      go to 6900
1170   call tfintersection(isp1, kx, 0, irtc)
      go to 6900
1180   call tfintersection(isp1, kx, 1, irtc)
      go to 6900
1190   if (narg == 1) then
        kx = tfeintf(tround, tcround, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
1200   if (narg == 1) then
        kx = tfeintf(inverseerf, cinverseerf, k, .true., -1.d0, 1.d0, irtc)
      else
        go to 6811
      end if
      go to 6900
1210   irtc = itfmessage(999, 'General::unregister', ' ')
      go to 6900
1220   if (narg == 1) then
        kx = tfeintf(polygamma, cpolygamma, k, .true., -dinfinity, dinfinity, irtc)
      else if (narg == 2) then
        kx = tfbessel(isp1, 4, irtc)
      else
        go to 6812
      end if
      go to 6900
1230   call tftoinputstring(isp1, kx, irtc)
      go to 6900
1240   call tfreadstring(isp1, kx, .false., .false., irtc)
      go to 6900
1250   call tfopenread(isp1, kx, irtc)
      go to 6900
1260   call tftoexpression(isp1, kx, irtc)
      go to 6900
1270   call tfstringmatchq(isp1, kx, irtc)
      go to 6900
1280   call tfstringposition(isp1, kx, irtc)
      go to 6900
1290   call tftouppercase(isp1, kx, id-119, irtc)
      go to 6900
1300   continue
1310   call tfbreak(id-123, narg, kx, irtc)
      go to 6900
1320   if (narg == 1) then
        call tfthrow(irtcgoto, k, irtc)
        return
      else
        go to 6811
      end if
      go to 6900
1330   continue
1340   if (narg == 1) then
        call tffourier(id == 124, k, kx, irtc)
      else
        go to 6811
      end if
      go to 6900
1350   kx = tfcheck(isp1, irtc)
      go to 6900
1360   call tfwhich(isp1, kx, irtc)
      go to 6900
1370   kx = tfmapfile(isp1, irtc)
      go to 6900
1380   kx = tfunmapfile(isp1, irtc)
      go to 6900
1390   irtc = itfmessage(999, 'General::unregister', ' ')
      go to 6900
1420   kx = tfsequence(isp1, isp)
      irtc = 0
      return
1430   call tfcases(isp1, kx, irtc)
      go to 6900
1440   call tfposition(isp1, kx, 2, irtc)
      go to 6900
1450   call tfvectorize(isp1, kx, irtc)
      go to 6900
1460   if (narg == 1) then
        kx = tfeintf(aloggamma, cloggamma, k, .true., 0.d0, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
1470   call tfnames(isp1, kx, irtc)
      go to 6900
1480   call tfgarbagecollect(isp1, kx, irtc)
      go to 6900
1490   if (narg == 1) then
        kx = tfeintf(aloggamma1, cloggamma1, k, .true., 0.d0, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
1500   if (narg == 1) then
        kx = tfeintf(factorial, cfactorial, k, .true., -1.d0 + 5.56d-17, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
1510   call tfwith(isp1, kx, .true., irtc)
      go to 6900
1520   call tfswitchcases(isp1, kx, 1, irtc)
      go to 6900
1530   kx = tfoverride(isp1, irtc)
      go to 6900
1540   call tfappendto(isp1, kx, id-143, irtc)
      go to 6900
1560   call tffindroot(isp1, kx, irtc)
      go to 6900
1570   kx = tfbessel(isp1, 9, irtc)
      go to 6900
1580   kx = tfbessel(isp1, 10, irtc)
      go to 6900
1590   if (narg == 1) then
        kx = tfeintf(ferf, cerf, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
1600   if (narg == 1) then
        kx = tfeintf(ferfc, cerfc, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
1610   call tffit(isp1, kx, irtc)
      go to 6900
1620   call tfsymbol(isp1, kx, irtc)
      go to 6900
1640   call tfextract(isp1, kx, irtc)
      go to 6900
1650   call tfread(isp1, kx, irtc)
      go to 6900
1660   call tfskip(isp1, kx, irtc)
      go to 6900
1670   kx = tftemporaryname(isp1, irtc)
      go to 6900
1680   call tfexit(isp1, kx, irtc)
      go to 6900
1690   call tfstringfill(isp1, kx, irtc)
      go to 6900
1700   call tfrestrict(isp1, kx, irtc)
      go to 6900
1710   kx = tfminmax(isp1, 0, irtc)
      go to 6900
1720   call tfshort(isp1, kx, irtc)
      go to 6900
1730   call setompnumthreads(isp1, kx, irtc)
      go to 6900
1740   irtc = itfmessage(999, 'General::unregister', ' ')
      go to 6900
1750   irtc = itfmessage(999, 'General::unregister', ' ')
      go to 6900
1760   irtc = itfmessage(999, 'General::unregister', ' ')
      go to 6900
1770   kx = tfbessel(isp1, id-167, irtc)
      go to 6900
1810   call tfbaseform(isp1, kx, irtc)
      go to 6900
1820   call tfstringtrim(isp1, kx, irtc)
      go to 6900
1830   call tfstringtostream(isp1, kx, irtc)
      go to 6900
1840   if (narg == 1) then
        kx = tfeintf(tfevenq, tfcevenq, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
1850   if (narg == 1) then
        kx = tfeintf(tfoddq, tfcoddq, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
1860   irtc = itfmessage(999, 'General::unregister', ' ')
      go to 6900
1870   kx = tfreplacepart(isp1, id-175, irtc)
      go to 6900
1900   if (narg .ne. 2) then
        go to 6812
      end if
      call tfreplace(dtastk(isp1 + 1), dtastk(isp), kx, .false., .true., .false., irtc)
      go to 6900
1910   irtc = itfmessage(999, 'General::unregister', ' ')
      go to 6900
1920   call tfspline(isp1, kx, irtc)
      go to 6900
1930   call tffindindex(isp1, kx, irtc)
      go to 6900
1940   call tfsetcontext(isp1, kx, irtc)
      go to 6900
1950   call tfsetcontextpath(isp1, kx, irtc)
      go to 6900
1960   call tftocontext(isp1, kx, irtc)
      go to 6900
1970   call tfcontext(isp1, kx, irtc)
      go to 6900
1980   call tfmod(isp1, kx, id-187, irtc)
      go to 6900
2010   call tfreplacemember(isp1, kx, irtc)
      go to 6900
2020   call tfmemberscan(isp1, kx, irtc)
      go to 6900
2030   call tfstandardform(isp1, kx, irtc)
      go to 6900
2040   irtc = -7
      if (narg == 1) then
        if (ktfrealq(k, v)) then
          irtc = int(min(max(-3.d0, v), -1.d0)-3.d0)
        end if
      end if
      go to 6900
2050   go to 6900
2060   call tfreleasehold(isp1, kx, irtc)
      go to 6900
2070   call tfnanqk(isp1, kx, irtc)
      go to 6900
2080   call tfthread(isp1, kx, id-197, irtc)
      go to 6900
2100   call tffirst(isp1, kx, id-201, irtc)
      go to 6900
2140   call tfobjectsymbol(isp1, kx, irtc)
      go to 6900
2150   if (narg == 1) then
        kx = tfeintf(productlog, cproductlog, k, .true., -exp(-1.d0), dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
2160   kx = tfgaussiancoulomb(isp1, irtc)
      go to 6900
2170   call tfclearmemberobject(isp1, kx, irtc)
      go to 6900
2180   call tfmalloc(isp1, kx, irtc)
      go to 6900
2190   if (narg == 1) then
        kx = k
        if (ktflistq(k)) then
          kx = kxcopylist(k)
        end if
        irtc = 0
        return
      else
        go to 6811
      end if
2200   kx = tfgetcommandline(isp1, irtc)
      go to 6900
2210   call tfseek(isp1, kx, irtc)
      go to 6900
2220   call tfdigitQ(isp1, kx, irtc)
      go to 6900
2230   call tfletterQ(isp1, kx, irtc)
      go to 6900
2240   call tfrealqk(isp1, kx, irtc)
      go to 6900
2250   if (narg .ne. 4) then
        go to 6814
      end if
      if (ktfnonrealq(dtastk(isp-1)) .or. ktfnonrealq(dtastk(isp))) then
        irtc = itfmessage(9, 'General::wrongarg', '"$NearlySameQ[a, b, relthre, absthre]"')
        go to 7000
      end if
      irtc = 0
      kx%k = merge(ktftrue, ktffalse, tfnearlysameqf(dtastk(isp1 + 1), &
          dtastk(isp1 + 2), rtastk(isp1 + 3), rtastk(isp)))
      go to 6900
2260   kx = tfopenshared(isp1, irtc)
      go to 6900
2270   kx = tfreadshared(isp1, irtc)
      go to 6900
2280   kx = tfwriteshared(isp1, irtc)
      go to 6900
2290   if (narg .ne. 1) then
        irtc = itfmessage(9, 'General::narg', '"1"')
        go to 7000
      end if
      isp0 = isp1 + 1
      call tfsharedsize(isp0, k, nsize, irtc)
      isp = isp1 + 1
      if (irtc .ne. 0) then
        go to 7000
      end if
      kx = dfromr(dble(max(0, nsize-2)*4))
      go to 6900
2300   if (narg .ne. 1) then
         irtc = itfmessage(9, 'General::narg', '"1"')
         go to 7000
      end if
      irtc = 0
      kx%k = merge(ktftrue, ktffalse, ktfoperq(k))
      go to 6900
2310   kx = tfgaussiancoulombu(isp1, irtc)
      go to 6900
2320   call tfgaussiancoulombfitted(isp1, kx, irtc)
      go to 6900
2330   call tfrest(isp1, kx, irtc)
      go to 6900
2340   call tfrotateright1(isp1, kx, irtc)
      go to 6900
2350   call tfdifference(isp1, kx, irtc)
      go to 6900
2360   if (narg == 1) then
        kx = tfeintf(gamma0, cgamma0, k, .false., 0.d0, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
2370   if (narg == 1) then
        kx = tfeintf(xsin, tcxsin, k, .true., -dinfinity, dinfinity, irtc)
      else
        go to 6811
      end if
      go to 6900
2380   if (narg == 2) then
        kx = tfbessel(isp1, 12, irtc)
      else
        go to 6812
      end if
      go to 6900
2390   if (narg == 4) then
        kx = tfhg(isp1, irtc)
      else
        go to 6814
      end if
      go to 6900
2400   if (narg == 4) then
        isp = isp + 1
        kx = tfhg(isp1, irtc)
      else
        go to 6814
      end if
      go to 6900
2410   if (narg == 3) then
        kx = tfhg(isp1, irtc)
      else
        go to 6813
      end if
      go to 6900
2420   if (narg == 3) then
        dtastk(isp + 1) = dxnullo
        isp = isp + 2
        kx = tfhg(isp1, irtc)
      else
        go to 6813
      end if
      go to 6900
2430   if (narg == 2) then
        kx = tfhg(isp1, irtc)
      else
        go to 6812
      end if
      go to 6900
2440   if (narg == 2) then
        dtastk(isp + 1) = dxnullo
        isp = isp + 3
        kx = tfhg(isp1, irtc)
      else
        go to 6812
      end if
      go to 6900
2450   if (narg == 1) then
        kx = tfeintf(zeta, czeta, k, .true., -dinfinity, dinfinity, irtc)
      else if (narg == 2) then
        kx = tfeintf2(zeta2, czeta2, dtastk(isp-1), k, .true., irtc)
      else
        go to 6812
      end if
      go to 6900
2460   if (narg == 1) then
        kx = tfeintf(polygamma, cpolygamma, k, .true., -dinfinity, dinfinity, irtc)
      else if (narg == 2) then
        kx = tfbessel(isp1, 5, irtc)
      else
        go to 6812
      end if
      go to 6900
2470   if (narg == 1) then
        kx = tfeintf(dzeta, dczeta, k, .true., -dinfinity, dinfinity, irtc)
      else if (narg == 2) then
        kx = tfeintf2(dzeta2, dczeta2, dtastk(isp-1), k, .true., irtc)
      else
        go to 6812
      end if
      go to 6900
2480   if (narg == 1) then
        kx = tfeintf(zeta, czeta, k, .true., -dinfinity, dinfinity, irtc)
      else if (narg == 2) then
        kx = tfbessel(isp1, 6, irtc)
      else
        go to 6812
      end if
      go to 6900
2490   if (narg == 1) then
        kx = tfeintf(dzeta, dczeta, k, .true., -dinfinity, dinfinity, irtc)
      else if (narg == 2) then
        kx = tfbessel(isp1, 7, irtc)
      else
        go to 6812
      end if
      go to 6900
2500   kx = tfbessel(isp1, 8, irtc)
      go to 6900
2510   if (narg == 1) then
        kx = tfeintf(rgamma, cgamma, k, .true., -dinfinity, dinfinity, irtc)
      else if (narg == 2) then
        kx = tfbessel(isp1, 11, irtc)
      else
        go to 6812
      end if
      go to 6900
2520   if (narg == 3) then
        kx = kxhg(dtastk(isp1 + 1), dxnullo, dtastk(isp1 + 2), dtastk(isp), .true., 3, irtc)
      else
        go to 6812
      end if
      go to 6900
2530   if (narg == 3) then
        kx = kxhgpq(isp1, .false., irtc)
      else
        go to 6812
      end if
      go to 6900
2540   if (narg == 3) then
        kx = kxhgpq(isp1, .true., irtc)
      else
        go to 6812
      end if
      go to 6900
2550   if (narg == 2) then
        kx = tfbessel(isp1, 13, irtc)
      else
        go to 6812
      end if
      go to 6900
2560   if (narg == 2) then
        kx = tfbessel(isp1, 14, irtc)
      else
        go to 6812
      end if
      go to 6900
2570   if (narg == 3) then
        kx = kxhg(dtastk(isp1 + 2), dtastk(isp), dtastk(isp), dtastk(isp1 + 1), .false., 4, irtc)
      else
        go to 6812
      end if
      go to 6900
2580   if (narg == 3) then
        kx = kxhg(dtastk(isp1 + 2), dtastk(isp), dtastk(isp), dtastk(isp1 + 1), .false., 5, irtc)
      else
        go to 6812
      end if
      go to 6900
2590   if (narg == 1) then
        if (ktfrealq(dtastk(isp), v) .and. anint(v) == v) then
          kx%x(1) = bernbf(nint(v))*factorial(dble(v))
          irtc = 0
        end if
      else if (narg == 2) then
        irtc = -1
        if (ktfrealq(dtastk(isp1 + 1), v) .and. anint(v) == v &
          .and. tfnumberq(dtastk(isp), c1)) then
          kx = kxcalocc(-1, berpol(nint(v), c1))
          irtc = 0
        end if
      else
        go to 6812
      end if
      go to 6900
8000   continue
8100   irtc = 0
      return
8200   if (irtc .ne. 0) then
        go to 6900
      end if
      kx%k = ktfoper + mtfnull
      return
6000   iaf = int(ka1)
!        go to (
!     $       6010, 
!     $       7000, 7000, 6600, 7000, 6600, 7000, 6620, 6630, 6690, 6690, 
!     $       6680, 6680, 6680, 6680, 6700, 6700, 6610, 6100, 6100, 6510, 
!     $       7000, 7000, 6010, 7000, 6640, 6650, 6750, 7000, 7000, 7000, 
!     $       7000, 6002, 6010, 6010, 6010, 6660, 6670, 6560, 6560, 6710, 
!     $       6010, 6740, 7000, 7000, 6200, 6090, 6520, 6530, 6540, 6010, 
!     $       6010, 6550, 6570, 6570, 6570, 6570, 6580, 6580, 6590, 6720, 
!     $       6730, 6010, 7000, 7000, 6010, 7000), 
!     $       iaf + 1
!            null
!            m    i     +     -    *    /    v    ^    e    n    
!            >    <    g    l    E    N    ~    &&   o    c
!            [    ]    {    }    s     =    
!            ;    &    :    r    d    RepA RepR u    U    S    
!            ?    f    #    ##   .    |    M    MA   A    rept 
!            repn ineq AT   SF   TB   DB   INc  Dec  Part @
!            msgn TagS (*   *)   Hold z
!        go to 6001
      select case(iaf)
      case (mtfnull, mtflist, mtfcolon, mtfhold, mtftagset, mtfrepeated, &
              mtfrepeatednull, mtfpattest, mtfrule, mtfruledelayed)
        kx = kxcompose(isp1)
        irtc = 0
        return
      case (mtfset)
        kx = tfset(isp1, .true., irtc)
        go to 6900
      case (mtfplus, mtftimes)
        kx = tfplus(isp1, iaf, irtc)
        go to 6900
      case (mtfrevpower)
        kx = tfrevpower(isp1, irtc)
        go to 6900
      case (mtfpower)
        kx = tfpower(isp1, irtc)
        go to 6900
      case (mtfgreater, mtfless, mtfgeq, mtfleq)
        kx = tfrelation(isp1, iaf, irtc)
        go to 6900
      case (mtfinequality)
        call tfinequality(isp1, kx, irtc)
        go to 6900
      case (mtfpart)
        if (ktflistq(ktastk(isp1 + 1))) then
          kx = tfpart(isp1 + 1, .true., irtc)
          if (irtc == 0) then
            kx = tfeevalref(kx, irtc)
          end if
        else if (ktfsymbolq(ktastk(isp1 + 1))) then
          irtc = -1
        else
          irtc = itfmessage(9, 'General::wrongtype', '"List or composition"')
        end if
        go to 6900
      case (mtfnot)
        kx = tfnot(isp1, iaf, irtc)
        go to 6900
      case (mtfupset, mtfupsetdelayed)
        if (narg .ne. 2) then
          go to 6812
        end if
        kx = tfupset(dtastk(isp1 + 1), dtastk(isp), i00, irtc)
        go to 6900
      case (mtffun)
        if (isp == isp1 + 2) then
          if (ktastk(isp) == ktfoper + mtfnull) then
            kx = kxpfaloc(dtastk(isp-1))
            irtc = 0
            return
          end if
        end if
      case (mtfalt, mtfand, mtfor)
        if (narg .lt. 2 .and. iaf == mtfalt) then
          irtc = -1
          go to 6900
        end if
        if (narg == 2) then
          kx = tfeval1(dtastk(isp1 + 1), dtastk(isp), iaf, irtc)
          go to 6900
        end if
        if (narg == 0) then
          kx = dfromr(dble(mtfor-iaf))
          irtc = 0
          return
        end if
        kx = dtastk(isp1 + 1)
        if (narg == 1) then
          irtc = 0
          return
        end if
        do i = isp1 + 2, isp
          k1 = kx
          kx = tfeval1(k1, dtastk(i), iaf, irtc)
          if (irtc .ne. 0) then
            go to 6900
          end if
        end do
        return
      case (mtfdot)
        if (narg == 2) then
          kx = tfeval1(dtastk(isp1 + 1), dtastk(isp), iaf, irtc)
          go to 6900
        end if
        if (narg == 0) then
          irtc = itfmessage(9, 'General::narg', '"1 or more"')
          go to 6900
        end if
        kx = dtastk(isp)
        if (narg == 1) then
          irtc = 0
          return
        end if
        do i = isp-1, isp1 + 1, -1
          k1 = kx
          kx = tfeval1(dtastk(i), k1, iaf, irtc)
          if (irtc .ne. 0) then
            go to 6900
          end if
        end do
        return
      case (mtfconcat)
        call tfstringjoin(isp1, kx, irtc)
        go to 6900
      case (mtfmap)
        kx = tfmap(isp1, 3, 1, irtc)
        go to 6900
      case (mtfmapall)
        call tfmapall(isp1, kx, irtc)
        go to 6900
      case (mtfapply)
        call tfapply(isp1, kx, irtc)
        go to 6900
      case (mtfaddto, mtfsubtractfrom, mtftimesby, mtfdivideby)
        if (narg .ne. 2) then
          go to 6812
        end if
        kx = tfeval1to(dtastk(isp1 + 1), dtastk(isp), iaf, .false., irtc)
        go to 6900
      case (mtfincrement, mtfdecrement)
        v1 = merge(1.d0, -1.d0, iaf == mtfincrement)
        if (narg == 1) then
          kx = tfeval1to(dtastk(isp1 + 1), dfromr(dble(v1)), mtfaddto, .true., irtc)
        else if (narg == 2 .and. ktastk(isp1 + 1) == ktfoper + mtfnull) then
          kx = tfeval1to(dtastk(isp), dfromr(dble(v1)), mtfaddto, .false., irtc)
        else
          go to 6811
        end if
        go to 6900
      case (mtfsetdelayed)
        if (narg .ne. 2) then
          go to 6812
        end if
        kx = tfset(isp1, .true., irtc)
        go to 6900
      case (mtfreplace)
        kx = tfreplace1(isp1, irtc)
        go to 6900
      case (mtfreplacerepeated)
        kx = tfreplacerepeated1(isp1, irtc)
        go to 6900
      case (mtfequal, mtfunequal)
        kx = tfequal(isp1, iaf, irtc)
        go to 6900
      case (mtfsame, mtfunsame)
        kx = tfsameq1(isp1, iaf, irtc)
        go to 6900
      case (mtfunset)
        if (narg .ne. 1) then
          go to 6811
        end if
        isp = isp1 + 2
        ktastk(isp) = ktfref
        kx = tfset(isp1, .true., irtc)
        isp = isp1 + 1
        kx%k = ktfoper + mtfnull
        go to 6900
      case (mtfatt)
        call tfatt(isp1, kx, .true., irtc)
        go to 6900
      case (mtfmessagename)
        km = klist(ifunbase + mtfmessagename)
        call tfdeval(isp1, km, kx, 1, .false., euv, irtc)
        go to 6900
      case (mtfflag)
        call tfflagordef(isp1, kx, irtc)
        go to 6900
      case (mtfcomplex)
        if (narg .ne. 2) then
          go to 6812
        end if
        kx = tfeval1(dtastk(isp1 + 1), dtastk(isp), mtfcomplex, irtc)
        go to 6900
      case default
        if (iaf .le. mtfend) then
          go to 7000
        end if
        irtc = itfmessage(999, 'General::invop', ' ')
        return
      end select
    else if (ktfsymbolqdef(k1%k, symd)) then
      if (symd%sym%override .ne. 0) then
        if (symd%downval .ne. 0) then
          call tfdeval(isp1, ktfaddr(k1), kx, 1, .false., euv, irtc)
          go to 6900
        else
          go to 6800
        end if
      else
        go to 6800
      end if
    else if (ktflistq(k1, kl1)) then
      kx = k1
      if (ktfoperq(kl1%head, kop)) then
        id = iget_fun_id(int(kop, kind=8))
        select case (id)
        case (-mtffun)
          kx = tfpuref(isp1, kl1, irtc)
          go to 6900
        case (-mtfnull)
          if (kl1%nl == 0) then
            kx = tfsequence(isp1, isp)
            if (ktflistq(kx, klx)) then
              kx = tfleval(klx, .true., irtc)
            else if (ktfsymbolq(kx) .or. ktfpatq(kx)) then
              kx = tfeevalref(kx, irtc)
            end if
            go to 6900
          else if (kl1%nl == 1 .and. ktfnonreallistqo(kl1)) then
            kx = kxmakelist(isp1, klx)
            kh = klx%dbody(1)
            klx%head = dtfcopy(kh)
            irtc = 0
            return
          end if
        case (-mtflist)
          kx = tfpart(isp1, .true., irtc)
          if (irtc == 0) then
            if (ktflistq(kx, klx)) then
              kx = tfleval(klx, .true., irtc)
            else if (ktfsymbolq(kx) .or. ktfpatq(kx)) then
              kx = tfeevalref(kx, irtc)
            end if
          end if
          go to 6900
        case (-mtfmap, -mtfapply)
          if (kl1%nl == 1) then
            isp2 = isp + 1
            dtastk(isp2) = kl1%head
            dtastk(isp2 + 1) = kl1%dbody(1)
            dtastk(isp2 + 2:isp2 + isp-isp1 + 1) = dtastk(isp1 + 1:isp)
            isp = isp + isp2-isp1 + 1
            kx = tfefunref(isp2, upvalue, irtc)
            isp = isp2
            go to 6900
          end if
        case (nfunappend, nfunprepend, nfuncases, nfundelcases, nfunselcases, &
              nfundelete, nfunposition, nfunselect, nfunreppart, nfunextract, nfunswicases)
          if (kl1%nl == 1) then
            isp2 = isp + 1
            dtastk(isp2) = kl1%head
            dtastk(isp2 + 1) = dtastk(isp1 + 1)
            dtastk(isp2 + 2) = kl1%dbody(1)
            if (isp .gt. isp1 + 1) then
              dtastk(isp2 + 3:isp2 + isp-isp1 + 1) = dtastk(isp1 + 2:isp)
            end if
            isp = isp2 + isp-isp1 + 1
            kx = tfefunref(isp2, upvalue, irtc)
            isp = isp2
            go to 6900
          end if
        case (nfuninsert)
          if (kl1%nl == 2) then
            isp2 = isp + 1
            dtastk(isp2) = kl1%head
            dtastk(isp2 + 1) = dtastk(isp1 + 1)
            dtastk(isp2 + 2) = kl1%dbody(1)
            dtastk(isp2 + 3) = kl1%dbody(2)
            isp = isp2 + 3
            kx = tfefunref(isp2, upvalue, irtc)
            isp = isp2
            go to 6900
          end if
        end select
        go to 6800
      end if
      do while(ktflistq(kx, klx))
        kx = klx%head
      end do
      if (ktfsymbolqdef(kx%k, symd)) then
        if (symd%sym%override .ne. 0 .and. symd%downval .ne. 0) then
          call tfdeval(isp1, ktfaddrd(kx), kx, 1, .false., euv, irtc)
          go to 6900
        end if
      end if
      go to 6800
    else if (ktfstringq(k1)) then
      if (narg .le. 0 .or. narg .gt. 2) then
        go to 6800
      else if (ktfnonrealq(ktastk(isp)) .or. ktfnonrealq(ktastk(isp1 + 1))) then
        go to 6800
      end if
      kx = kxsubstring(k1, isp1 + 1, isp)
      irtc = 0
      return
    else
      go to 6800
    end if
    go to 6900
6800 irtc = 0
    go to 7000
6811 irtc = itfmessage(9, 'General::narg', '"1"')
    go to 7000
6812 irtc = itfmessage(9, 'General::narg', '"2"')
    go to 7000
6813 irtc = itfmessage(9, 'General::narg', '"3"')
    go to 7000
6814 irtc = itfmessage(9, 'General::narg', '"4"')
    go to 7000
6900 if (irtc == 0 .or. irtc .lt. -1) then
      return
    else if (irtc == -1) then
      irtc = 0
    end if
7000 isp = isp1 + narg
    kx = kxcrelistm(narg, ktastk(isp1 + 1:isp), dtastk(isp1))
    if (irtc .gt. 0) then
      if (ierrorprint .ne. 0) then
        call tferrorhandle(kx, irtc)
      else
        call tfdebugprint(kx, '... in', 3)
      end if
    else if (irtc == -1) then
      irtc = 0
    end if
    return
    end

    function tfcheck(isp1, irtc) result(kx)
    use tfstk
    use eeval
    use tfcsi
    implicit none
    type (sad_descriptor) kx, kf, kxcheckmessage, kxmessagelist
    type (sad_symdef),  pointer, save :: symd
    integer(kind=4), intent(in):: isp1
    integer(kind=4), intent(out):: irtc
    integer(kind=4) isp0, itgetfpe, itfmessage, narg, irtc1
    data kxmessagelist%k, kxcheckmessage%k /0, 0/
    narg = isp-isp1
    if (narg .lt. 2) then
      kx = dxnullo
      irtc = itfmessage(9, 'General::narg', '"2 or more"')
      return
    end if
    isp0 = isp
    rlist(ierrorgen) = 0.d0
    kx = tfeevalref(dtastk(isp1 + 1), irtc)
    isp = isp0
    if (irtc == 0) then
      if (itgetfpe() .ne. 0) then
        call tclrfpe
        irtc = itfmessage(9, 'General::fpe', '""')
      end if
    end if
    if (irtc .gt. 0) then
      if (ierrorprint .ne. 0) then
        call tfaddmessage(' ', 0, icslfno())
      end if
    else if (irtc == irtcabort .and. rlist(ierrorgen) == 0.d0) then
      irtc = itfmessage(999, 'General::abort', ' ')
    else if (irtc .le. irtcret) then
      rlist(ierrorgen) = 1.d0
    end if
    if (rlist(ierrorgen) .ne. 0.d0) then
      if (irtc .le. irtcret) then
        call tfcatchreturn(modethrow, kx, irtc)
      end if
      if (narg .gt. 2) then
        if (kxcheckmessage%k == 0) then
          kxcheckmessage = kxsymbolz('Check$Message', 13)
        end if
        dtastk(isp1 + 1) = kxcheckmessage
        kf = tfefunref(isp1 + 1, .false., irtc1)
        if (irtc1 == 0 .and. ktfrealq(kf) .and. kf%k .ne. 0) then
          rlist(ierrorgen) = 0.d0
          kx = tfeevalref(dtastk(isp1 + 2), irtc)
        end if
      else
        if (kxmessagelist%k == 0) then
          kxmessagelist = kxsymbolz('$MessageList', 12)
          call descr_sad(kxmessagelist, symd)
        end if
        call tflocald(symd%value)
        symd%value = dtfcopy1(dxnulll)
        rlist(ierrorgen) = 0.d0
        kx = tfeevalref(dtastk(isp1 + 2), irtc)
      end if
    end if
    return
    end

    function tfjoin(isp1, eval, irtc) result(kx)
    use tfstk
    implicit none
    type (sad_descriptor) kx
    type (sad_descriptor) kf
    type (sad_dlist),  pointer :: kl1, kli
    integer(kind=4), intent(in):: isp1
    integer(kind=4), intent(out):: irtc
    integer(kind=4) itfmessage, i, narg, isp0
    logical(kind=4), intent(in):: eval
    logical(kind=4) ev
    narg = isp-isp1
    if (ktfnonlistq(ktastk(isp1 + 1), kl1)) then
      go to 9010
    end if
    if (narg .le. 1) then
      if (narg == 1) then
        kx = dtastk(isp1 + 1)
        irtc = 0
      else
        kx = dxnullo
        irtc = itfmessage(9, 'General::narg', '"1 or more"')
      end if
      return
    end if
    kf = kl1%head
    isp = isp + 1
    isp0 = isp
    call tfgetllstkall(kl1)
    if (isp .ge. mstk) then
      kx = dxnullo
      irtc = itfmessage(9999, 'General::stack', '"Join"')
      return
    end if
    do i = isp1 + 2, isp0-1
      if (ktfnonlistq(ktastk(i), kli)) then
        isp = isp0-1
        go to 9000
      end if
      if (.not. tfsameq(kli%head, kf)) then
        go to 9100
      end if
      call tfgetllstkall(kli)
      if (isp .ge. mstk) then
        kx = dxnullo
        irtc = itfmessage(9999, 'General::stack', '"Join"')
        return
      end if
    end do
    dtastk(isp0) = kf
    ev = eval
    if (ev .and. (kf%k == ktfoper + mtflist .or. kf%k == ktfoper + mtfalt &
      .or. kf%k == ktfoper + mtfnull)) then
        ev = .false.
    end if
    if (ev) then
      kx = tfefunref(isp0, .true., irtc)
    else
      kx = kxcompose(isp0)
      irtc = 0
    end if
    isp = isp0-1
    return
9000 isp = isp0-1
9010 irtc = itfmessage(9, 'General::wrongtype', '"List or composition for all args"')
    kx = dxnullo
    return
9100 irtc = itfmessage(9, 'General::samehead', ' ')
    kx = dxnullo
    isp = isp0-1
    return
    end

    function tfbessel(isp1, mode, irtc) result(kx)
    use tfstk
    use gammaf
    implicit none
    type (sad_descriptor) kx
    integer(kind=4), intent(in):: isp1, mode
    integer(kind=4), intent(out):: irtc
    integer(kind=4) itfmessage
    if (isp .ne. isp1 + 2) then
      irtc = itfmessage(9, 'General::narg', '"2"')
      kx = dxnullo
      return
    end if
    select case (mode)
    case (0)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, cbesj, irtc)
    case (1)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, cbesy, irtc)
    case (2)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, cbesi, irtc)
    case (3)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, cbesk, irtc)
    case (4)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, cgpolygamma2, irtc)
    case (5)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, cpolygamma2, irtc)
    case (6)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, chzeta2, irtc)
    case (7)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, dchzeta2, irtc)
    case (8)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, chpolygamma2, irtc)
    case (9)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, cgammaq, irtc)
    case (10)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, cgammap, irtc)
    case (11)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, cgamma2, irtc)
    case (12)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, cpochh, irtc)
    case (13)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, cpolylog, irtc)
    case (14)
      call tfbesself(dtastk(isp1 + 1), dtastk(isp), kx, cbeta2, irtc)
    end select
    return
    end

    recursive subroutine tfbesself(k1, k2, kx, cfun, irtc)
    use tfstk
    implicit none
    type (sad_descriptor), intent(in):: k1, k2
    type (sad_descriptor), intent(out):: kx
    type (sad_dlist),  pointer :: kl1, kl2
    integer(kind=4), intent(out):: irtc
    integer(kind=4) itfmessage, n1, n2, isp0, isp2, i
    real(kind=4) x1, x2
    complex(kind=16) cx, c1, c2
    complex(kind=16), external:: cfun
    if (tfcomplexnumlistqk(k1%k, kl1)) then
      n1 = kl1%nl
      if (tfcomplexnumlistqk(k2%k, kl2)) then
        if (n1 .ne. kl2%nl) then
          irtc = itfmessage(9, 'General::equalleng', '"#1 and #2"')
          return
        end if
        isp0 = isp
        call tfgetllstkall(kl1)
        call tfgetllstkall(kl2)
        isp2 = isp
        do i = 1, n1
          isp = isp + 1
          call tfbesself(dtastk(isp0 + i), dtastk(isp0 + n1 + i), dtastk(isp), cfun, irtc)
          if (irtc .ne. 0) then
            isp = isp0
            return
          end if
        end do
        kx = kxmakelist(isp2)
        isp = isp0
      else if (tfnumberq(k2)) then
        isp0 = isp
        call tfgetllstkall(kl1)
        isp2 = isp
        do i = 1, n1
          isp = isp + 1
          call tfbesself(dtastk(isp0 + i), k2, dtastk(isp), cfun, irtc)
          if (irtc .ne. 0) then
            isp = isp0
            return
          end if
        end do
        kx = kxmakelist(isp2)
        isp = isp0
      else
        irtc = -1
        return
      end if
    else if (tfcomplexnumlistqk(k2%k, kl2)) then
      n2 = kl2%nl
      isp0 = isp
      call tfgetllstkall(kl2)
      isp2 = isp
      do i = 1, n2
        isp = isp + 1
        call tfbesself(k1, dtastk(isp0 + i), dtastk(isp), cfun, irtc)
        if (irtc .ne. 0) then
          isp = isp0
          return
        end if
      end do
      kx = kxmakelist(isp2)
      isp = isp0
    else if (ktfrealq(k1, x1)) then
      if (ktfrealq(k2, x2)) then
        cx = cfun(dcmplx(x1, 0.d0), dcmplx(x2, 0.d0))
      else if (tfcomplexq(k2, c2)) then
        cx = cfun(dcmplx(x1, 0.d0), c2)
      else
        irtc = -1
        return
      end if
      go to 10
    else if (tfcomplexq(k1, c1)) then
      if (ktfrealq(k2, x2)) then
        cx = cfun(c1, dcmplx(x2, 0.d0))
      else if (tfcomplexq(k2, c2)) then
        cx = cfun(c1, c2)
      else
        irtc = -1
        return
      end if
      go to 10
    else
      irtc = -1
      return
    end if
    irtc = 0
    return
10   kx = kxcalocc(-1, cx)
    irtc = 0
    return
    end

    end module efun

    subroutine tfefun(isp1, kx, ref, upvalue, irtc)
    use tfstk
    use efun
    implicit none
    type (sad_descriptor) kx
    integer(kind=4) isp1, irtc
    logical(kind=4) ref, upvalue
    if (ref) then
      kx = tfefunref(isp1, upvalue, irtc)
    else
      call tfefundef(isp1, kx, irtc)
    end if
    return
    end

    subroutine tfefunrefc(isp1, kx, irtc)
    use tfstk
    use efun
    implicit none
    type (sad_descriptor) kx
    integer(kind=4) isp1, irtc
    levele = levele + 1
    kx = tfefunref(isp1, .true., irtc)
    call tfconnect(kx, irtc)
    return
    end

    subroutine tfefunrefstk(isp1, isp2, irtc)
    use tfstk
    use efun
    implicit none
    type (sad_descriptor) kx
    type (sad_dlist),  pointer :: kl
    integer(kind=4) isp1, irtc, isp2
    kx = tfefunref(isp1, .true., irtc)
    if (irtc .ne. 0) then
      return
    end if
    if (ktflistq(kx, kl)) then
      if (kl%head%k == ktfoper + mtfnull) then
        isp = isp2-1
        call tfgetllstkall(kl)
        return
      end if
    end if
    isp = isp2
    dtastk(isp) = kx
    return
    end

    subroutine tfefundef(isp1, kx, irtc)
    use tfstk
    use funs
    use eeval
    implicit none
    type (sad_descriptor) kx, k1, tfsolvemember, tfefun1
    type (sad_dlist),  pointer :: kl, kli, klx
    type (sad_symbol),  pointer :: sym
    type (sad_symdef),  pointer :: symd
    integer(kind=4) ka1
    integer(kind=4) isp1, irtc, narg, id, itfmessageexp
    logical(kind=4) tfrefq, rep
    irtc = 0
    narg = isp-isp1
1    k1 = dtastk(isp1)
    if (ktfoperq(k1, ka1)) then
      id = iget_fun_id(ka1)
      if (ka1 .gt. mtfend .and. id .gt. 999 .and. id .le. 2000) then
        kx = tfefun1(isp1, id, .false., irtc)
        if (irtc == 0 .and. tfrefq(kx)) then
        else
          irtc = itfmessageexp(999, 'General::invset', k1)
        end if
        go to 6900
      else if (ka1 == mtfatt) then
        call tfatt(isp1, kx, .false., irtc)
        go to 6900
      else
        kx = kxcompose(isp1)
        return
      end if
    else if (ktfsymbolqdef(k1%k, symd)) then
      if (symd%sym%override .ne. 0) then
        if (ktfoperq(symd%value)) then
          dtastk(isp1) = symd%value
          go to 1
        end if
      end if
      go to 7000
    else if (ktflistq(k1, kl)) then
      kx = k1
      if (kl%head%k == ktfoper + mtffun) then
        kx = tfpuref(isp1, kl, irtc)
        go to 6900
      else if (kl%head%k == ktfoper + mtfnull) then
        if (kl%nl == 0) then
          kx = tfsequence(isp1, isp)
          if (ktflistq(kx, klx)) then
            kx = tfleval(klx, .false., irtc)
          else if (ktfsymbolq(kx, sym)) then
            if (sym%override == 0) then
              sym => tfsydef(sym)
              kx = sad_descr(sym)
            end if
          end if
          go to 6900
        else if (kl%nl == 1 .and. ktfnonreallistqo(kl)) then
          kx = kxmakelist(isp1, klx)
          klx%head = dtfcopy(kl%head)
          return
        end if
      else if (kl%head%k == ktfoper + mtflist) then
        go to 6900
      else if (ktflistq(kl%head, kli)) then
        k1 = tfsolvemember(kli, rep, irtc)
        if (irtc == 0) then
          dtastk(isp1) = k1
        end if
      end if
      go to 7000
    else if (ktfstringq(k1)) then
      irtc = itfmessageexp(999, 'General::invset', k1)
    else
      go to 7000
    end if
6900 if (irtc == 0 .or. irtc .lt. -1) then
      return
    else if (irtc == -1) then
      irtc = 0
    end if
7000 isp = isp1 + narg
    kx = kxcrelistm(narg, ktastk(isp1 + 1:isp1 + narg), dtastk(isp1))
    if (irtc .gt. 0) then
      call tferrorhandle(kx, irtc)
    else if (irtc == -1) then
      irtc = 0
    end if
    return
    end

!     Type fixed function proxy for generic math function/vendor extension
!     DOUBLE instance of ASINH in Fortran2008
    real(kind=4) function tasinh(x)
    implicit none
!      include 'inc/MATH.inc'
    real(kind=4) x
    tasinh = asinh(x)
    return
    end
!     DOUBLE instance of ACOSH in Fortran2008
    real(kind=4) function tacosh(x)
    implicit none
!      include 'inc/MATH.inc'
    real(kind=4) x
    tacosh = acosh(x)
    return
    end
!     DOUBLE instance of ATANH in Fortran2008
    real(kind=4) function tatanh(x)
    implicit none
!      include 'inc/MATH.inc'
    real(kind=4) x
    tatanh = atanh(x)
    return
    end
!     DOUBLE COMPLEX proxy of ZTAN vendor extension
    complex(kind=16) function tctan(z)
    implicit none
    complex(kind=16) z, ztan
    tctan = ztan(z)
    return
    end

    subroutine tfefundummy
    use mackw
    return
    end

    subroutine tfseval(ks, ns, kh, kx, stk, av, ref, irtc)
    use tfstk
    use tfcode
    use iso_c_binding
    use efun
    use funs
    use eeval
    implicit none
    type (sad_descriptor) kx, kf, kh, kl
    type (sad_funtbl),  pointer :: fun
    type (sad_symbol),  pointer :: sym
    type (sad_dlist),  pointer :: list, klf, kls, kls1
    integer(kind=4) maxlevel
    parameter (maxlevel = 2**12)
    integer(kind=4) ks, iaat, kaf
    integer(kind=4) irtc, ns, iaf, isp10, isp11, j
    integer(kind=4) isp1, level, itfgetrecl, isp0, mf, i1, level1, &
      i, itfmessage, lpw, l, itfdownlevel
    real(kind=4) v
    logical(kind=4) ref, ev, evalh, rep, stk, tfconstlistqo, tfgetseqstk, av
    data level/0/
    level = level + 1
    if (level .ge. maxlevel-64) then
      level1 = level
      level = 0
      irtc = itfmessage(999, 'General::deep', '""')
      level = level1
      go to 9000
    end if
    call loc_sad(ks, kls)
    isp0 = isp
    kf = kh
    evalh = .false.
    kaf = ktfaddr(kf%k)
    select case(kf%k-kaf)
    case (ktfoper)
      call c_f_pointer(c_loc(klist(klist(ifunbase + kaf)-9)), fun)
      iaf = -fun%id
      if (fun%narg .lt. 0) then
        go to 100
      end if
    case (ktfsymbol)
      if (ref) then
        kf = tfsyeval(kf, irtc)
        if (irtc .ne. 0) then
          go to 8000
        end if
      else
        call loc_sym(kaf, sym)
        sym => tfsydef(sym)
        kf = sad_descr(sym)
      end if
    case (ktflist)
      call loc_dlist(kaf, list)
      if (iand(lconstlist, list%attr) == 0) then
        kf = tfleval(list, ref, irtc)
        if (irtc .ne. 0) then
          go to 8000
        end if
      end if
    case (ktfpat)
      if (ref) then
        kf = tfpateval(kf, irtc)
        if (irtc .ne. 0) then
          go to 8000
        end if
      end if
    end select
    ev = .true.
    iaat = 0

    do
      if (ktfoperq(kf%k)) then
        kaf = ktfaddr(kf)
        call c_f_pointer(c_loc(klist(klist(ifunbase + kaf)-9)), fun)
        iaat = klist(ifunbase + kaf) + 1
        mf = fun%narg
        if (mf .lt. 0) then
          iaf = -fun%id
          evalh = .true.
          exit
        end if
        ev = fun%mapeval(2, 1) == -2
        if (fun%mapeval(2, 1) == -1) then
          iaat = 0
        end if
      else if (ktfsymbolq(kf%k, sym)) then
        if (sym%override .ne. 0) then
          iaat = iand(iattrholdall, sym%attr)
          if (iaat .ne. 0) then
            ev = .false.
            iaat = merge(i00, -iaat, iaat == iattrholdall)
          end if
        end if
      else if (ktflistq(kf, klf)) then
        if (klf%head%k == ktfoper + mtfnull) then
          if (klf%nl == 1) then
            kf = klf%dbody(1)
            cycle
          else if (klf%nl == 0) then
            kf%k = ktfoper + mtfnull
            evalh = .true.
            exit
          end if
        else if (ktfsymbolq(klf%head, sym) .and. .not. ref) then
          if (sym%gen == -3 .and. ktfreallistq(klf)) then
            ev = .false.
            iaat = 0
          end if
        end if
      end if
      isp = isp0 + 1
      isp1 = isp
      go to 3000
    end do

100  isp = isp0 + 1
    isp1 = isp
    select case(iaf)
    case(mtfnull, mtflist, mtfrule, mtfrepeated, mtfrepeatednull)
      if (stk) then
        ev = .true.
        go to 3000
      end if
      if (evalh .or. .not. ref) then
        if (tfonstackq(ks) .or. kls%ref .gt. 0) then
          kls1 => tfduplist(kls)
          kls => kls1
        end if
        kls%head%k = ktfoper + iaf
        if (ref .and. tfconstlistqo(kls)) then
          kx = sad_descr(kls)
          irtc = 0
          go to 8000
        end if
      end if
      if (ref) then
        kx = tfevallev(kls, irtc)
      else
        call tfevallstkall(kls, .false., .false., irtc)
        if (irtc == 0) then
          levele = levele + 1
          go to 6000
        end if
      end if
    case(mtfset)
      rep = tfgetseqstk(ks, ns)
      if (isp .gt. isp1) then
        dtastk(isp) = tfeevalref(dtastk(isp), irtc)
        if (irtc .ne. 0) then
          go to 8000
        end if
      end if
      levele = levele + 1
      go to 6000
    case (mtfand)
      do i = 1, ns
        isp10 = isp
        call tfseqevalstk(kls%dbody(1), ns, i, av, irtc)
        if (irtc .ne. 0) then
          go to 8000
        end if
        isp11 = isp
        isp = isp10
        do j = isp10 + 1, isp11
          if (ktfrealq(ktastk(j), v)) then
            if (v==0.d0) then
              kx%k = 0
              go to 8000
            end if
          else
            isp = isp + 1
            ktastk(isp) = ktastk(j)
          end if
        end do
      end do
      if (isp == isp1) then
        kx%k = ktftrue
      else if (isp == isp1 + 1) then
        kx = dtastk(isp)
      else
        levele = levele + 1
        go to 6000
      end if
    case (mtfor)
      do i = 1, ns
        isp10 = isp
        call tfseqevalstk(kls%dbody(1), ns, i, av, irtc)
        if (irtc .ne. 0) then
          go to 8000
        end if
        isp11 = isp
        isp = isp10
        do j = isp10 + 1, isp11
          if (ktfrealq(ktastk(j), v)) then
            if (v/ = 0.d0) then
              kx%k = ktftrue
              go to 8000
            end if
          else
            isp = isp + 1
            ktastk(isp) = ktastk(j)
          end if
        end do
      end do
      if (isp == isp1) then
        kx%k = ktffalse
      else if (isp == isp1 + 1) then
        kx = dtastk(isp)
      else
        levele = levele + 1
        go to 6000
      end if
    case (mtfpart)
      if (ref .or. ns == 0) then
        ev = .true.
        go to 3000
      end if
      isp = isp + 1
      dtastk(isp) = tfeevaldef(kls%dbody(1), irtc)
      if (irtc .ne. 0) then
        go to 8000
      end if
      call tfseqevalstkall(kls%dbody(2), ns-1, av, irtc)
      if (irtc == 0) then
        levele = levele + 1
        go to 6000
      end if
    case (mtfslot, mtfslotseq)
      kx = tfslot(iaf, kls, ref, irtc)
    case (mtfcomp)
      if (ns == 0) then
        kx%k = ktfoper + mtfnull
        go to 8000
      end if
      i1 = 1
      do
        if (ltrace .gt. 0) then
          levele = levele + 1
          lpw = min(131, itfgetrecl())
          do i = i1, ns
            call tfprint1(kls%dbody(i), 6, -lpw, 4, .true., .true., irtc)
            kx = tfeevalref(kls%dbody(i), irtc)
            if (irtc .ne. 0) then
              go to 1320
            end if
          end do
        else
          levele = levele + 1
          irtc = 0
          do i = i1, ns
            kx = kls%dbody(i)
            if (ktflistq(kx, list)) then
              kx = tfleval(list, .true., irtc)
            else if (ktfsymbolq(kx)) then
              kx = tfsyeval(kx, irtc)
            else if (ktfpatq(kx)) then
              kx = tfpateval(kx, irtc)
            end if
            if (irtc .ne. 0) then
              go to 1320
            end if
          end do
        end if
        go to 7000
1320     if (irtc .gt. irtcret) then
          go to 7000
        end if
        call tfcatchreturn(irtcgoto, kl, irtc)
        l = itfdownlevel()
        if (irtc .ne. 0) then
          exit
        end if
        call tffindlabel(kls, ns, i1, kl)
        if (i1 .le. 0) then
          call tfthrow(irtcgoto, kl, irtc)
          exit
        end if
        i1 = i1 + 1
      end do
    case (mtffun, mtfpattest, mtftagset, mtfhold)
      rep = tfgetseqstk(ks, ns)
      if (rep .or. stk .or. evalh) then
        levele = levele + 1
        go to 6000
      end if
      kx%k = ktflist + ks
    case default
      write(*, *)'tfleval-implementation error: ', kaf, iaf
      call abort
    end select
    go to 8000

3000 if (levele .ge. maxlevele-32) then
      irtc = itfmessage(999, 'General::deep', '""')
      go to 8000
    end if
    levele = levele + 1
    if (ev) then
      call tfseqevalstkall(kls%dbody(1), ns, av, irtc)
      if (irtc .ne. 0) then
        go to 7000
      end if
    else if (iaat == 0 .or. av) then
      rep = tfgetseqstk(ks, ns)
    else
      call tfargevalstk(isp1, kls, ns, iaat, mf, .false., irtc)
      if (irtc .ne. 0) then
        go to 7000
      end if
    end if
6000 dtastk(isp1) = kf
    if (ref) then
      kx = tfefunref(isp1, .true., irtc)
    else
      call tfefundef(isp1, kx, irtc)
    end if
7000 continue
    call tfconnect(kx, irtc)
8000 isp = isp0
9000 level = max(0, level-1)
    return
    end

    function tfjoin2(k1, k2, eval, irtc) result(kx)
    use tfstk
    use efun
    implicit none
    type (sad_descriptor) kx
    type (sad_descriptor), intent(in):: k1, k2
    integer(kind=4), intent(out):: irtc
    integer(kind=4) isp1
    logical(kind=4), intent(in):: eval
    isp1 = isp
    isp = isp + 1
    dtastk(isp) = k1
    isp = isp + 1
    dtastk(isp) = k2
    kx = tfjoin(isp1, eval, irtc)
    isp = isp1
    return
    end function tfjoin2