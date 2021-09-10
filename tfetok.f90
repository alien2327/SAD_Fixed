module tftok
    implicit none
    character(len=24), parameter :: oper1 = '+-*/=(){}[],;@#:^&<>|~.?'
    character(len=15), parameter :: oper2 = '=><&|/.+-:#@[*)'
    character(len=4), parameter :: oper3 = '=>@.'
    character(len=11), parameter :: opern = '0123456789.'
    character(len=29), parameter :: oper = ' +-*/=(){}[],;@#:^&<>|~.?"''\t\r'
    integer(kind=4) levelop(0:255)
    integer(kind=4) ichorder(0:255)
    data levelop /256*0/

    contains
    type (sad_descriptor) function kxmakestring(string, l, del, ii)
        use tfstk
        use strbuf
        use iso_c_binding
        implicit none
        type (sad_strbuf), pointer :: strb
        logical(kind=4) full
        integer(kind=4), intent(in) :: l
        integer(kind=4), intent(out) :: ii
        integer(kind=4) i, j, jmax
        integer ucode
        character(len=8) ubuf
        character(len=*), intent(in) :: string
        character, intent(in) :: del
        character ch
        integer(kind=4), external :: Unicode2UTF8
        
        call getstringbuf(strb, l, .true.)
        i = 1
        
        do while(i .le. l)
            ch = string(i:i)
            i = i + 1
            if (ch .eq. '\\' .and. i .le. l) then
                ch = string(i:i)
                if ('0' .le. ch .and. ch .le. '7') then
                    !   Octal character literal: \#[##]
                    ucode = iachar(ch) - iachar('0')
                    do j = 1, min(2, l - i)
                        ch = string(i + j:i + j)
                        if ('0' .le. ch .and. ch .le. '7') then
                            ucode = ucode * 8 + (iachar(ch) - iachar('0'))
                        else
                            exit
                        end if
                    end do
                    ch = char(ucode)
                    i = i + j
                else if (i + 1 .le. l .and. (ch .eq. 'x' .or. ch .eq. 'X')) then
                    !   Hexadecimal character literal: \x#[#] or \X#[#]
                    ucode = 0
                    do j = 1, min(2, l - i)
                        ch = string(i + j: i + j)
                        if ('0' .le. ch .and. ch .le. '9') then
                            ucode = ucode * 16 + (iachar(ch) - iachar('0'))
                        else if ('a' .le. ch .and. ch .le. 'f') then
                            ucode = ucode * 16 + (iachar(ch) - iachar('a') + 10)
                        else if ('A' .le. ch .and. ch .le. 'F') then
                            ucode = ucode * 16 + (iachar(ch) - iachar('A') + 10)
                        else
                            exit
                        end if
                    end do
                    ch = char(ucode)
                    i = i + j
                else if (i + 1 .le. l .and. (ch .eq. 'u' .or. ch .eq. 'U')) then
                    !   Unicode character literal:	\u#[###] or \U#[#######]
                    if (ch .eq. 'U') then
                        jmax = min(8, l - i)
                    else
                        jmax = min(4, l - i)
                    end if
                    ucode = 0
                    do j = 1, jmax
                        ch = string(i + j:i + j)
                        if ('0' .le. ch .and. ch .le. '9') then
                            ucode = ucode * 16 + (iachar(ch) - iachar('0'))
                        else if ('a' .le. ch .and. ch .le. 'f') then
                            ucode = ucode * 16 + (iachar(ch) - iachar('a') + 10)
                        else if ('A' .le. ch .and. ch .le. 'F') then
                            ucode = ucode * 16 + (iachar(ch) - iachar('A') + 10)
                        else
                            exit
                        end if
                    end do
                    if (j .gt. 1) then
                        jmax = Unicode2UTF8(ucode, ubuf)
                        if (jmax .gt. 0) then
                            call putstringbufb(strb, ubuf, jmax, full)
                            i = i + j
                            cycle
                        end if
                    end if
                    ch = string(i:i)
                    i = i + 1
                else
                    select case(ch)
                    case ('n')
                        ch = C_NEW_LINE
                        i = i + 1
                    case ('r')
                        ch = C_CARRIAGE_RETURN
                        i = i + 1
                    case ('t')
                        ch = C_HORIZONTAL_TAB
                        i = i + 1
                    case ('f')
                        ch = C_FORM_FEED
                        i = i + 1
                    case ('e')
                        ch = char(27)
                        i = i + 1
                    case (C_NEW_LINE)
                        i = i + 1
                        cycle
                    case default
                        i = i + 1
                    end select
                end if
            else if (notabspace(del) .and. ch .eq. del) then
                exit
            end if
            call putstringbufb1(strb, ch)
            ii = i
            kxmakestring = sad_descr(strb%string(1))
        end do
        return
    end function kxmakestring

    logical(kind=4) function notabspace(ch)
        implicit none
        character, intent(in) :: ch
        notabspace = ch .ne. ' ' .and. ch .ne. char(9)
        return
    end function notabspace

    subroutine tremovecont(string, buf)
        implicit none
        character(len=*), intent(in) :: string
        character(len=*), intent(out) :: buf
        integer(kind=4) i, j
        i = 0
        j = 0
        do while(i .lt. len(string))
            i = i + 1
            if (i .lt. len(string) .and. string(i:i + 1) .eq. '\\\n') then
                i = i + 1
                cycle
            else if (i .lt. len(string) - 1 .and. string(i:i + 2) .eq. '\\\r\n') then
                i = i + 2
                cycle
            else
                j = j + 1
                buf(j:j) = string(i:i)
            end if
        end do
        return
    end subroutine tremovecont

    real(kind=8) function eval2(string, is1, istop, buf) result(vx)
        implicit none
        character(len=*), intent(in) :: string
        character(len=*), intent(out) :: buf
        integer(kind=4), intent(in) :: is1
        integer(kind=4), intent(out) :: istop
        integer(kind=4) l, m, j, nnl, is2
        real(kind=8) eval1
        l = len(string)
        vx = eval1(string, l, is1, m)
        istop = is1 + m
        if (m .gt. 0 .or. string(is1:is1) .eq. '.') then
            is2 = is1 + max(m, 1)
            if (is2 .lt. l .and. string(is2:is2+1) .eq. '\\\n') then
                call tedigicopy(string(is1:l), buf, j, nnl)
                vx = eval1(buf, j, 1, m)
                istop = is1 + m + nnl
            end if
        end if
        return
    end function eval2

    subroutine tedigicopy(string, buf, j, nnl)
        implicit none
        character(len=*), intent(in) :: string
        character(len=*), intent(out) :: buf
        integer(kind=4), intent(out) :: j, nnl
        integer(kind=4) i
        character ch
        j = 0
        i = 0
        nnl = 0
        do while(i .lt. len(string))
            i = i + 1
            ch = string(i:i)
            if (ch .eq. '\\') then
                if (i .lt. len(string) .and. string(i + 1:i + 1) .eq. '\n') then
                    nnl = nnl + 2
                    i = i + 1
                    cycle
                else
                    exit
                end if
            else if (ch .ge. '0' .and. ch .le. '9' .or. index('eEdDxX-+.', ch) .ne. 0) then
                j = j + 1
                buf(j:j) = ch
            else
                exit
            end if
        end do
        return 
    end subroutine tedigicopy

    subroutine tftokinit
        implicit none
        integer(kind=4) i, ic1
        character ch
        do i = 0, 255
            ch = char(i)
            if (index(oper3, ch) .ne. 0) then
                levelop(i) = 4
            else if (index(oper2, ch) .ne. 0) then
                levelop(i) = 3
            else if (index(oper1, ch) .ne. 0) then
                levelop(i) = 2
            else if (index(oper // char(10), ch) .ne. 0) then
                levelop(i) = 1
            end if
            if (ch .ge. 'a' .and. ch .le. 'z') then
                ic1 = 100000 + (i - ichar('a')) * 2
            else if (ch .ge. 'A' .and. ch .le. 'Z') then
                ic1 = 100000 + (i - ichar('A')) * 2 + 1
            else
                ic1 = i
            end if
            ichorder(i) = ic1
        end do
        return
    end subroutine tftokinit
end module tftok

subroutine tfetok(string, istop, kx, icont, irt)
    use iso_c_binding
    use tfstk
    use tftok
    use tfcode
    use strbuf
    implicit none
    type (sad_descriptor), intent(out) :: kx
    type (sad_dlist), pointer :: klx
    type (sad_rlist), pointer :: klr
    type (sad_symdef), pointer :: symd
    type (sad_namtbl), pointer :: loc
    type (sad_strbuf), pointer :: strb
    integer(kind=4), intent(out) :: istop, irt
    integer(kind=8), intent(in) :: icont
    integer(kind=4) nc, is2, isp0, lm, ich, nnl
    integer(kind=8) kax, ka
    character(len=*), intent(in) :: string
    real(kind=8) vx
    integer(kind=4) l, l1, is1, notspace, ifromstr, i, jc, itfopcode, it1, it2, notany1
    character(len=2048) buf
    character ch

    kx%k = ktfoper + mtfnull
    l = len(string)
    irt = -2

    if (l .le. 0) then
        istop = 2
        return
    end if
    
    istop = l + 1
    if (notabspace(string(1:1))) then
        is1 = 1
        go to 1
    end if

    do i = 2, l
        if (notabspace(string(i:i))) then
            is1 = i
            go to 1
        end if
    end do
    return
    1 ch = string(is1:is1)
    ich = ichar(ch)
    if (levelop(ich) .ne. 0) then
        select case(ch)
        case (C_NEW_LINE)
            if (is1 .eq. 1 .or. string(is1 - 1:is1 - 1) .ne. '\\') then
                irt = -2
                istop = is1 + 1
                return
            endif
        case (C_CARRIAGE_RETURN)
            if (is1 .eq. 1 .or. string(is1 - 1:is1 - 1) .ne. '\\') then
              irt = -2
                if (is1 .lt. l .and. string(is1 + 1:is1 + 1) .eq. C_NEW_LINE) then
                    istop = is1 + 2
                else
                    istop = is1 + 1
                endif
                return
            end if
        case ('''','"')
            isp0=isp
            kx=kxmakestring(string(is1 + 1:l), l - is1, ch, i1)
            call descr_strbuf(kx, strb)
            if (is1 + i1 .eq. l+1 .and. string(l:l) .ne. ch) then
                istop = l
                irt = -4
                call tfreestringbuf(strb)
            else
                istop = is1 + i1
                irt = 0
                kx = kxstringbuftostring(strb)
            end if
            isp = isp0
            return
        end select
        if (is1 .eq. l) then
            lm = is1
        else if (is1 .eq. l - 1) then
            if (levelop(ichar(string(l:l))) .gt. 1) then
                lm = l
            else
                lm = is1
            end if
        else if (levelop(ichar(string(is1 + 1:is1 + 1))) .gt. 2) then
            if (levelop(ichar(string(is1 + 2:is1 + 2))) .gt. 3) then
                lm = is1 + 2
            else
                lm = is1 + 1
            end if
        else
            lm = is1
        end if
        do i = lm, is1, -1
            jc = itfopcode(string(is1:i))
            if (jc .ge. 0) then
                irt = -1
                kax = jc
                istop = i + 1
                select case (jc)
                case (mtfdot)
                    vx = eval2(string, i, istop, buf)
                    if (istop .gt. i) then
                        irt = 0
                        kx = dfromr(vx)
                        return
                    end if
                    istop = i + 1
                case (mtfreplace, mtfunset, mtfreplacerepeated, mtfincrement, mtfdecrement)
                    if (i .lt. l .and. string(istop:istop) .le. '9' .and. string(istop:istop) .ge. '0') then
                    istop = i
                    select case (jc)
                    case (mtfreplace)
                        kax = mtfdiv
                    case (mtfreplacerepeated)
                        kax = mtfconcat
                    case (mtfincrement)
                        kax = mtfplus
                    case (mtfdecrement)
                        kax = mtfminus
                    case default
                        kax = mtfset
                    end select
                    end if
                case (mtfatt)
                    if (is1 .gt. 1) then
                        irt = -2
                        istop = is1
                        return
                    end if
                case (mtfpower)
                    if (is1 .ne. 1 .and. is1 .lt. l-1 .and. string(is1:is1 + 2) .eq. '^^^') then
                        irt = -2
                        istop = is1
                        return
                    end if
                case (mtfleftcomment)
                    istop = l + 1
                    if (is1 .lt. l - 2) then
                        is2=index(string(is1 + 2:l), '*)')
                        if (is2 .gt. 0) then
                            is2 = is2 + is1 + 1
                            if (is2 .lt. l - 1) then
                                is1 = notspace(string(1:l), is2 + 2)
                                if (is1 .gt. 0) then
                                    go to 1
                                endif
                            endif
                            irt = -2
                            return
                        endif
                    endif
                    irt = -3
                    return
                end select
                kx%k = ktfoper + kax
                return
            end if
        end do
        istop = is1 + 1
    else if (ch .ge. '0' .and. ch .le. '9') then
        vx = eval2(string, is1, istop, buf)
        kx = dfromr(vx)
        irt = 0
        return
    else if (ch .eq. '!') then
        return 
    else
        irt = 0
        nc = l - is1 + 1
        nnl = 0
        i = is1 + 1
        if (levelop(ichar(string(i:i))) .ne. 0) then
            nc = 1
            istop = i
        else
            do i = is1 + 2, l
                if (levelop(ichar(string(i:i))) .ne. 0) then
                    if (string(i - 1:i) .eq. '\\\n') then
                        nnl = nnl + 2
                        cycle
                    else if (string(i - 1:i) .eq. '\\\r') then
                        nnl = nnl + merge(3, 2, i .lt. l .and. string(i + 1:i + 1) .eq. '\n')
                        cycle
                    end if
                    nc = i - is1
                    istop = i
                    exit
                end if
            end do
        end if
        if (icont .ge. 0) then
            it1 = is1
            it2 = it1 + nc - 1
            if (nnl .gt. 0) then
                call tremovecont(string(it1:it2), buf)
            end if
            if (index(string(it1:it2), '_') .ne. 0) then
                irt = 0
                kx = merge(kxpaloc(string(it1:it2)), kxpaloc(buf(1:nc-nnl)), nnl .eq. 0)
                return
            else if (string(it1:it2) .eq. '%') then
                if (nc .eq. 1) then
                    irt = 0
                    kx = kxadaloc(-1, 1, klx)
                    klx%head%k = ktfsymbol + ktfcopy1(iaxout)
                    klx%dbody(1)%k = ktfsymbol + ktfcopy1(iaxline + 4)
                    go to 9000
                else if (notany1(string(it1 + 1:it2), max(0, it2 - it1), '0123456789', 1) .le. 0) then
                    irt = 0
                    kx = kxavaloc(-1, 1, klr)
                    call descr_sad(kx, klx)
                    klx%head%k = ktfsymbol + ktfcopy1(iaxout)
                    klr%rbody(1)=merge(ifromstr(string(it1 + 1:it2)), ifromstr(buf(1:nc - nnl)), nnl .eq. 0)
                    go to 9000
                end if
            end if
            kx=merge(kxsymbolz(string(it1:it2), nc, symd), kxsymbolz(buf, nc - nnl, symd), nnl .eq. 0)
            if (rlist(iaximmediate) .ne. 0.d0) then
                call loc_namtbl(symd%sym%loc, loc)
                ka = loc%symdef
                if (ka .ne. 0) then
                    call loc1_symdef(ka, symd)
                    if (ktfconstantsymq(symd%sym)) then
                        kx = symd%value
                    end if
                end if
            end if
        end if
    end if
    9000 return
end subroutine tfetok

integer function itfopcode(oper)
    use ophash
    implicit none
    integer(kind=4) iop1, ih, j
    character(len=*), intent(in) :: oper
    character(len=4) oper1
    if (opini) then
        call tfopcodehash
    end if
    oper1 = oper
    iop1 = tranfer(oper1, 0)
    ih = ichar(oper1(1:1)) + ichar(oper1(2:2)) + ichar(oper1(3:3)) + ichar(oper1(4:4))
    ih = iand(ih, 63)
    j = iophash(1, ih)
    if (j .lt. 0) then
        go to 10
    else if (iop1 .eq. transfer(opcode(j), 0)) then
        itfopcode = j
        return
    end if
    j = iophash(2, ih)
    if (j .lt. 0) then
        go to 10
    else if (iop1 .eq. transfer(opcode(j), 0)) then
        itfopcode = j
        return
    end if
    j = iophash(3, ih)
    if (j .lt. 0) then
        go to 10
    else if (iop1 .eq. transfer(opcode(j), 0)) then
        itfopcode = j
        return
    end if
    j=iophash(4,ih)
    if (j .ge. 0) then
        if (iop1 .eq. transfer(opcode(j),0)) then
            itfopcode=j
            return
        endif
    endif
    10  if (oper1 .eq. '=>  ') then
            itfopcode=mtfgeq
        else if (oper1 .eq. '=<  ') then
            itfopcode=mtfleq
        else if (oper1 .eq. '><  ') then
            itfopcode=mtfunequal
        else
            itfopcode=-1
        endif
    return
end function itfopcode

real(kind=8) function fflogi(name, exist)
    use tfstk
    implicit none
    character(len=*), intent(inout) :: name
    logical(kind=4), intent(out) :: exist
    logical(kind=4) tflogi
    call capita(name)
    fflogi = merge(1.d0, 0.d0, tflogi(name, exist))
    return
end function fflogi

subroutine tfflags(isp1, kx, irtc)
    use tfstk
    use ffs, only: fff, nflag, fname
    use tmacro
    implicit none
    type (sad_descriptor), intent(out) :: kx
    type (sad_dlist), pointer :: klx, klxi
    type (sad_string), pointer :: str
    integer(kind=4), intent(out) :: irtc
    integer(kind=4), intent(in) :: isp1
    integer(kind=4) i, lenw, itfmessage
    if (isp .gt. isp1 + 1) then
        irtc = itfmessage(9, 'General::narg', '"0"')
        return
    end if
    kx = kxadaloc(-1, nflag, klx)
    do i = 1, nflag
        klx%dbody(i) = kxadaloc(0, 2, klxi)
        klxi%dbody(1) = kxsalocb(0, fname(i), lenw(fname(i)), str)
        klxi%dbody(2)%k = merge(ktftrue, ktffalse, fff%flags(i))
    end do
    irtc = 0
    return
end subroutine tfflags

real(kind=8) function ffval(name, exist)
    use tfstk
    use sad_main
    use ffs
    use ffs_pointer, only: errk, tfvalvar, pnamec
    implicit none
    logical(kind=4), intent(out) :: exist
    integer(kind=4) i, lenw
    character(len=*), intent(in) :: name
    character(len=MAXPNAME) name1
    ffval = 0.d0
    if (name(1:1) .ne. '#') then
        exist = .false.
        return
    end if
    name1 = name(2:lenw(name))
    do i = 1, nele
        if (pnamec(nelvx(i)%klp) .eq. name1) then
            exist = .true.
            ffval = tfvalvar(nelvx(i)%klp, nelvx(i)%ival) / errk(1, nelvx(i)%klp)
            return
        end if
    end do
    exist = .false.
    return 
end function ffval

subroutine tfsymbol(isp1, kx, irtc)
    use tfstk
    implicit none
    type (sad_descriptor), intent(out) :: kx
    type (sad_string), pointer :: str
    integer(kind=4), intent(in) :: isp1
    integer(kind=4), intent(out) :: irtc
    integer(kind=4) itfmessage
    if (isp .ne. isp1 + 1) then
        irtc=itfmessage(9, 'General::narg', '"1"')
        return
    endif
        if (.not. ktfstringq(dtastk(isp), str)) then
            irtc=itfmessage(9, 'General::wrongtype', '"Character-string"')
            return
        endif
        kx=kxsymbolf(str%str, str%nch, .false.)
        irtc=0
    return
end subroutine tfsymbol