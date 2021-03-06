module eeval
    contains
    function tfeeval(k, ref, irtc) result(kx)
        use tfstk
        implicit none
        type (sad_descriptor) kx
        type (sad_descriptor), intent(in) :: k
        logical(kind=4), intent(in) :: ref
        integer(kind=4), intent(out) :: irtc
        kx = merge(tfeevalref(k, irtc), tfeevaldef(k, irtc), ref)
        return
    end function tfeeval

function tfeevalref(k, irtc) result(kx)
    use tfstk
    use mackw
    implicit none
    type (sad_descriptor) kx
    type (sad_descriptor), intent(in) :: k
    type (sad_dlist), pointer :: list
    integer(kind=8) ka
    integer(kind=4), intent(out) :: irtc
    kx = k
    irtc = 0
    if (ktflistq(k, list)) then
        if (iand(lconstlist, list%attr) .eq. 0) then
            kx = tfleval(list, .true., irtc)
        end if
    else if (ktfsymbolq(k)) then
        kx = tfsyeval(k, irtc)
    else if (ktfpatq(k)) then
        kx = tfpateval(k, irtc)
    else if (ktfrefq(k,ka)) then
        kx = dlist(ka)
    end if
    return
end function tfeevalref

function tfleval(list, ref, irtc) result(kx)
    use tfstk
    implicit none
    type (sad_descriptor) kx
    type (sad_dlist), intent(inout) :: list
    integer(kind=8) kaa
    integer(kind=4), intent(out) :: irtc
    logical(kind=4), intent(in) :: ref
    logical(kind=4) tfconstlistqo
    kaa = ksad_loc(list%head%k)
    kx%k = ktflist + kaa
    irtc = 0
    if (iand(lconstlist, list%attr) .ne. 0) then
      return
    end if
    if (iand(lnoconstlist, list%attr) .eq. 0) then
      if (tfconstlistqo(list)) then
        return
      end if
    end if
    list%ref = list%ref + 1
    call tfseval(kaa, list%nl, list%head, kx, .false., ktfreallistq(list), ref, irtc)
    call tflocal1(kaa)
    return
    end function tfleval

    subroutine tflevalstk(list, ref, irtc)
    use tfstk
    implicit none
    type (sad_dlist), intent(inout) :: list
    type (sad_dlist),  pointer :: kl
    integer(kind=4), intent(out) :: irtc
    logical(kind=4), intent(in) :: ref
    if (list%head%k .eq. ktfoper + mtfnull) then
      call tfevallstkall(list, ref, ref, irtc)
    else
      isp = isp + 1
      if (iand(lconstlist, list%attr) .ne. 0) then
        dtastk(isp) = sad_descr(list)
        irtc = 0
      else
        dtastk(isp) = tfleval(list, ref, irtc)
        if (irtc .ne. 0) then
          return
        end if
        if (ktfsequenceq(ktastk(isp), kl)) then
          isp = isp-1
          call tfgetllstkall(kl)
        end if
      end if
    end if
    return
    end subroutine tflevalstk

    recursive subroutine tfargevalstk(isp0, list, m, iaat, mf, av, irtc)
    use tfstk
    implicit none
    type (sad_dlist), intent(inout) :: list
    type (sad_dlist),  pointer :: kli
    type (sad_descriptor) ki
    integer(kind=8), intent(in) :: iaat
    integer(kind=4), intent(in) :: m, isp0
    integer(kind=4), intent(out) :: irtc
    integer(kind=4) mf, i
    logical(kind=4), intent(in) :: av
    irtc = 0
    if (m .le. 0) then
      return
    end if
    if (av) then
      dtastk(isp + 1:isp + m) = list%dbody(1:m)
      isp = isp + m
    else
      select case (iaat)
      case (1:)
        do i = 1, m
          ki = list%dbody(i)
          if (ktflistq(ki, kli)) then
            if (kli%head%k .eq. ktfoper + mtfnull) then
              call tfargevalstk(isp0, kli, kli%nl, iaat, mf, ktfreallistq(kli), irtc)
              if (irtc .ne. 0) then
                return
              end if
            elseif (ilist(2, iaat + min(isp-isp0 + 1, mf + 1)) .eq. 0) then
              call tflevalstk(kli, .true., irtc)
              if (irtc .ne. 0) then
                return
              end if
            else
              isp = isp + 1
              dtastk(isp) = ki
            end if
          elseif ((ktfsymbolq(ki) .or. ktfpatq(ki)) .and. ilist(2, &
                iaat + min(isp-isp0 + 1, mf + 1)) .eq. 0) then
            call tfevalstk(ki, .true., irtc)
            if (irtc .ne. 0) then
              return
            end if
          else
            isp = isp + 1
            dtastk(isp) = ki
          end if
        end do
      case (-iattrholdfirst)
        do i = 1, m
          ki = list%dbody(i)
          if (ktfsequenceq(ki, kli)) then
            call tfevallstkall(kli, isp .gt. isp0, .true., irtc)
            if (irtc .ne. 0) then
              return
            end if
          elseif (isp .gt. isp0) then
            isp = isp + 1
            dtastk(isp) = tfeevalref(ki, irtc)
            if (irtc .ne. 0) then
              return
            end if
          else
            isp = isp + 1
            dtastk(isp) = ki
          end if
        end do
      case (-iattrholdrest)
        do i = 1, m
          ki = list%dbody(i)
          if (ktfsequenceq(ki, kli)) then
            call tfevallstkall(kli, isp .eq. isp0, .false., irtc)
            if (irtc .ne. 0) then
              return
            end if
          elseif (isp .eq. isp0) then
            isp = isp + 1
            dtastk(isp) = tfeevalref(ki, irtc)
            if (irtc .ne. 0) then
              return
            end if
          else
            isp = isp + 1
            dtastk(isp) = ki
          end if
        end do
      case (-iattrholdall)
        call tfgetllstkall(list)
      case default
        call tfevallstkall(list, .true., .true., irtc)
      end select
    end if
    return
    end subroutine tfargevalstk

    subroutine tfseqevalstkall(ks, m, av, irtc)
    use tfstk
    implicit none
    type (sad_descriptor), intent(in) :: ks(m)
    integer(kind=4), intent(in) :: m
    integer(kind=4), intent(out) :: irtc
    integer(kind=4) i, isp0
    logical(kind=4), intent(in) :: av
    irtc = 0
    if (m .le. 0) then
      return
    end if
    if (av) then
      isp0 = isp
      dtastk(isp0 + 1:isp0 + m) = ks
      isp = isp0 + m
    else
      do i = 1, m
        call tfevalstk(ks(i), .true., irtc)
        if (irtc .ne. 0) then
          return
        end if
      end do
    end if
    return
    end subroutine tfseqevalstkall

    subroutine tfseqevalstk(ks, m, i, av, irtc)
    use tfstk
    implicit none
    type (sad_dlist),  pointer :: kli
    type (sad_descriptor), intent(in) :: ks(m)
    integer(kind=4), intent(out) :: irtc
    integer(kind=4), intent(in) :: m, i
    logical(kind=4), intent(in) :: av
    irtc = 0
    if (i .le. m) then
      if (av) then
        isp = isp + 1
        dtastk(isp) = ks(i)
      else
        if (ktflistq(ks(i), kli)) then
          call tflevalstk(kli, .true., irtc)
        elseif (ktfsymbolq(ks(i)) .or. ktfpatq(ks(i))) then
          call tfevalstk(ks(i), .true., irtc)
        else
          isp = isp + 1
          dtastk(isp) = ks(i)
        end if
      end if
    end if
    return
    end subroutine tfseqevalstk

    function tfevallev(list, irtc) result(kx)
    use tfstk
    implicit none
    type (sad_descriptor) kx
    type (sad_descriptor) ki0, ki
    type (sad_dlist), intent(in) :: list
    type (sad_dlist),  pointer :: listi
    integer(kind=8) kai
    integer(kind=4), intent(out) :: irtc
    integer(kind=4) i, isp1
    logical(kind=4) ev
    if (ktfreallistq(list) .or. iand(list%attr, kconstarg) .ne. 0) then
      kx = sad_descr(list)
      irtc = 0
      return
    end if
    ev = .false.
    isp = isp + 1
    isp1 = isp
    dtastk(isp) = list%head
    do i = 1, list%nl
      ki = list%dbody(i)
      kai = ktfaddr(ki)
      select case(ki%k-kai)
      case (ktflist)
        call loc_dlist(kai, listi)
        if (iand(lconstlist, listi%attr) .eq. 0) then
          ki0 = ki
          ki = tfleval(listi, .true., irtc)
          if (irtc .ne. 0) then
            go to 9000
          end if
          ev = ev .or. ki%k .ne. ki0%k
        end if
      case (ktfsymbol)
        ki0 = ki
        ki = tfsyeval(ki0, irtc)
        if (irtc .ne. 0) then
          go to 9000
        end if
        ev = ev .or. ki%k .ne. ki0%k
      case (ktfpat)
        ki0 = ki
        ki = tfpateval(ki0, irtc)
        if (irtc .ne. 0) then
          go to 9000
        end if
        ev = ev .or. ki%k .ne. ki0%k
      end select
      isp = isp + 1
      dtastk(isp) = ki
      if (ktfsequenceq(ki%k, listi)) then
        ev = .true.
        isp = isp-1
        call tfgetllstkall(listi)
      end if
    end do
    kx = merge(kxcompose(isp1), sad_descr(list), ev)
    irtc = 0
9000 isp = isp1-1
    return
    end function tfevallev

    subroutine tfevalstk(k, ref, irtc)
    use tfstk
    implicit none
    type (sad_descriptor), intent(in) :: k
    type (sad_dlist),  pointer :: kl
    integer(kind=4), intent(out) :: irtc
    logical(kind=4), intent(in) :: ref
    if (ktfsequenceq(k, kl)) then
      call tfevallstkall(kl, ref, ref, irtc)
    else
      isp = isp + 1
      dtastk(isp) = tfeeval(k, ref, irtc)
      if (irtc .ne. 0) then
        return
      end if
      if (ktfsequenceq(ktastk(isp), kl)) then
        isp = isp-1
        call tfgetllstkall(kl)
      end if
    end if
    return
    end subroutine tfevalstk

    subroutine tfevallstkall(list, ref1, ref, irtc)
    use tfstk
    implicit none
    type (sad_dlist), intent(in) :: list
    type (sad_descriptor) ki
    integer(kind=4), intent(out) :: irtc
    integer(kind=4) i, isp0, m
    logical(kind=4), intent(in) :: ref, ref1
    m = list%nl
    if (ktfreallistq(list)) then
      isp0 = isp
      dtastk(isp0 + 1:isp0 + m) = list%dbody(1:m)
      isp = isp0 + m
    elseif (m .gt. 0) then
      ki = list%dbody(1)
      if (ktfrealq(ki)) then
        isp = isp + 1
        dtastk(isp) = ki
      else
        call tfevalstk(ki, ref1, irtc)
        if (irtc .ne. 0) then
          return
        end if
      end if
      do i = 2, m
        ki = list%dbody(i)
        if (ktfrealq(ki)) then
          isp = isp + 1
          dtastk(isp) = ki
        else
          call tfevalstk(ki, ref, irtc)
          if (irtc .ne. 0) then
            return
          end if
        end if
      end do
    end if
    irtc = 0
    return
    end subroutine tfevallstkall
 
    function tfeevaldef(k, irtc) result(kx)
    use tfstk
    implicit none
    type (sad_descriptor), intent(in) :: k
    type (sad_descriptor) kx
    type (sad_dlist),  pointer :: list
    type (sad_symbol),  pointer :: sym
    integer(kind=4), intent(out) :: irtc
    if (ktflistq(k, list)) then
      if (iand(lconstlist, list%attr) .eq. 0) then
        kx = tfleval(list, .false., irtc)
        return
      end if
    elseif (ktfsymbolq(k, sym) .and. sym%override .eq. 0) then
      sym => tfsydef(sym)
      kx = sad_descr(sym)
      irtc = 0
      return
    end if
    irtc = 0
    kx = k
    return
    end function tfeevaldef

    function tfpateval(k, irtc) result(kx)
    use tfstk
    use tfcode
    implicit none
    type (sad_descriptor) kx
    type (sad_descriptor), intent(in) :: k
    type (sad_descriptor) ke
    type (sad_pat),  pointer :: pat
    integer(kind=8) ka, kae, kte
    integer(kind=4), intent(out) :: irtc
    ka = ktfaddrd(k)
    call loc_pat(ka, pat)
    kx%k = ktfpat + ka
    irtc = 0
    ke = pat%expr
    kae = ktfaddr(ke)
    kte = ke%k-kae
    if (kte .ne. ktfref) then
      ke = tfeevalref(ke, irtc)
      if (irtc .ne. 0) then
        return
      end if
      kae = ktfaddr(ke)
      kte = ke%k-kae
    end if
    if (ke%k .ne. pat%expr%k) then
      kx = kxpcopyss(ke, pat%head, pat%sym%alloc, pat%default)
    end if
    return
    end function tfpateval

    function tfsyeval(ka, irtc) result(kx)
    use tfstk
    use tfcode
    use iso_c_binding
    use mackw
    implicit none
    type (sad_descriptor) kx
    type (sad_descriptor), intent(in) :: ka
    type (sad_symbol),  pointer :: sym
    type (sad_symdef),  pointer :: symd
    type (sad_namtbl),  pointer :: loc
    type (sad_object),  pointer :: obj
    integer(kind=4) maxlevel
    parameter (maxlevel = 4096)
    integer(kind=8) kas, j
    integer(kind=4), intent(out) :: irtc
    integer(kind=4) itfmessage, level1
    integer(kind=4) level, ig, ig1
    data level/0/
    kas = ktfaddrd(ka)
    call loc_sym(kas, sym)
    irtc = 0
    ig = sym%gen
    if (ig .eq. -3) then
      kx%k = ktfsymbol + kas
      return
    end if
    if (sym%override .ne. 0) then
      call loc_symdef(kas, symd)
    else
      call loc_namtbl(sym%loc, loc)
      kas = loc%symdef
      ig = max(0, ig)
      do while(kas .gt. 0)
        call loc1_symdef(kas, symd)
        ig1 = max(0, symd%sym%gen)
        if (ig .eq. ig1) then
          exit
        elseif (ig .lt. ig1) then
          kas = symd%next
        else
          exit
        end if
      end do
    end if
    kx = symd%value
    if (kx%k .ne. ktfsymbol + kas .and. ktfnonrealq(kx%k)) then
      level = level + 1
      if (level .gt. maxlevel-64) then
        level1 = level
        level = 0
        irtc = itfmessage(999, 'General::deep', '""')
        level = level1
        return
      end if
      kx = tfeevalref(kx, irtc)
      level = level-1
      if (ktfobjq(kx, obj)) then
        if (ktfaddr(obj%alloc) .eq. 0) then
          j = itflocal + levele
          obj%alloc%k = ktftype(obj%alloc%k) + klist(j)
          klist(j) = ksad_loc(obj%alloc%k)
        end if
      end if
    end if
    return
    end function tfsyeval

    function tfpuref(isp1, kf, irtc) result(kx)
    use tfstk
    implicit none
    type (sad_descriptor) kx, ki, ka
    type (sad_dlist), intent(in) :: kf
    type (sad_dlist),  pointer :: kla
    integer(kind=4), intent(in) :: isp1
    integer(kind=4), intent(out) :: irtc
    integer(kind=4) itfmessage, narg, m, j, i, ipf0, nap0, isp0
    logical(kind=4) rep
    kx = dxnullo
    if (kf%nl .eq. 1) then
      isp0 = isp
      do i = isp1 + 1, isp
        dtastk(i) = dtfcopy(dtastk(i))
      end do
      ipf0 = ipurefp
      nap0 = napuref
      ipurefp = isp1
      napuref = isp-isp1
      isp = isp + 1
      itastk(1, isp) = ipf0
      itastk(2, isp) = nap0
      kx = tfeevalref(kf%dbody(1), irtc)
      ipurefp = ipf0
      napuref = nap0
      do i = isp1 + 1, isp0
        call tflocald(dtastk(i))
      end do
      isp = isp0
    elseif (kf%nl .eq. 2) then
      narg = isp-isp1
      ka = kf%dbody(1)
      if (ktfsymbolq(ka)) then
        if (narg .ne. 1) then
          irtc = itfmessage(9, 'General::narg', '"equal to actual number of args"')
          return
        end if
        dtastk(isp + 1) = ka
        isp = isp + 2
        dtastk(isp) = dtastk(isp1 + 1)
      elseif (tflistq(ka, kla)) then
        m = kla%nl
        if (m .ne. narg) then
          irtc = itfmessage(9, 'General::narg', '"equal to actual number of args"')
          return
        end if
        if (m .ne. 0) then
          if (ktfreallistq(kla)) then
            irtc = itfmessage(9, 'General::wrongtype', '"List of symbols"')
            return
          end if
          do i = 1, m
            ki = kla%dbody(i)
            if (.not. ktfsymbolq(ki)) then
              irtc = itfmessage(9, 'General::wrongtype', '"List of symbols"')
              return
            end if
            j = isp + i*2
            dtastk(j-1) = ki
            ktastk(j) = ktastk(isp1 + i)
          end do
          isp = isp + 2*m
        end if
      else
        irtc = itfmessage(9, 'General::wrongtype', '"List of symbols"')
        return
      end if
      kx = kf%dbody(2)
      if (narg .ne. 0) then
        call tfreplacesymbolstk(kx, isp1 + narg, narg, kx, .true., rep, irtc)
        if (irtc .ne. 0) then
          isp = isp1 + narg
          return
        end if
      end if
      kx = tfeevalref(kx, irtc)
      isp = isp1 + narg
    else
      irtc = itfmessage(9, 'General::narg', '"1 or 2"')
    end if
    return
    end function tfpuref

    recursive function tfset(isp1, upvalue, irtc) result(kx)
    use tfstk
    implicit none
    type (sad_descriptor) kx, k1, k2, k110, k11, tfset1
    type (sad_symbol),  pointer :: sym
    type (sad_symdef),  pointer :: symd
    type (sad_dlist),  pointer :: kl11
    integer(kind=4), intent(in) :: isp1
    integer(kind=4), intent(out) :: irtc
    integer(kind=4) i, itfmessage, isp11, isp0
    logical(kind=4), intent(in) :: upvalue
    logical(kind=4) euv
    if (isp1 + 1 .ge. isp) then
      kx = dxnull
      irtc = itfmessage(9, 'General::narg', '"2 or more"')
      return
    end if
    k2 = dtastk(isp)
    if (isp1 + 2 .eq. isp) then
      k1 = dtastk(isp1 + 1)
      if (ktfsymbolq(k1, sym)) then
        if (sym%override .eq. 0) then
          sym => tfsydef(sym)
          k1 = sad_descr(sym)
        end if
      elseif (ktflistq(k1)) then
        k1 = tfeevaldef(k1, irtc)
        if (irtc .ne. 0) then
          return
        end if
      end if
      if (k1%k .ne. ktastk(isp1 + 1) .and. upvalue) then
        k11 = k1
        k110 = k1
        if (ktflistq(k11, kl11)) then
          k11 = kl11%head
          if (ktflistq(k11)) then
            k110 = k11
            do while(ktflistq(k11, kl11))
              k11 = kl11%head
            end do
          end if
        end if
        if (ktfsymbolq(k11, sym)) then
          if (sym%override .eq. 0) then
            sym => tfsydef(sym)
          end if
          call sym_symdef(sym, symd)
          if (symd%upval .ne. 0) then
            isp = isp + 1
            isp11 = isp
            dtastk(isp11) = dtastk(isp1)
            isp = isp + 1
            dtastk(isp) = k1
            isp = isp + 1
            dtastk(isp) = k2
            call tfdeval(isp11, ksad_loc(sym%loc), kx, 0, .false., euv, irtc)
            isp = isp11-1
            if (euv) then
              return
            end if
          end if
        end if
      end if
      kx = tfset1(k1, k2, ktfaddr(ktastk(isp1)), irtc)
    else
      isp0 = isp
      kx = dtastk(isp)
      do i = isp-1, isp1 + 1, -1
        isp = isp0 + 3
        dtastk(isp-2) = dtastk(isp1)
        dtastk(isp-1) = dtastk(i)
        dtastk(isp  ) = kx
        kx = tfset(isp0 + 1, upvalue, irtc)
        if (irtc .ne. 0) then
          return
        end if
      end do
      isp = isp0
    end if
    return
    end function tfset

    function tfslot(iopc, kls, ref, irtc) result(kx)
    use tfstk
    use funs
    implicit none
    type (sad_descriptor) kx, ka
    type (sad_dlist), intent(in) :: kls
    type (sad_symbol),  pointer :: sym
    type (sad_namtbl),  pointer :: nam
    integer(kind=8) kaa
    integer(kind=4), intent(in) :: iopc
    integer(kind=4), intent(out) :: irtc
    integer(kind=4) ind, nc, isp1, isps, itfmessage, ns, ipf0, naf0, ls, isp2, itfmessagestr
    real(kind=8) ffval, vx
    character*256 name
    character*12 inds
    logical(kind=4), intent(in) :: ref
    logical(kind=4) exist
    kx = dxnullo
    ns = kls%nl
    if (ns .gt. 1) then
      irtc = itfmessage(9, 'General::narg', '"0 or 1"')
      return
    end if
    if (ns .eq. 0) then
      ind = 1
    else
      ka = kls%dbody(1)
      if (ktfoperq(ka, kaa)) then
        if (kaa .eq. mtfnull) then
          ind = 1
        else
          irtc = itfmessage(999, 'General::invop', ' ')
          return
        end if
      elseif (ktfrealq(ka, ind)) then
      elseif (ktfsymbolq(ka, sym) .and. iopc .eq. mtfslot) then
        call sym_namtbl(sym, nam)
        nc = nam%str%nch + 1
        name(2:nc) = nam%str%str(1:nc-1)
        name(1:1) = '#'
        call capita(name(1:nc))
        vx = ffval(name(1:nc), exist)
        if (exist) then
          kx = dfromr(vx)
          irtc = 0
        else
          irtc = itfmessage(999, 'FFS::undef', '"element"')
        end if
        return
      else
        irtc = itfmessage(999, 'General::wrongtype', '"Number or symbol"')
        return
      end if
      if (ind .lt. 0) then
        ind = napuref + ind + 1
      end if
    end if
    isps = ipurefp + ind
    if (iopc .eq. mtfslot) then
      if (ipurefp .eq. 0 .or. ind .le. 0 .or. ind .gt. napuref) then
        call strfromil(ind, inds, ls)
        irtc = itfmessagestr(999, 'General::slot', '#'//inds(:ls))
        return
      end if
      kx = dtastk(isps)
    else
      if (ipurefp .eq. 0 .or. ind .le. 0) then
        call strfromil(ind, inds, ls)
        irtc = itfmessagestr(999, 'General::slot', '##'//inds(:ls))
        return
      end if
      isp1 = isp
      isp2 = ipurefp + napuref
      kx = tfsequence(isps-1, isp2)
    end if
    ipf0 = ipurefp
    naf0 = napuref
    ipurefp = itastk(1, ipf0 + naf0 + 1)
    napuref = itastk(2, ipf0 + naf0 + 1)
    kx = tfeeval(kx, ref, irtc)
    ipurefp = ipf0
    napuref = naf0
    return 
    end function tfslot

    end module eeval

    subroutine tfgetllstk(list, i1, i2)
    use tfstk
    implicit none
    type (sad_dlist), intent(in) :: list
    type (sad_dlist),  pointer :: kl
    integer(kind=4) i, m
    integer(kind=4), intent(in) :: i1, i2
    if (i2 .ge. 0) then
      m = min(i2, list%nl)
    else
      m = list%nl + i2 + 1
    end if
    if (i1 .gt. m) then
      return
    end if
    do i = max(0, i1), m
      isp = isp + 1
      dtastk(isp) = list%dbody(i)
      if (ktfsequenceq(ktastk(isp), kl)) then
        isp = isp-1
        call tfgetllstkall(kl)
      end if
    end do
    return
    end subroutine tfgetllstk

    subroutine tffindlabel(list, m, i, kr)
    use tfstk
    implicit none
    type (sad_descriptor) kr
    type (sad_dlist), intent(in) :: list
    type (sad_dlist),  pointer :: listj
    integer(kind=4), intent(out) :: i
    integer(kind=4), intent(in) :: m
    integer(kind=4) j
    type (sad_descriptor),  save :: kxlabel
    data kxlabel%k /0/
    if (kxlabel%k .eq. 0) then
      kxlabel = kxsymbolf('Label', 5, .true.)
    end if
    do j = 1, m
      if (ktflistq(list%dbody(j), listj)) then
        if (listj%nl .eq. 1) then
          if (tfsameq(kxlabel, listj%head)) then
            if (tfsameq(kr, listj%dbody(1))) then
              i = j
              return
            end if
          end if
        end if
      end if
    end do
    i = 0
    return
    end subroutine tffindlabel

    logical(kind=4) function tfgetseqstk(ks, ns)
    use tfstk
    implicit none
    type (sad_dlist),  pointer :: kl
    integer(kind=8), intent(in) :: ks
    integer(kind=8) ki
    integer(kind=4), intent(in) :: ns
    integer(kind=4) i
    tfgetseqstk = .false.
    if (ns .gt. 0) then
      do i = 1, ns
        ki = klist(ks + i)
        if (ktfsequenceq(ki, kl)) then
          tfgetseqstk = .true.
          call tfgetllstkall(kl)
          cycle
        end if
        isp = isp + 1
        ktastk(isp) = ki
      end do
    end if
    return
    end function tfgetseqstk

    integer(kind=4) function itfdownlevel()
    use tfstk
    use tfcode
    use iso_c_binding
    implicit none
    type (sad_dlist),  pointer :: list
    type (sad_pat),  pointer :: pat
    integer(kind=8) j, kt, ka, i, lastfree
    integer(kind=4) kk
    lastfree = 0
    j = itflocal + levele
    i = klist(j)
    do while(i .ne. j)
      ka = ktfaddr(klist(i))
      if (ilist(1, i + 1) .le. 0) then
        kt = klist(i)-ka
        if (kt .eq. ktflist) then
          call loc_sad(i + 2, list)
          if (iand(list%attr, lnonreallist) .ne. 0) then
            do kk = list%nl, 1, -1
              call tflocalrel(list%dbody(kk), ka)
            end do
          end if
          call tflocalrel(list%head, ka)
          i = i-list%lenp
          ilist(1, i-1) = list%lenp + list%lena + list%nl + 4
        elseif (kt .eq. ktfpat) then
          call loc_pat(i + 2, pat)
          if (pat%sym%ref .gt. 1) then
            call tflocalrel(sad_descr(pat%sym), ka)
            pat%sym%attr = pat%len-7
            pat%sym%gen = 0
            pat%len = 7
          end if
          if (pat%sym%loc .ne. 0) then
            call tflocalrel(pat%sym%alloc, ka)
          end if
          pat%sym%alloc%k = ktfsymbol
          call tflocalrel(pat%default, ka)
          call tflocalrel(pat%head, ka)
          call tflocalrel(pat%expr, ka)
        end if
        call tfreel(i, lastfree)
      else
        klist(i) = klist(i)-ka
      end if
      i = ka
    end do
    call tfreel(i00, lastfree)
    klist(j) = j
    levele = max(0, levele-1)
    itfdownlevel = levele
    return
    end function itfdownlevel

    subroutine tflocalrel(k, kan)
    use tfstk
    implicit none
    type (sad_descriptor), intent(in) :: k
    type (sad_object),  pointer :: obj
    integer(kind=8), intent(out) :: kan
    if (ktfobjq(k, obj)) then
      obj%ref = obj%ref-1
      if (obj%ref .le. 0) then
        obj%ref = 0
        if (ktfaddr(obj%alloc%k) .eq. 0) then
          obj%alloc%k = ktftype(obj%alloc%k) + kan
          kan = ktfaddr(k%k)-2
        end if
      end if
    end if
    return
    end subroutine tflocalrel

    subroutine tfreel(ip, last)
    use tfstk
    implicit none
    integer(kind=8), intent(in) :: ip
    integer(kind=8), intent(inout) :: last
    if (ip .eq. 0) then
      if (last .ne. 0) then
        call tfree(last)
        last = 0
      end if
    else
      ilist(1, ip-1) = max(4, ilist(1, ip-1))
      if (last .eq. 0) then
        last = ip
      else
        if (ilist(1, last-1) + last .eq. ip) then
          ilist(1, last-1) = ilist(1, last-1) + ilist(1, ip-1)
        elseif (ilist(1, ip-1) + ip .eq. last) then
          ilist(1, ip-1) = ilist(1, ip-1) + ilist(1, last-1)
          last = ip
        else
          call tfree(last)
          last = ip
        end if
      end if
    end if
    return
    end subroutine tfreel

    subroutine tflocal(k)
    use tfstk
    implicit none
    type (sad_object),  pointer :: obj
    integer(kind=8), intent(in) :: k
    integer(kind=8) ka, itfroot
    if (ktfobjq(k)) then
      ka = ktfaddr(k)
      call loc_obj(ka, obj)
      obj%ref = obj%ref-1
      if (obj%ref .le. 0) then
        obj%ref = 0
        if (ktfaddr(obj%alloc) .eq. 0) then
          itfroot = itflocal + levele
          obj%alloc%k = obj%alloc%k + ktfaddr(klist(itfroot))
          klist(itfroot) = ka-2
        end if
      end if
    end if
    return
    end subroutine tflocal

    subroutine tflocal1(k)
    use tfstk
    implicit none
    type (sad_object),  pointer :: obj
    integer(kind=8), intent(in) :: k
    integer(kind=8) ka, itfroot
    ka = ktfaddr(k)
    call loc_obj(ka, obj)
    obj%ref = obj%ref-1
    if (obj%ref .le. 0) then
      obj%ref = 0
      if (ktfaddr(obj%alloc) .eq. 0) then
        itfroot = itflocal + levele
        obj%alloc%k = obj%alloc%k + ktfaddr(klist(itfroot))
        klist(itfroot) = ka-2
      end if
    end if
    return
    end subroutine tflocal1

    integer(kind=4) function itfuplevel()
    use tfstk
    implicit none
    levele = min(maxlevele, levele + 1)
    klist(itflocal + levele) = itflocal + levele
    itfuplevel = levele
    return
    end function itfuplevel