module tfshare
    integer(kind=4),  parameter:: nshmax = 1024
    integer(kind=8),  save:: kashare(nshmax) = 0, lshare(nshmax) = 0
    integer(kind=4),  save :: ishared(nshmax) = 0, kstshare(0:nshmax) = 0
    integer(kind=4), parameter :: sem_init = 0, sem_destroy = 1, &
        sem_post = 2, sem_wait = 3, sem_trywait = 4
    
    contains
    integer(kind=8) function ktfallocshared(n)
    use tfstk
    use iso_c_binding
    implicit none
    integer(kind=4),  save :: lps = 0
    integer(kind=4), intent(in):: n
    integer(kind=4) irtc, getpagesize, nsh1, i, na
    integer(kind=8) k, kpb, kcp
    if (lps .eq. 0) then
      lps = getpagesize() / 8
    end if
    na = ((n + 3) / lps + 2)*lps
    nsh1 = 0
    do i = 1, kstshare(0)
      if (kstshare(i) .eq. 0 .and. lshare(i) .ge. na) then
        kstshare(i) = 1
        ktfallocshared = kashare(i)
        return
      else if (kstshare(i) .ge. 0) then
        nsh1 = i
      end if
    end do
    kstshare(0) = nsh1
    nsh1 = 0
    if (kstshare(0) .eq. nshmax) then
      do i = 1, kstshare(0)
        if (kstshare(i) .le. 0) then
          if (lshare(i) .gt. 0) then
            kstshare(i) = 1
            if (nsh1 .ne. 0) then
              kstshare(nsh1) = 1-kstshare(i)
            end if
            call tfreleaseshared(kashare(i))
            nsh1 = i
            exit
          else if (nsh1 .eq. 0) then
            kstshare(i) = 1-kstshare(i)
            nsh1 = i
          end if
        end if
      end do
    else
      nsh1 = kstshare(0) + 1
      kstshare(nsh1) = 1
      kstshare(0) = nsh1
    end if
    k = ktaloc(na)
    kcp = transfer(c_loc(klist(k)), k) / 8
    kpb = k + ((kcp + 1 + lps) / lps)*lps-kcp
    call mapallocshared8(klist(kpb), int8(na-lps), 8, irtc)
    if (irtc .ne. 0) then
      write(*, *)'ktfallocshared failed: ', kpb, na-lps
      call abort
    end if
    ktfallocshared = kpb + 2
    klist(kpb) = k
    klist(kpb + 1) = na-lps
    if (nsh1 .ne. 0) then
      ishared(nsh1) = 1
      kstshare(nsh1) = 1
      kashare(nsh1) = kpb + 2
      lshare(nsh1) = na
    end if
    return
    end function ktfallocshared

    subroutine tfreeshared(kpb, ist)
    use tfstk
    implicit none
    integer(kind=4),  optional, intent(in):: ist
    integer(kind=4) i, is
    integer(kind=8), intent(in):: kpb
    is = 0
    if (present(ist)) then
      is = ist
    end if
    do i = 1, kstshare(0)
      if (kashare(i) .eq. kpb) then
        kstshare(i) = is
        if (is .lt. 0) then
          lshare(i) = 0
        end if
        return
      end if
    end do
    call tfreleaseshared(kpb)
    return
    end subroutine tfreeshared

    subroutine tfreleaseshared(kpb)
    use tfstk
    implicit none
    integer(kind=8), intent(in):: kpb
    integer(kind=8) k
    integer(kind=4) irtc
    k = klist(kpb-2)
    call mapallocfixed8(klist(kpb-2), klist(kpb-1), 8, irtc)
    if (irtc .ne. 0) then
      write(*, *)'tffreecshared failed: ', kpb, klist(kpb-1)
      call abort
    end if
    if (itfcbk(k) .eq. 0) then
      call tfentercbk(k, klist(kpb-1) + 1)
    end if
    call tfree(k)
    return
    end subroutine tfreleaseshared

    end module tfshare

    subroutine tfsavesharedmap()
    use tfstk
    use tfshare
    implicit none
    ishared = 0
    kstshare(0) = 0
    return
    end subroutine tfsavesharedmap

    subroutine tfresetsharedmap()
    use tfstk
    use tfshare
    implicit none
    integer(kind=4) i
    do i = 1, kstshare(0)
      if (ishared(i) .ne. 0) then
        kstshare(i) = -1
      end if
    end do
    return
    end subroutine tfresetsharedmap

    subroutine ktfinitshare
    use tfstk
    use iso_c_binding
    use tfshare
    implicit none
    integer(kind=4),  save :: lps = 0
    integer(kind=4) getpagesize
    if (lps .eq. 0) then
      lps = getpagesize() / 8
    end if
    return
    end subroutine ktfinitshare

    recursive subroutine tfrecallshared(isp0, k, kx, irtc)
    use tfstk
    implicit none
    type (sad_descriptor), intent(out):: kx
    type (sad_descriptor), intent(in):: k
    type (sad_descriptor) k0, ki
    type (sad_string),  pointer :: str
    type (sad_dlist),  pointer :: kl
    integer(kind=8) kax
    integer(kind=4), intent(in):: isp0
    integer(kind=4), intent(out):: irtc
    integer(kind=4) m, itfmessage, i
    logical(kind=4) tfcheckelement
    do i = isp0 + 1, isp
      if (ktastk(i) .eq. k%k) then
        kx = dtastk2(i)
        return
      end if
    end do
    if (ktfstringq(k, str)) then
      kx = kxsalocb(-1, str%str, str%nch)
    else if (ktfsymbolq(k)) then
      if ( .not. tfcheckelement(k, .false.)) then
        irtc = itfmessage(9, 'General::wrongtype', '"defined symbol returned in Shared"')
        return
      end if
      kx = k
    else if (ktflistq(k, kl)) then
      k0 = kl%head
      if (ktfobjq(k0)) then
        call tfrecallshared(isp0, k0, k0, irtc)
        if (irtc .ne. 0) then
          return
        end if
      end if
      m = kl%nl
      if (kl%ref .eq. 0) then
        kax = ktavaloc(-1, m)
        dlist(kax + 1:kax + m) = kl%dbody(1:m)
      else
        kax = ktadaloc(-1, m)
        do i = 1, m
          ki = kl%dbody(i)
          if (ktfobjq(ki)) then
            call tfrecallshared(isp0, ki, ki, irtc)
            if (irtc .ne. 0) then
              klist(kax + 1:kax + m) = ktfoper + mtfnull
              exit
            end if
            dlist(kax + i) = dtfcopy1(ki)
          else
            dlist(kax + i) = ki
          end if
        end do
      end if
      dlist(kax) = dtfcopy(k0)
      kx%k = ktflist + kax
    else
      kx%k = ktfoper + mtfnull
    end if
    isp = isp + 1
    dtastk(isp) = k
    dtastk2(isp) = kx
    irtc = 0
    return
    end subroutine tfrecallshared

    subroutine tfstoreshared(isp0, k, kap)
    use tfstk
    implicit none
    integer(kind=8), intent(in):: k, kap
    integer(kind=8) ka, kh, ki, kt
    integer(kind=4), intent(in):: isp0
    integer(kind=4) i, j, m
    ka = ktfaddr(k)
    kt = k-ka
    if (kt .eq. ktfstring) then
      klist(kap) = klist(ka)
      do i = 1, (ilist(1, ka) + 7) / 8
        rlist(kap + i) = rlist(ka + i)
      end do
    else if (kt .eq. ktflist) then
      kh = klist(ka)
      klist(kap + 1) = kh
      if (ktfobjq(kh) .and. ktfnonsymbolq(kh)) then
        do j = isp0, isp
          if (ktastk(j) .eq. kh) then
            klist(kap + 1) = merge(ktfstring + ktastk2(j), ktflist + ktastk2(j) + 1, ktfstringq(kh))
            exit
          end if
        end do
      end if
      m = ilist(2, ka-1)
      ilist(2, kap) = m
      if (ktfreallistq(ka)) then
        ilist(1, kap) = 0
        rlist(kap + 2:kap + m + 1) = rlist(ka + 1:ka + m)
      else
        ilist(1, kap) = 1
        do i = 1, m
          ki = klist(ka + i)
          if (ktfnonobjq(ki) .or. ktfsymbolq(ki)) then
            klist(kap + i + 1) = ki
          else
            do j = isp0, isp
              if (ktastk(j) .eq. ki) then
                if (ktfstringq(ki)) then
                  klist(kap + i + 1) = ktfstring + ktastk2(j)
                else
                  klist(kap + i + 1) = ktflist + ktastk2(j) + 1
                end if
                exit
              end if
            end do
          end if
        end do
      end if
    end if
    return
    end subroutine tfstoreshared

    recursive subroutine tfsharedsize(isp0, k, n, irtc)
    use tfstk
    implicit none
    integer(kind=8), intent(in):: k
    integer(kind=8) ka, kt, ki, kh
    integer(kind=4), intent(in):: isp0
    integer(kind=4), intent(out):: irtc, n
    integer(kind=4) i, itfmessage, ni
    irtc = 0
    if (ktfnonobjq(k) .or. ktfsymbolq(k)) then
      n = 1
    else
      ka = ktfaddr(k)
      kt = k-ka
      if (kt .eq. ktfstring) then
        n = 3 + (ilist(1, ka) + 7) / 8
        do i = isp0 + 1, isp
          if (ktastk(i) .eq. k) then
            n = 1
            exit
          end if
        end do
        isp = isp + 1
        ktastk(isp) = k
        itastk2(1, isp) = n
      else if (kt .eq. ktflist) then
        do i = isp0 + 1, isp
          if (ktastk(i) .eq. k) then
            n = 1
            return
          end if
        end do
        n = 3 + ilist(2, ka-1)
        isp = isp + 1
        ktastk(isp) = k
        itastk2(1, isp) = n
        kh = klist(ka)
        if (ktfobjq(kh) .and. ktfnonsymbolq(kh)) then
          call tfsharedsize(isp0, klist(ka), ni, irtc)
          if (irtc .ne. 0) then
            return
          end if
          n = n + ni-1
        end if
        if (ktfnonreallistq(ka)) then
          do i = 1, ilist(2, ka-1)
            ki = klist(ka + i)
            if (ktfobjq(ki) .and. ktfnonsymbolq(ki)) then
              call tfsharedsize(isp0, ki, ni, irtc)
              if (irtc .ne. 0) then
                return
              end if
              n = n + ni-1
            end if
          end do
        end if
      else
        go to 9000
      end if
    end if
    return
9000 irtc = itfmessage(9, 'General::wrongtype', '"No Symbols nor Patterns"')
    return
    end subroutine tfsharedsize
