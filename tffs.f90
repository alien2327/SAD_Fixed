module sad_main
    use tfstk, only: sad_descriptor, mbody
    integer(kind=4), parameter :: expnsize = 7
    integer(kind=4), parameter :: iaidx(6, 6) = reshape( &
        (/ 1, 2, 4, 7,11,16, &
           2, 3, 5, 8,12,17, &
           4, 5, 6, 9,13,18, &
           7, 8, 9,10,14,19, &
          11,12,13,14,15,20, &
          16,17,18,19,20,21/), (/6,6/))
    
    type sad_el
        sequence
        integer(kind=4) nl, nlat0
        integer(kind=8) aux, comp(1:mbody)
    end type sad_el

    type sad_comp
        sequence
        integer(kind=4) ncomp2, id
        integer(kind=4) nparam
        logical(kind=1) ori, update, updateseg, ldummy2
        integer(kind=4) ievar1,ievar2
        type (sad_descriptor) dvalue(1:0)
        integer(kind=8) kvalue(1:0)
        integer(kind=4) ivalue(2, 1:0)
        logical(kind=1) lvalue(8, 1:0)
        real(kind=8) value(1:mbody)
    end type sad_comp

    contains
    integer(kind=8) function kmelaloc(n, el)
        use tfstk, only:klist, ktaloc
        use iso_c_binding
        implicit none
        integer(kind=4) n
        type (sad_el), pointer, intent(out) :: el
        kmelaloc = ktaloc(n + 1)
        call c_f_pointer(c_loc(klist(kmelaloc-1)), el)
        el%nlat0 = n
        el%aux = 0
        return
    end function kmelaloc

    integer(kind=8) function kmcompaloc(n, cmp)
        use tfstk, only:klist, ktraaloc
        use iso_c_binding
        implicit none
        integer(kind=4) , intent(in)::n
        type (sad_comp), pointer, intent(out) :: cmp
        kmcompaloc = ktraaloc(0, n + expnsize + 2) + 2
        call c_f_pointer(c_loc(klist(kmcompaloc - 2)), cmp)
        cmp%ncomp2 = n + expnsize + 3
        return
    end function kmcompaloc

    subroutine loc_el(k, el)
        use tfstk, only:klist
        use iso_c_binding
        implicit none
        integer(kind=8) , intent(in)::k
        type (sad_el), pointer, intent(out) ::el
        call c_f_pointer(c_loc(klist(k-1)), el)
        return
    end subroutine loc_el

    subroutine loc_comp(k, cmp)
        use tfstk, only:klist
        use iso_c_binding
        implicit none
        integer(kind=8) , intent(in)::k
        type (sad_comp), pointer, intent(out) ::cmp
        call c_f_pointer(c_loc(klist(k-2)), cmp)
        return
    end subroutine loc_comp

    integer(kind=4) function idcomp(el, i)
        implicit none
        type (sad_el), intent(in) :: el
        type (sad_comp), pointer :: cmp
        integer(kind=4) , intent(in)::i
        call loc_comp(el%comp(i), cmp)
        idcomp = cmp%id
        return
    end function idcomp

    real(kind=8) function dircomp(el, i)
        implicit none
        integer(kind=4) , intent(in)::i
        type (sad_el), intent(in) :: el
        type (sad_comp), pointer :: cmp
        call loc_comp(el%comp(i), cmp)
        dircomp = merge(1.d0, -1.d0, cmp%ori)
        return
    end function dircomp
end module sad_main

module tmacro
    use mackw
    use macphys
    use macfile
    real(kind=8), parameter :: c=cveloc, hp=plankr, e=elemch, epsrad=1.d-6,  &
        emminv=1.d-15, eps00m=0.005d0, ampmaxm=0.05d0
    integer(kind=4) , parameter :: ndivmaxm=1000
    real(kind=8) amass, charge, h0, p0, omega0, trf0, crad, erad, codin(6), &
        dleng, anrad, urad, u0, vc0, wrfeff, dp0, brho, ccintr, cintrb, &
        pbunch, coumin, re0, pgev, emidiv, emidib, emidiq, emidis, ctouck, &
        dvemit, h1emit, anbunch, tdummy(6), zlost, alost, taurdx, taurdy, &
        taurdz, fridiv, beamin(21), vcalpha, vceff, vcacc, dvcacc, ddvcacc, &
        alphap, pspac_dx, pspac_dy, pspac_dz, dvfs, rcratio, rclassic, brhoz, &
        bradprev, amom0, circ, hvc0, cuc
    integer(kind=8) ilattp, lspect, ipoltr, ipolb, ipoll, ipolid, ipolo
    integer(kind=4) nflag0, nlat, np0, nturn, isynch, nspect, lplot, nplot, nuse, &
    nclas, irad, novfl, npelm, ipelm, nparallel, pspac_nx, pspac_ny, pspac_nz, &
    pspac_nturn, pspac_nturncalc, l_track
    logical(kind=4) oldflagsdummy, tparaed
end module tmacro

module tffitcode
    implicit none
    integer(kind=4),  parameter :: mfitax = 1, mfitbx = mfitax + 1, mfitnx = mfitbx + 1, &
        mfitay = mfitnx + 1, mfitby = mfitay + 1, mfitny = mfitby + 1, &
        mfitex = mfitny + 1, mfitepx = mfitex + 1, &
        mfitey = mfitepx + 1, mfitepy = mfitey + 1, &
        mfitr1 = mfitepy + 1, mfitr2 = mfitr1 + 1, &
        mfitr3 = mfitr2 + 1, mfitr4 = mfitr3 + 1, &
        mfitdetr = mfitr4 + 1, &
        mfitdx = mfitdetr + 1, mfitdpx = mfitdx + 1, &
        mfitdy = mfitdpx + 1, mfitdpy = mfitdy + 1, &
        mfitdz = mfitdpy + 1, mfitddp = mfitdz + 1, &
        mfitaz = mfitddp + 1, mfitbz = mfitaz + 1, mfitnz = mfitbz + 1, &
        mfitzx = mfitnz + 1, mfitzpx = mfitzx + 1, &
        mfitzy = mfitzpx + 1, mfitzpy = mfitzy + 1, &
        mfitpex = mfitzpy + 1, &
        mfitpepx = mfitpex + 1, &
        mfitpey = mfitpepx + 1, mfitpepy = mfitpey + 1, &
        mfitpzx = mfitpepy + 1, &
        mfitpzpx = mfitpzx + 1, &
        mfitpzy = mfitpzpx + 1, mfitpzpy = mfitpzy + 1, &
        mfitgmx = mfitpzpy + 1, mfitgmy = mfitgmx + 1, &
        mfitgmz = mfitgmy + 1, mfittrx = mfitgmz + 1, &
        mfittry = mfittrx + 1, mfitleng = mfittry + 1, &
        mfitgx = mfitleng + 1, mfitgy = mfitgx + 1, mfitgz = mfitgy + 1, &
        mfitchi1 = mfitgz + 1, mfitchi2 = mfitchi1 + 1, mfitchi3 = mfitchi2 + 1, &
        ntwissfun = mfitzpy, mfito = mfittry, mfit = mfitchi3, &
        mfit1 = mfit + 12
    logical(kind=4) :: inittws = .true.

    contains
    subroutine tfinittws
        use tfstk
        implicit none
        type (sad_descriptor) kx
        integer(kind=4) irtc
        character*2 strfromis
        if (.not. inittws) then
            return
        end if
        call tfevals('BeginPackage[tws$`];Begin[tws$`]', kx, irtc)
        call tfevals(' mfax = '//strfromis(mfitax)// &
            ';mfbx = '//strfromis(mfitbx)// &
            ';mfnx = '//strfromis(mfitnx)// &
            ';mfay = '//strfromis(mfitay)// &
            ';mfby = '//strfromis(mfitby)// &
            ';mfny = '//strfromis(mfitny)// &
            ';mfex = '//strfromis(mfitex)// &
            ';mfepx = '//strfromis(mfitepx)// &
            ';mfey = '//strfromis(mfitey)// &
            ';mfepy = '//strfromis(mfitepy)// &
            ';mfr1 = '//strfromis(mfitr1)// &
            ';mfr2 = '//strfromis(mfitr2)// &
            ';mfr3 = '//strfromis(mfitr3)// &
            ';mfr4 = '//strfromis(mfitr4)// &
            ';mfdetr = '//strfromis(mfitdetr)// &
            ';mfdx = '//strfromis(mfitdx)// &
            ';mfdpx = '//strfromis(mfitdpx)// &
            ';mfdy = '//strfromis(mfitdy)// &
            ';mfdpy = '//strfromis(mfitdpy)// &
            ';mfdz = '//strfromis(mfitdz)// &
            ';mfddp = '//strfromis(mfitddp)// &
            ';mfaz = '//strfromis(mfitaz)// &
            ';mfbz = '//strfromis(mfitbz)// &
            ';mfnz = '//strfromis(mfitnz)// &
            ';mfzx = '//strfromis(mfitzx)// &
            ';mfzpx = '//strfromis(mfitzpx)// &
            ';mfzy = '//strfromis(mfitzy)// &
            ';mfzpy = '//strfromis(mfitzpy)// &
            ';mfpex = '//strfromis(mfitpex)// &
            ';mfpepx = '//strfromis(mfitpepx)// &
            ';mfpey = '//strfromis(mfitpey)// &
            ';mfpepy = '//strfromis(mfitpepy)// &
            ';mfpzx = '//strfromis(mfitpzx)// &
            ';mfpzpx = '//strfromis(mfitpzpx)// &
            ';mfpzy = '//strfromis(mfitpzy)// &
            ';mfpzpy = '//strfromis(mfitpzpy)// &
            ';mfgmx = '//strfromis(mfitgmx)// &
            ';mfgmy = '//strfromis(mfitgmy)// &
            ';mfgmz = '//strfromis(mfitgmz)// &
            ';mftrx = '//strfromis(mfittrx)// &
            ';mftry = '//strfromis(mfittry)// &
            ';mfleng = '//strfromis(mfitleng)// &
            ';mfgx = '//strfromis(mfitgx)// &
            ';mfgy = '//strfromis(mfitgy)// &
            ';mfgz = '//strfromis(mfitgz)// &
            ';mfchi1 = '//strfromis(mfitchi1)// &
            ';mfchi2 = '//strfromis(mfitchi2)// &
            ';mfchi3 = '//strfromis(mfitchi3)// &
            ';ntwissfun = '//strfromis(ntwissfun)// &
            ";End[];EndPackage[]", kx, irtc)
        inittws = .false.
        return
    end subroutine tfinittws
end module tffitcode