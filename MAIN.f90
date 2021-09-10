module version
    character(len=19), parameter :: versionid = '1.1.10.2.4k64      '
    character(len=19), parameter :: versiondate = '4/5/2021 18:00:00  '
    character(len=25) builtdate
    character(len=30) startdat
end module version

program MAIN
    use version
    use maccbk
    use tfmem
    use tftok
    implicit none

    call fdate1(startdat)
    call buildinfo_get_string('Build:Date', builtdate)

    write (*,*) '*** Welcome to SAD Ver.', versionid(1:len_trim(versionid)), ' built at ', builtdate(1:len_trim(builtdate)), ' ***'
    write (*,*) '*** Today: ', startdat(1:len_trim(startdat)), ' ***'

    call talocinit
    call inifil
    call initbl
    call tftokinit
    call ktfinitshare

    call toplvl
end program MAIN

subroutine fdate(dat)
    implicit none
    character(len=*), intent(out) :: dat
    character(len=8) d
    character(len=12) tim
    character(len=9) day
    integer lene

    call datetime(d, tim)
    if ( d(7:7) .eq. ' ' ) then
        d(7:7) = '0'
    end if

    call tday(day)
    dat = tim(1:2) // ':' // tim(3:4) // ':' // tim(5:6) // ' ' // day(1:lene(day)) // ' ' // d(5:6) // '/' // d(7:8) // '/' // d(1:4)
    return
end subroutine fdate