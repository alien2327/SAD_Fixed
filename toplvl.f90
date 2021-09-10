module mackw
    use maccode
    integer(kind=4), parameter :: kwL = 1, kwANGL = kwL + 1, kwROT = kwANGL + 1, kwK0 = kwROT + 1, &
        kwK1 = kwK0 + 1, kwDK1 = kwK1 + 1, kwK2 = kwDK1 + 1, kwDK2 = kwK2 + 1, &
        kwK3 = kwDK2 + 1, kwK4 = kwK3 + 1, kwK5 = kwK4 + 1, &
        kwK6 = kwK5 + 1, kwK7 = kwK6 + 1, kwK8 = kwK7 + 1, &
        kwK9 = kwK8 + 1, kwK10 = kwK9 + 1, kwDK3 = kwK10 + 1, kwA3 = kwDK3 + 1, &
        kwA5 = kwA3 + 1, kwA7 = kwA5 + 1, kwA9 = kwA7 + 1, &
        kwA11 = kwA9 + 1, kwA13 = kwA11 + 1, kwA15 = kwA13 + 1, &
        kwA17 = kwA15 + 1, kwE1 = kwA17 + 1, kwE2 = kwE1 + 1, &
        kwTILT = kwE2 + 1, kwKICK = kwTILT + 1, kwDX = kwKICK + 1, kwDY = kwDX + 1, &
        kwVOLT = kwDY + 1, kwPHI = kwVOLT + 1, kwHARM = kwPHI + 1, &
        kwFREQ = kwHARM + 1, kwAX = kwFREQ + 1, kwAY = kwAX + 1, &
        kwBX = kwAY + 1, kwBY = kwBX + 1, kwEX = kwBY + 1, kwEY = kwEX + 1, &
        kwDP = kwEY + 1, kwRAD = kwDP + 1, kwCHRO = kwRAD + 1, kwDZ = kwCHRO + 1, &
        kwEPX = kwDZ + 1, kwEPY = kwEPX + 1, kwPX = kwEPY + 1, kwPY = kwPX + 1, &
        kwSIGZ = kwPY + 1, kwSIGE = kwSIGZ + 1 , kwAZ = kwSIGE + 1, &
        kwDPX = kwAZ + 1, kwDPY = kwDPX + 1, &
        kwR1 = kwDPY + 1, kwR2 = kwR1 + 1, kwR3 = kwR2 + 1, kwR4 = kwR3 + 1, &
        kwDETR = kwR4 + 1, kwEMIX = kwDETR + 1, kwEMIY = kwEMIX + 1, &
        kwINDX = kwEMIY + 1, kwBMAX = kwINDX + 1, kwPRD = kwBMAX + 1, &
        kwRHO0 = kwBMAX + 1, kwRHO = kwRHO0 + 1, kwCHI1 = kwRHO + 1, &
        kwCHI2 = kwCHI1 + 1, kwCHI3 = kwCHI2 + 1, kwGEO = kwCHI3 + 1, &
        kwBZ = kwGEO + 1, kwBND = kwBZ + 1, kwFRIN = kwBND + 1, kwK0FR = kwFRIN + 1, &
        kwEPS = kwK0FR + 1, kwF1 = kwEPS + 1, kwF2 = kwF1 + 1, kwFRMD = kwF2 + 1, &
        kwRANK = kwFRMD + 1, kwRANP = kwRANK + 1, kwRANV = kwRANP + 1, &
        kwKIN = kwRANV + 1, kwLWAK = kwKIN + 1, kwTWAK = kwLWAK + 1, &
        kwSK1 = kwTWAK + 1, kwSK2 = kwSK1 + 1, &
        kwSK3 = kwSK2 + 1, kwSK4 = kwSK3 + 1, kwSK5 = kwSK4 + 1, kwSK6 = kwSK5 + 1, &
        kwSK7 = kwSK6 + 1, kwSK8 = kwSK7 + 1, kwSK9 = kwSK8 + 1, kwSK10 = kwSK9 + 1, &
        kwSLI = kwSK10 + 1, kwNP = kwSLI + 1, kwDIR = kwNP + 1, &
        kwLAG = kwDIR + 1, kwSK0 = kwLAG + 1, &
        kwJDX = kwSK0 + 1, kwJDPX = kwJDX + 1, kwJDY = kwJDPX + 1, kwJDPY = kwJDY + 1, &
        kwK11 = kwJDPY + 1, kwK12 = kwK11 + 1, kwK13 = kwK12 + 1, &
        kwK14 = kwK13 + 1, kwSK11 = kwK14 + 1, kwSK12 = kwSK11 + 1, kwSK13 = kwSK12 + 1, &
        kwSK14 = kwSK13 + 1, kwV1 = kwSK14 + 1, &
        kwZX = kwV1 + 1, kwZPX = kwZX + 1, kwZY = kwZPX + 1, kwZPY = kwZY + 1, &
        kwXANGLE = kwZPY + 1, kwPZ = kwXANGLE + 1, kwEMIZ = kwPZ + 1, &
        kwSTURN = kwEMIZ + 1, kwBSTRL = kwSTURN + 1, &
        kwDX1 = kwBSTRL + 1, kwDX2 = kwDX1 + 1, kwDY1 = kwDX2 + 1, kwDY2 = kwDY1 + 1, &
        kwD11 = kwDY2 + 1, kwD12 = kwD11 + 1, kwD13 = kwD11 + 2, &
        kwD14 = kwD11 + 3, kwD15 = kwD11 + 4, kwD16 = kwD11 + 5, &
        kwD21 = kwD11 + 6, kwD22 = kwD21 + 1, kwD23 = kwD21 + 2, &
        kwD24 = kwD21 + 3, kwD25 = kwD21 + 4, kwD26 = kwD21 + 5, &
        kwD31 = kwD21 + 6, kwD32 = kwD31 + 1, kwD33 = kwD31 + 2, &
        kwD34 = kwD31 + 3, kwD35 = kwD31 + 4, kwD36 = kwD31 + 5, &
        kwD41 = kwD31 + 6, kwD42 = kwD41 + 1, kwD43 = kwD41 + 2, &
        kwD44 = kwD41 + 3, kwD45 = kwD41 + 4, kwD46 = kwD41 + 5, &
        kwD51 = kwD41 + 6, kwD52 = kwD51 + 1, kwD53 = kwD51 + 2, &
        kwD54 = kwD51 + 3, kwD55 = kwD51 + 4, kwD56 = kwD51 + 5, &
        kwD61 = kwD51 + 6, kwD62 = kwD61 + 1, kwD63 = kwD61 + 2, &
        kwD64 = kwD61 + 3, kwD65 = kwD61 + 4, kwD66 = kwD61 + 5, &
        kwB11 = kwD61 + 6, kwB12 = kwB11 + 1, kwB13 = kwB11 + 2, &
        kwB14 = kwB11 + 3, kwB15 = kwB11 + 4, kwB16 = kwB11 + 5, &
        kwB22 = kwB11 + 6, kwB23 = kwB11 + 7, kwB24 = kwB11 + 8, &
        kwB25 = kwB11 + 9, kwB26 = kwB11 + 10, kwB33 = kwB11 + 11, kwB34 = kwB11 + 12, &
        kwB35 = kwB11 + 13, kwB36 = kwB11 + 14, kwB44 = kwB11 + 15, kwB45 = kwB11 + 16, &
        kwB46 = kwB11 + 17, kwB55 = kwB11 + 18, kwB56 = kwB11 + 19, kwB66 = kwB11 + 20, &
        kwR11 = kwB11 + 21, kwR12 = kwR11 + 1, kwR13 = kwR11 + 2, &
        kwR14 = kwR11 + 3, kwR15 = kwR11 + 4, kwR16 = kwR11 + 5, &
        kwR22 = kwR11 + 6, kwR23 = kwR11 + 7, kwR24 = kwR11 + 8, &
        kwR25 = kwR11 + 9, kwR26 = kwR11 + 10, kwR33 = kwR11 + 11, kwR34 = kwR11 + 12, &
        kwR35 = kwR11 + 13, kwR36 = kwR11 + 14, kwR44 = kwR11 + 15, kwR45 = kwR11 + 16, &
        kwR46 = kwR11 + 17, kwR55 = kwR11 + 18, kwR56 = kwR11 + 19, kwR66 = kwR11 + 20, &
        kwKx = kwR11 + 21, kwQy = kwkx + 1, kwpole = kwQy + 1, &
        kwFBx = kwpole + 1, kwFBy = kwFBx + 1, kwDBZ = kwFBy + 1, & 
        kwK15 = kwDBZ + 1, kwK16 = kwK15 + 1, kwK17 = kwK16 + 1, &
        kwK18 = kwK17 + 1, kwK19 = kwK18 + 1, kwK20 = kwK19 + 1, &
        kwK21 = kwK20 + 1, kwSK15 = kwK21 + 1, kwSK16 = kwSK15 + 1, kwSK17 = kwSK16 + 1, &
        kwSK18 = kwSK17 + 1, kwSK19 = kwSK18 + 1, kwSK20 = kwSK19 + 1, &
        kwSK21 = kwSK20 + 1, kwV20 = kwSK21 + 1, kwV11 = kwV20 + 1, kwV02 = kwV11 + 1, &
        kwJDZ = kwV02 + 1, kwJDPZ = kwJDZ + 1, kwOFFSET = kwJDPZ + 1, &
        kwCOUPLE = kwOFFSET + 1, kwDDP = kwCOUPLE + 1, kwRADI = kwDDP + 1, & 
        kwDPHI = kwRADI + 1, kwW1 = kwDPHI + 1, kwDROT = kwW1 + 1, &
        kwAE1 = kwDROT + 1, kwAE2 = kwAE1 + 1, kwFB1 = kwAE2 + 1, kwFB2 = kwFB1 + 1, &
        kwLDEV = kwFB2 + 1, kwLRAD = kwLDEV + 1, kwFL = kwLRAD + 1, kwAPHI = kwFL + 1, & 
        kwF1K1F = kwAPHI + 1,  kwF2K1F = kwF1K1F + 1, &
        kwF1K1B = kwF2K1F + 1, kwF2K1B = kwF1K1B + 1, &
        kwDVOLT = kwF2K1B + 1, kwPROF = kwDVOLT + 1, &
          kwNPARAM = kwPROF + 1, kwMAX = kwNPARAM + 1
    integer(kind=4), allocatable :: kytbl(:, :)
    integer(kind=4), pointer :: kyindex(:, :), kyindex1(:, :)
    integer(kind=4) INDMAX

    contains
    subroutine initkyindex
        use tfcode
        use maccbk, only:pname
        implicit none
        integer(kind=4) i, k, id
        INDMAX = 0
        do i = icDRFT, icMXEL
            INDMAX = max(INDMAX, kytbl(kwMAX, i))
        end do
        allocate (kyindex(0:INDMAX, 0:icMXEL))
        allocate (kyindex1(0:INDMAX, 0:icMXEL))
        kyindex = 0
        kyindex1 = 0
        do i = icDRFT, icMXEL
            do k = 1, kwMAX-2
                id = kytbl(k, i)
                if (id .ne. 0) then
                    if (kyindex(id, i) .eq. 0) then
                        kyindex(id, i) = k
                    else if (kyindex1(id, i) .eq. 0) then
                        kyindex1(id, i) = k
                    else
                        write(*, *)'Too many aliases ', pname(kytbl(0, i)), ' ', pname(kytbl(k, 0))
                        call abort
                    end if
                end if
            end do
        end do
        return
    end subroutine initkyindex
end module mackw

module macttyp
    integer(kind=4), parameter :: ttypNM = 4, ttypID = 2, ttypEF = -1, ttypDM = 1, ttypST = 8, ttypLS = 16
end module macttyp

module macvar
    integer(kind=4), parameter :: VarPt = 1, VarLog = 2, VarInt = 4, VarRl = 8, VarStr = 128, VarLst = 256,maxv = 20
end module macvar

module macfile
    integer(kind=4), parameter :: STDERR = 6, STDIN = 5, STDOUT = 6, STDPLT = 8, STDLST = 21, MAXLLEN = 256
    integer(kind=8) inflpt
    integer(kind=4) errfl, infl, outfl, pltfl, msglvl, lstfl
    integer(kind=4) pbuf
    character buf(MAXLLEN)
end module macfile

module macmisc
    real(kind=8), parameter :: EPS  =  1.0d-7
    character(kind=1), parameter :: LCURL = '{', RCURL = '}', LPAR = '(',RPAR = ')', COMMA = ',', &
            SEMIC = ';', EQCHR = ' = ', MINUS = '-', PLUS = '+', STAR = '*'
end module macmisc

module cbkmac
    use macfile
    use macmisc
    use macttyp
    use maccode
    use mackw
end module cbkmac