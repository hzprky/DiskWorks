; ---------------------------------------------------
; * Module:         DWX
; * Purpose:        DWX is an operating system tool for
; *                 the Sharp PC-1600. It mainly allows
; *                 to manage files, transfer data and
; *                 files from and to the pocket computer.
; *                 It can start BASIC and machine language
; *                 programs. All activities can be
; *                 performed with any interface as is:
; *                 serial, RAM drive (S1:, S2: S3:) and
; *                 CE-160F (floppy disk).
; *                 This version is re-engineered from the
; *                 least available binary version, as i lost
; *                 the source code of that version but wantet
; *                 to develop the program further.
; *
; *
; *
; *
; * Description:    the complete code is written in Z80 assembler.
; *                 I started developing in the late 80s and finished
; *                 the public version in 1992.
; *                 This version was disassembled with the help of
; *                 z80dasm and then i started to correct, re-understand
; *                 and make it work as initially designed.
; *                 With this code comes a complete user documentation
; *                 and the binary program for the Sharp PC-1600. The
; *                 complete package can be found at github:
; *                 URL:
; *                 The software is published under the GNU license and should
; *                 be delivered with the package.
; *                 Have fun using the software and code, please respect
; *                 my intellectual property and mention me with your
; *                 versions.
; *
; * Version:        DWX25.1   (re-developed 2025, first version)
; * Date:           17.06.2025
; * Changes:        initial version amended from disassembler
; * Release ref.:   DWX.BIN v3.0pre
; * Assembler:      ZASM 4.4
; * Author:         Christian Becker aka XianBild/XianSoft
; * Copyright:      2025 Christian Becker
; * License:        GNU...
; ---------------------------------------------------
;
; include all neccessary definitions etc.
#include "df.asm"
; -------------------------------------------------------------------------
;
; assemble to binary file for PC-1600
#target bin
;
; start address of the program code
#define STARTADR $D000
;
; if 'public' version to be assembled then with start info screen
#define PUBLIC  1    ; change to 0 if private
;#define PUBLIC  0   ; so it's private
;
; -----------------------------------------------------------------
;#define HEADER 0    ;fuer PockEmul und um Filegröße zu ermitteln
#define HEADER 1    ;fuer PC-1600
; -----------------------------------------------------------------
;
; internal structure of the file header
;
; no header for ascii files, simply ascii codes, line terminated
; with CR+LF ($0D, $0A)
; end of file is $1A
;
; header for BASIC and machine language programs
; position  : content and explanation
; -----------------------------------
; +00-03    : $FF $10 $00 $00 (starter)
; +04       : $10: machine language program
;             $21: tokenised basic program
; +05/      : length mod 65536d
; +07       : length \65536d
; +08-0D    : only ml-programs otherwise 0
; +08/      : load address of ml program
; +0A       : bank
; +0B       : autostart address or (if none) $FFFF
; +0D       : bank of autostart oder $FF
; +0E-0F    : $00 $0F
; +10++     : program data
; -----------------------------------

;
#IF HEADER = 1          ; header is needed for normal load from any drive
                        ; so 1 for load on 'real' PC-1600
                        ;    0 for load on emulated PC-1600
                        ; see further up in this file
#Code DWX,STARTADR-16   ; 16 bytes for the header
;
; header indicating a machine language program
    defb $FF,$10,$00,$00    ; starter
    defb $10                ; machine language program
    ;
; lenght of program
    ; low byte and highbyte order
    defw ENDADR-STARTADR+1
    ; bank
    defb $00
; load address of program
    ; low byte and highbyte order
    defw STARTADR
    ; bank
    defb $00
; (auto) start address
    ; low byte and highbyte order
    defw ASTART
    ; bank
    defb $00
; end of header
    defb $00,$0F
;
#ELSE                 ; header is not needed for PockEmul
#Code M.BIN,STARTADR
#ENDIF
;
;
        ; z80dasm 1.1.6
        ; command line: z80dasm --origin=0xD000 -l -v -t DW2DISS

          org  0d000h

ASTART:   ; program start point after loading from disk
          jp PRGINI    ; this is the starting point after load from disk

          ; fill up until &D00E - just an easteregg
          ; ... nothing which makes really sense
          DEFB 00,00,00,00,00,00,00,00,00,00,00,00
          ; so that next jp is exactly on &D00F (doof in German means "stupid")
LD00F:    jp MN_ENTR    ; jump to the menu after the init process

INPUT:
          push hl
          push de
          push bc
          ex de,hl
          call PRTSTR
          ex de,hl
          call CRSRSET

          ld hl,ASTR
          push hl
          ld de,ASTR+1
          ld bc,&000F
          ld (hl),CR
          ldir

          pop hl
          ld b,&00

IP_01:
          call INKEY2
          cp CR
          jr z,IP_EX
          cp BS
          jr z,IP_04
          cp MODE
          jr z,IP_EX1
          ld (hl),a
          push hl
          push bc
          call PRTAKKU
          pop bc
          pop hl
          inc hl
          inc b
          ld a,b
          cp &0F
          jr z,IP_EX
          jr IP_01
IP_04:    ; backspace
          ld a,b
          or a
          jr z,IP_01
          dec hl
          ld (hl),CR
          dec b
          ld a,(CURX)
          dec a
          ld (CURX),a
          push af
          ld a," "
          call PRTAKKU
          pop af
          ld (CURX),a
          jr IP_01

IP_EX:    ; valid input
          scf
          ccf
          pop bc
          pop de
          pop hl
          ret

IP_EX1:   ; invalid input
          scf
          pop bc
          pop de
          pop hl
          ret

; following functions are to configure the serial interface to
; your needs.
INITCOM:
          call MENLIN
          PRINT BAUDR

ITC_01:
          call INKEYC
          ret c
          ld hl,SERCON1
          cp F2             ; function key F2 9600 baud
          ld de,00008h
          jr z,ITC_02
          cp F3             ; function key F3 4800 baud
          ld de,00010h
          jr z,ITC_02
          cp F4             ; function key F4 2400 baud
          ld de,00020h
          jr z,ITC_02
          cp F5             ; function key F5 1200 baud
          ld de,00040h
          jr z,ITC_02
          cp F6             ; function key F6 300 baud
          ld de,&0100
          jr z,ITC_02
          jr ITC_01

ITC_02:
          ld (hl),e
          inc hl
          ld (hl),d
          call MENLIN
          PRINT PAR_TXT

ITC_03:
          call INKEYC
          ret c
          ld hl,SERCON2
          cp F6             ; function key F6 - no partity
          res 4,(hl)
          jr z,ITC_06
          cp F5             ; function key F5 - parity odd/even
          jr nz,ITC_03

          call MENLIN
          PRINT PAR_EVOD

ITC_04:
          call INKEYC
          ret c
          ld hl,SERCON2
          cp F5             ; function key F5 - parity EVEN
          jr nz,ITC_05
          set 5,(hl)
          jr ITC_06

ITC_05:
          cp F6              ; function key F6 - parity ODD
          jr nz,ITC_04
          res 5,(hl)

ITC_06:
          call MENLIN
          PRINT BITS_TXT

ITC_07:
          call INKEYC
          ret c
          ld hl,SERCON2
          cp F3             ; function key F3 - 5 bits
          jr nz,ITC_08
          res 3,(hl)
          res 2,(hl)
          jr ITC_11

ITC_08:
          cp F4             ; function key F4 - 6 bits
          jr nz,ITC_09
          res 3,(hl)
          set 2,(hl)
          jr ITC_11

ITC_09:
          cp F5             ; function key F5 - 7 bits
          jr nz,ITC_10
          set 3,(hl)
          res 2,(hl)
          jr ITC_11

ITC_10:
          cp F6             ; function key F6 - 8 bits
          jr nz,ITC_07
          set 3,(hl)
          set 2,(hl)

ITC_11:
          call MENLIN
          PRINT STPBTXT

ITC_12:
          call INKEYC
          ret c
          ld hl,SERCON2
          res 1,(hl)
          cp F5             ; function key F5 - 1 stopbit
          jr nz,ITC_13
          res 0,(hl)
          jr ITC_14

ITC_13:
          cp F6             ; function key F6 - 2 stopbits
          jr nz,ITC_12
          set 0,(hl)

ITC_14:
          call MENLIN
          PRINT XOXOTXT

ITC_15:
          call INKEYC
          ret c
          ld hl,SERCON2
          cp F5             ; function key F5 - Xon/Xoff on
          jr nz,ITC_16
          set 6,(hl)
          jr ITC_17

ITC_16:
          cp F6             ; function key F6 - Xon/Xoff off
          jr nz,ITC_15
          res 6,(hl)

ITC_17:
          ld a,(SERCON2)
          bit 3,(hl)
          jr z,ITC_20
          bit 2,(hl)
          jr nz,ITC_20

          call MENLIN
          PRINT SHFT_IO

ITC_18:
          call INKEYC
          ret c
          ld hl,SERCON2
          cp F5             ; function key F5 - Shift In/Out on
          jr nz,ITC_19
          set 7,(hl)
          jr z,ITC_20

ITC_19:
          cp F6             ; function key F6 - Shift In/Out off
          jr nz,ITC_18
          res 7,(hl)

ITC_20:   ; d220h
          ld c,SETCOM
          ld d,$01
          ld hl,SERCON1
          call SERIELL
          ld c,SETDEV
          xor a
          jp SERIELL

SERCON1:  DEFB &08,&00

SERCON2:  DEFB &4C

SHOWBL:   ; show block
          ; from $F400 in HEX/ASCII
          push hl
          push de
          ld hl,TRDATA      ; $F400 transfer data area

SB_01:
          call SHOWLI
          xor a
          ld (CURX),a
          ld a,(CURY)
          inc a
          ld (CURY),a
          cp 4
          jr nz,SB_03
          dec a
          ld (CURY),a
          push hl
          push de
          push bc
          ld de,$1903
          call CRSRSET
          call INKEY2
          cp MODE
          jr z,SB_E1
          call UPSCRL
          ld de,$0003
          call CRSRSET
          pop bc
          pop de
          pop hl

SB_03:
          ld a,h
          cp &F4
          jr z,SB_01

SB_EX:
          pop de
          pop hl
          ret

SB_E1:
          scf
          pop bc
          pop de
          pop hl
          jr SB_EX

SHOWLI:   ; SHOW LINE
          push bc
          ld b,6
          push hl
          push bc

SL_01:
          ld a,(hl)
          call AOUT
          ld a," "
          call PRTAKKU
          inc hl
          djnz SL_01

          pop bc
          pop hl

SL_02:
          ld a,(hl)
          call PRTAKKU
          inc hl
          djnz SL_02
          pop bc
          ret

HEXDUMP:  ; function called VIEW in old DW 1.x
          ld a,(COUNT)
          or a
          ret z
          call CLS          ; clear screen
          ld de,&0000
          call CRSRSET      ; cursor to to left corner
          call SV_FMSK
          ld a,(FPTR)       ; get current file
          push af
          dec a
          call GFNADR       ; get the file informtaion
          ld de,FMASK
          ld bc,&000C       ; 12bytes to copy
          ldir
          ld hl,TRDATA
          ld (TRANAD),hl
          call OPENFF       ; open the file
          jr c,HXD_EX

HXD_01:
          ld de,FCB
          ld c,RD_FL        ; read the file
          call FILE
          jr c,HXD_EX
          call SHOWBL
          jr c,HXD_EX
          jr HXD_01

HXD_EX:
          call CLOSE1       ; close the file
          call LD_FMSK
          call RDSRC
          call CLS
          pop af
          ld (FPTR),a       ; filepointer restored
          ret


PROTECT:  ; set write portection attribute
          ld a,(COUNT)
          or a
          ret z

          call SV_FMSK
          ld a,(FPTR)
          push af
          dec a

          call GFNADR
          ld de,FMASK
          ld bc,0000ch
          ldir
          call SETFCB

          ld a,(FCB+&14)
          cp $20          ; unprotected?
          jr z,PT_01      ; yes then protect
          ld a,$20        ; else unprotect
          jr PT_02

PT_01:
          ld a,$21        ; unprotect

PT_02:
          ld (FCB+&14),a
          ld de,FCB
          ld c,SETATR
          call FILE       ; and set the attribute
          call c,FEHLER
          call LD_FMSK
          call RDSRC
          pop af
          ld (FPTR),a
          ret

BATTI:    ; test internal battery
          push de
          ld h,&00
          ld b,16           ; 16 measurements

BT_01:
          push bc
          push hl
          call TIMER
          ld d,$0000
          ld e,a
          pop hl
          add hl,de
          pop bc
          djnz BT_01

          ld b,004h         ; devide by 16 (#measurements)

BT_02:
          srl h
          ld a,l
          rra
          ld l,a
          djnz BT_02

          push hl
          pop de
          ld hl,00000h
          adc hl,de
          adc hl,de
          adc hl,de
          pop de
          scf
          ccf
          sbc hl,de
          ret

INFO:
          push hl
          push de
          push bc

          call CLS

          ld a,(QUELLE)
          cp "C"
          jr z,INF_03   ; if C=COM then only date/time/I and E
          CP "3"
          jr z,INF_S3   ; if S3: then only filemask, date/time/I and E

          ; drive and free space on drive
          call DSKF
          PRINTAT &0000,ANZERG
          call PRTSTR

          ld a,(QUELLE)
          call Q_TEXT
          call PRTSTR
          PRINT ERGEB-1
          PRINT ANZBYT

INF_S3:
          ;Number of files and number
          ;of marked files
          PRINTAT &0101,AF_TXT

          ld a,(COUNT)
          ld l,a
          ld h,$00
          ld de,BSTR        ; B$
          call STRCNV       ; convert HL into string (DE)
          xor a
          ld (de),a
          PRINT BSTR

          ld a,"("
          call PRTAKKU

          ld a,(MARKED)
          ld l,a
          ld h,$00
          ld de,BSTR        ; B$
          call STRCNV
          xor a
          ld (de),a
          PRINT BSTR

          ld a,")"
          call PRTAKKU

          ; ----------------
          ;files mask'FILEMASKE
INF_03:
          PRINTAT &0102,MI_TXT
          ld b,&08
          ld de,FMASK

INF_01:   ; print filemask
          ld a,(de)
          call PRTAKKU
          inc de
          djnz INF_01

          ld a,"."
          call PRTAKKU
          ld b,$03

INF_02:   ; print extension of filemask
          ld a,(de)
          call PRTAKKU
          inc de
          djnz INF_02
          ; --------------------------------

          ; date and time
          call TIME
          PRINTAT &0103,BSTR

          ; internal battery
          PRINTAT &0E03,POWERI
          ld de,528
          ld c,&18
          call BATTI
          ld de,$0004
          sbc hl,de
          ld de,BSTR
          call STRCNV
          xor a
          ld (de),a
          PRINT BSTR
          ld a,"%"
          call PRTAKKU
          ld a,(ROMBITS)
          cp $10
          jr z,INF_EX

          call ISDISK
          jr nc,INF_EX ; 1600F not attached

          PRINT POWERE
          ld de,550
          ld c,&1A
          call BATTI
          ld de,$0007
          sbc hl,de
          ld de,BSTR
          call STRCNV
          xor a
          ld (de),a
          PRINT BSTR
          ld a,"%"
          call PRTAKKU

INF_EX:
          call INKEY2
          call CLS
          pop bc
          pop de
          pop hl
          ret

TIME:     ; get current date and time
          ; and store in B$ terminated by $00
          ld hl,ASTR
          ld c,TMREAD
          call TIMER

          ld de,BSTR
          ld hl,ASTR+1      ; day BCD
          ld a,(hl)
          call BCDASC
          ld a,"."
          ld (de),a
          inc de
          dec hl

          push hl           ; month binary
          ld l,(hl)
          ld h,&00
          call STRCNV
          pop hl
          ld a,"."
          ld (de),a
          inc de
          ld a," "
          ld (de),a
          inc de
          inc hl
          inc hl
          ld a,(hl)
          call BCDASC
          ld a,":"
          ld (de),a
          inc de
          inc hl
          ld a,(hl)
          call BCDASC
          ld (de),a
          xor a
          ld (de),a
          ret

BCDASC:   ; convert BCD to ascii
          push af
          and &F0
          rrca
          rrca
          rrca
          rrca
          call BINASC
          ld (de),a
          inc de
          pop af
          and &0F
          call BINASC
          ld (de),a
          inc de
          ret


KILL: 		; delete (KILL) files
          ld a,(COUNT)
          or a
          ret z
          ld a,(MARKED)
          or a
          jp z,KLWLD

          call SV_FMSK
          call MENLIN
          ld de,BES_TXT
          call PRINTS
          call YESNO
          ret z

          jr nc,KL_01
          ld a,001h
          ld (BES_TAG),a
          jr KL_02

KL_01:
          xor a
          ld (BES_TAG),a

KL_02:
          call MENLIN
          ld de,DEL_TX
          call PRINTS
          xor a

KL_03:
          ld b,a
          ld a,(COUNT)
          sub b
          jr z,DEL_EX
          ld a,b
          inc a
          call GFNADR
          dec hl
          ld b,a
          xor a
          or (hl)
          ld a,b
          jr z,KL_03

          call SHOWDIR

          call ASK_YN
          jr nc,KL_04
          push af
          ld de,&0F00
          call CRSRSET
          ld de,JANEIN
          call PRINTS
          pop af
          call YESNO
          jr z,DEL_E1
          jr nc,KL_03

KL_04:
          call DEL_FL
          jr c,DEL_E1
          call MARK
          jr KL_03

DEL_EX:
          call SETWLD
          call SV_FMSK
          call RDSRC
          jr nc,DEL_E1
          call CLS

DEL_E1:
          call LD_FMSK
          ld de,&0E00
          call CRSRSET
          ld b,14
          jp BLANKS

DEL_FL:   ; delete file
          ; file number in A
          push hl
          push de
          push bc
          push af
          ld de,FCB
          ld c,CR_FC
          call FILE
          jr c,DF_ER
DF_01:
          pop af
          push af
          call SETFNAME
          call SETFCB
          ld c,DL_FL
          call FILE
          jr nc,DF_EX
DF_ER:
          call FEHLER
          pop af
          scf
          jr DF_E1

DF_EX:
          pop af
          scf
          ccf

DF_E1:
          pop bc
          pop de
          pop hl
          ret

SETFNAME:
          push hl
          push de
          push bc
          call GFNADR
          ld de,FBFLEN
          scf
          ccf
          sbc hl,de         ; HL=address of filename
                            ; source
          ld de,FMASK       ; target
          ld bc,&000C       ; #bytes
          ldir              ; copy
          pop bc
          pop de
          pop hl
          ret

ASK_YN:
          push af
          ld a,(BES_TAG)
          or a
          jr z,NO
          jr YES


YESNO:    ; only Yes or No [Y/N] and MODE
          ; all registers untouched
          ; YES  = SC, NZ
          ; NO   = NC, NZ
          ; MODE = NC, SZ

          push af
YN_01:
          call INKEY2
          cp "Y"
          jr z,YES
          cp "N"
          jr z,NO
          cp MODE
          jr z,NOT
          jr YN_01

NO:				; no
          pop af
          or a
          ret

YES:			; yes
          pop af
          scf
          ret

NOT:			; MODE key pressed
          pop af
          xor a
          or a
          ret

SV_FMSK:  ; save file mask
          push hl
          push de
          push bc
          ld hl,FMASK
          ld de,FMASK1
          ld bc,FNLEN
          ldir
          pop bc
          pop de
          pop hl
          ret

LD_FMSK:
          push hl
          push de
          push bc
          ld hl,FMASK1
          ld de,FMASK
          ld bc,FNLEN
          ldir
          pop bc
          pop de
          pop hl
          ret

KLWLD:		; kill (delete) with wildcards

          call MENLIN
          ld hl,KLTX01
          call PRINTS
          ld de,$0603
          ld bc,$0600
          call INPUT
          ret c

          ld hl,ASTR          ; A$
          ld a,(hl)
          cp CR
          ret z

          PRINT KLTX01
          call YESNO
          ret z
          ret nc

          ld hl,ASTR          ; A$
          ld a,(hl)
          cp CR
          ret z

          call SETFCB
          ld de,FCB+&09
          ld b,&08
          call NWFNS
          ld de,FCB
          ld c,DL_FL          ; delete file command
          call FILE
          jr nc,KLWLD1
          jp FEHLER

KLWLD1:
          call SETWLD
          jp RDSRC

DSKF:     ; get free space on disk (not on S3: and COM1:)
          call SETFCB
          ld de,FCB
          ld c,GETALC     ; IOCS function get alloc get inf. about drive
          call XFILE      ; and get it
          or a            ; error?
          jr z,DSKF02     ; no, then go ahead for the calculates
          ld a,(ERRCODE)  ; else go for error handling
          jp FEHLER

DSKF02:
          push bc         ; bytes per sector
          ld d,$00
          push de         ; sectors per cluster
          push hl         ; free cluster
          pop de
          call BIN2BCD
          ld hl,XX
          ld de,YY
          ld bc,$0008
          ldir            ; copy XX->YY
          pop de
          call BIN2BCD
          ld a,$02        ; 2 variables
          ld (ARGCOUNT),a ; into argument counter
          call CMULT      ; and multiply XX*YY->XX
          ld hl,XX
          ld de,YY
          ld bc,$0008
          ldir            ; XX-> YY

          pop de          ; bytes per sector
          call BIN2BCD
          ld a,&02        ; 2 operators
          ld (ARGCOUNT),a ; into argument counter
          call CMULT      ; and multiply xx*YY->XX
          ld hl,XX
          ld de,ERGEB
          jp CGETR        ; and get reult into (DE)

FORMAT:   ; format disk (X)
          call ISDISK
          jr c,FRM_02     ; no error, then continue
          ld a,155        ; invalid drive name
          jp FEHLER

FRM_02:
          call MENLIN
          ld de,FRMTTX
          call PRINTS
          ld de,&0003
          ld a,26
          call RVSCHR     ; invert 26 chars
          call YESNO
          jr c,FRM_01     ; continue on "Y"
          ret

FRM_01:
          ld a,&01        ; = drive "X:"
          ld c,&83        ; format
          rst 20h         ; BANKCALL
          DEFB &05
          DEFW &4008
          ret nc          ; return on no error
DSK_ER:   ; error in direct call from disk ROM
          cp &08
          jr z,ER_NF
          cp &20
          jr z,ER_WP
          cp &40
          jr z,ER_AE
          cp &80
          jr z,ER_ND

ER_OUT:
          jp FEHLER

ER_NF:
          ld a,161
          jr ER_OUT

ER_WP:
          ld a,159
          jr ER_OUT

ER_AE:
          ld a,168
          jr ER_OUT

ER_ND:
          ld a,160
          jr ER_OUT

DATELN:   ; copy date and length
          push bc
          push hl
          push de
          ld bc,00010h
          add hl,bc
          ld a,(hl)
          inc hl
          ld h,(hl)
          ld l,a
          ld b,$00
          ld a,h
          cp $28          ; "("
          jr nc,DL_06
          cp $28
          jr nz,DL_02
          ld a,l
          cp $10
          jr nc,DL_06

DL_02:
          inc b
          ld a,h
          cp $04
          jr nc,DL_05
          cp $03
          jr nz,DL_03
          ld a,l
          cp $E8
          jr nc,DL_05

DL_03:
          inc b
          ld a,h
          cp $01
          jr nc,DL_05
          or a
          jr nz,DL_04
          ld a,l
          cp $64
          jr nc,DL_05

DL_04:
          inc b
          cp $0A
          jr nc,DL_05
          inc b

DL_05:
          ld a," "
          ld (de),a
          inc de
          djnz DL_05

DL_06:
          call STRCNV
          pop de
          ex de,hl
          ld bc,$0005
          add hl,bc
          xor a
          ld (hl),a
          pop hl
          pop bc
          ret


; -------------------------------------------------------------
; load the config file
; -------------------------------------------------------------
LOADCFG:  ; load quickstart/macro definitions
          ; data from DW.CFG (from default drive)
          ; delete memory area first

          ld hl,CFGDATA
          ld de,CFGDATA+1
          ld bc,ASTART-CFGDATA-1
          ld (hl),&00
          ldir

          ld hl,CFGNAME
          ld de,FMASK
          ld bc,FNLEN
          ldir

          ; open the file now as all data is
          ; properly set
          call OPENFF
          ret c           ; return on error (C-flag set)

          ; area in RAM for the config data
          ld hl,CFGDATA-&0100

LDCFG_01: ; the read cycle
          ld de,&0100       ; 256 byte
          add hl,de
          ld (TRANAD),hl
          ld c,RD_FL        ; IOCS command for sequential read
          ld de,FCB
          call FILE
          jr nc,LDCFG_01    ; continue on no error

          cp 162            ; Error 162 - end of file reached
          jr z,LDCFG_EX

          call CLOSE1       ; close file and
          jp FEHLER         ; jump to error handling (152=leave dwx)

LDCFG_EX:                   ; close without error
          call CLOSE1
          ret                 ; and return to main loop

; ----------------------------------------------------------------------
; the following function checks if the incoming key from the main
; menu loop is a defined macro code and works them through accordingly
MMKCHK:
          ld b,a
          ld hl,CFGDATA-1
          inc hl
          ld a,(hl)
          cp TRENNER
          ret nz

MMKC_01:
          inc hl
          ld a,(hl)
          cp b
          jr z,MMKC_03

MMKC_02:
          inc hl
          ld a,(hl)
          cp &0A            ; LF?
          jr z,MMKC_01
          cp TRENNER
          ret z
          jr MMKC_02

MMKC_03:
          inc hl
          inc hl
          ld de,HEADERCV      ; header for receive data
          push de
          ld c,000h

MMKC_04:
          ld a,(hl)
          cp 07eh             ;
          jr nz,MMKC_05
          ld a,00dh
          jr MMKC_07

MMKC_05:
          cp 05ch
          jr nz,MMKC_06
          inc hl
          ld a,(hl)
          cp 05ch
          jr z,MMKC_07
          cp 040h
          jr z,MMKC_09
          call MMK_EXEC
          jr MMKC_07

MMKC_06:
          cp 00dh
          jr z,MMKC_08
          cp 05eh
          jr z,MMKC_08
MMKC_07:
          ld (de),a
          inc hl
          inc de
          inc c
          jr MMKC_04
MMKC_08:
          pop de
          ld a,c
          call 0016ch
          ret
MMKC_09:
          push af
          push hl
          push de
          ld a,(QUELLE)
          call Q_TEXT
          ex de,hl
          pop de
          ld b,004h
          call MMKC_10
          ld a,03ah
          ld (de),a
          inc de
          inc c
          ld a,(FPTR)
          dec a
          call GFNADR
          ld b,&08        ; filename length
          call MMKC_10
          ld a,"."        ; separator for file extension
          ld (de),a
          inc de
          inc c
          ld b,&03        ; extension length
          call MMKC_10
          pop hl
          pop af
          inc hl
          jr MMKC_04

MMKC_10:
          ld a,(hl)
          inc hl
          cp &20          ; blank?
          jr z,MMKC_11
          or a
          jr z,MMKC_11
          ld (de),a
          inc de
          inc c

MMKC_11:
          djnz MMKC_10
          ret


MMK_EXEC:
          push de
          push bc
          push hl
MME_01:
          inc hl
          ld a,(hl)
          cp 00dh
          jr z,MME_03
          cp 05eh
          jr z,MME_03
          or a
          jr z,MME_02
          cp 05ch
          jr nz,MME_01
          xor a
          ld (hl),a
MME_02:
          pop hl
          call ASCBIN
          ld a,e
          scf
          ccf
          jr MME_04

MME_03:
          scf
          pop hl
MME_04:
          pop bc
          pop de
          ret


RENAME:
          ld a,(COUNT)
          or a
          ret z
          ld a,(MARKED)
          or a
          jp z,NAMWLD

          call SV_FMSK
          xor a

NM_01:
          ld b,a
          ld a,(COUNT)
          sub b
          jr z,NM_EX1
          ld a,b
          inc a
          call GFNADR
          dec hl
          ld b,a
          xor a
          or (hl)
          ld a,b
          jr z,NM_01            ; next file

          push af               ; rename
          call SHOWDIR
          call MENLIN
          ld hl,NM_TXT          ; "name as"
          ld de,$0803
          call INPUT            ; HL and DE get swapped in INPUT routine
          jr c,NM_EX

          ld hl,ASTR            ; A$
          ld a,(hl)
          cp CR
          jr z,NM_EX

          pop af
          push af
          dec a
          call GFNADR           ; HL = sourcefile
          ld de,FMASK
          ld bc,FNLEN
          ldir
          call SETFCB

          ; blank A$ first
          ld hl,FCB+&29
          ld de,FCB+&2A
          ld bc,FNLEN-1
          ld a," "
          ld (hl),a
          ldir

          ld hl,ASTR            ; A$
          ld de,FCB+&29
          call SETFN
          jr c,NM_EX

          call COMPHD
          jr c,NM_EX

          ;Rename file
          ld c,NM_FL
          ld de,FCB
          call FILE
          jr nc,NM_02

          ; show error message
          call FEHLER
          jr NM_EX    ;d901  18 07   . .

NM_02:
          pop af
          call MARK
          jp NM_01

NM_EX:
          pop af

NM_EX1:
          call LD_FMSK
          ld a,(FPTR)
          push af
          call RDSRC
          pop af
          ld (FPTR),a
          ret

COMPHD:
          ld hl,FCB+&09
          ld de,FCB+&29
          ld b,FNLEN-1
          call CPHLDE
          scf
          ccf
          ret nz
          ld a,$97
          jp FEHLER

BEEP1:
          push af
          push bc
          ld a,$50
          ld bc,$00FF
          call SOUT
          pop bc
          pop af
          ret

CPHLDE:   ; compare (HL) and  (DE)
          ld a,(de)
          cp (hl)
          ret nz
          inc de
          inc hl
          djnz CPHLDE
          ret

NAMWLD:  ; rename with wildcards
          call MENLIN
          ld hl,NMTX01
          ld de,&0603
          call INPUT
          jr c,NMWD_01

          ld hl,ASTR            ; A$
          ld a,(hl)
          cp CR
          ret z

          call SETFCB

          ld de,FCB+&09
          ld b,$08
          call NWFNS
          ld de,$0003
          ld a,26
          call RVSCHR           ; invert 26 chars
          call UPSCRL           ; scroll up 1 line
          ld de,$0003
          call CRSRSET
          ld hl,NMTX02
          ld de,$0603
          call INPUT
          jr c,NMWD_01
          ld hl,ASTR              ; A$
          ld a,(hl)
          cp CR
          jr z,NMWD_01

          ld de,FCB+&29
          call NWFNS

          call COMPHD
          jr c,NMWD_01
          ld de,FCB
          ld c,NM_FL              ; rename file
          call FILE
          jr nc,NMWD_01
          call FEHLER
NMWD_01:
          call NM_EX1
          ld a,2
          jp DEL1LIN

NWFNS:   ; set filename for rename with wildcards
          ld b,$08
          call SETFN
          ld a,(hl)
          cp "."
          jr nz,NF_01
          inc hl

NF_01:
          ld b,$03
          jp SETFN

RUNIT:    ; start of programs (BASIC and ML)
          ld a,(COUNT)
          or a
          ret z
          call SV_FMSK

          ld a,(FPTR)
          dec a
          call GFNADR
          ld (&F1B7),hl     ; &F1B7 = auto increment of BASIC memory
          ld de,FMASK
          ld bc,FNLEN
          ldir

          call OPENFF
          jp c,RI_10
          ld hl,TRDATA
          ld (TRANAD),hl

          ld c,RD_FL        ; sequential read
          ld de,FCB
          call FILE
          push af
          call CLOSE1
          pop af
          jp c,RI_10

          ld a,(TRDATA)
          cp &FF            ; BASIC or ML?
          jr nz,RI_10       ; no? then return

          call MENLIN
          ld de,RNTX03
          call PRINTS
          call YESNO
          jr c,RI_08
          jr RI_10

RI_08:
          ld a,(QUELLE)
          call Q_TEXT       ; load DE with source drive name (4 byte)
          ex de,hl          ; store de into hl, now (hl)=source drive name
          ld de,RNTX02      ; load de with buffer address
          ld c,&06          ; 6?

RI_07:
          ld a,(hl)
          cp $00
          jr z,RI_06
          ld (de),a
          inc de
          inc hl
          inc c
          jr RI_07

RI_06:
          ld a,":"
          ld (de),a
          inc c
          inc de

          ld hl,(&F1B7) ; fetch file name
          ld b,008h

RI_01:
          ld a,(hl)
          inc hl
          cp " "
          jr z,RI_02
          ld (de),a
          inc c
          inc de

RI_02:
          djnz RI_01
          ld a,"."
          ld (de),a
          inc c
          inc de
          ld b,&03

RI_03:
          ld a,(hl)
          inc hl
          cp " "
          jr z,RI_04
          ld (de),a
          inc c
          inc de
          djnz RI_03

RI_04:
          ld a,(&F404)
          cp 010h
          jr z,MLPRO

BASPRO:   ; BASIC program
          ld a,&22    ; '"'
          ld (de),a
          inc de
          ld a,","
          ld (de),a
          inc de
          ld a,"R"
          ld (de),a
          inc de
          ld a,CR
          ld (de),a
          ld a,c
          add a,003h
          ld de,RNTX04
          jr RI_09

MLPRO:    ; machine language program
          ld a,CR
          ld (de),a
          inc c
          ld a,c
          ld de,RNTX01

RI_09:
          call KBUFSET
          jp EXIT

RI_10:
          jp LD_FMSK


CHDEV:    ; FCB2 for output
          push af
          push bc
          push de
          push hl
          ld a,(ZIEL)         ; read target
          call Q_TEXT
          ld hl,FCB2+1        ; fill FCB2
          ld b,&04

SF2_01:
          ld a,(de)
          ld (hl),a
          inc de
          inc hl
          djnz SF2_01

          pop hl
          pop de
          pop bc
          pop af
          ret

COMCOP:   ; copy from serial port
          call MENLIN
          ld hl,NM_TXT
          ld de,&0309
          call INPUT
          ret c               ; return on carry set (break)
          ld hl,ASTR
          ld a,(hl)
          cp CR
          ret z               ; return on CR -> ok

          ld hl,ASTR
          ld de,FMASK
          call SETFN
          ret c

          call COPYX
          ret nc
          jp FEHLER

CPFD_MN:  ; must be any copy function wrapper or similar
          ; function name is just fantasy
          ld a,$01
          ld (BUFFCPD),a
          call MENLIN
          PRINT CPT1_TXT
          ld a,(FPTR)
          dec a
          call GFNADR
          ld b,$08

CPFD_01:
          ld a,(hl)
          cp " "
          call nz,PRTAKKU       ; print only when not " "
          inc hl
          djnz CPFD_01
          ld a,"."
          call PRTAKKU
          ld b,$03

CPFD_02:
          ld a,(hl)
          cp " "
          call nz,PRTAKKU       ; print only when not " "
          inc hl
          djnz CPFD_02
          ld de,$0003
          ld a,26
          call RVSCHR           ; invert menu bar
          call UPSCRL
          call MENLIN
          ld hl,CPT2_TXT
          ld de,$0603
          call INPUT
          jr c,CPFD_03

          ld hl,ASTR
          ld a,(hl)
          cp CR
          jr z,CPFD_03

          ld de,FMASK1
          call SETFN
          jr c,CPFD_03

          ld a,(FPTR)
          dec a
          call GFNADR
          ld de,FMASK
          ld bc,FNLEN
          ldir
          call COPYX

CPFD_03:
          call CLS
          xor a
          ld (BUFFCPD),a
          jp LD_FMSK

; the file copy funktion
CP_FILE:
          ld a,(QUELLE)
          cp "C"              ; serial
          jp z,COMCOP         ; if serial, then procede further down
          ld a,(COUNT)
          or a
          ret z

          call SV_FMSK
          ld a,(MARKED)
          or a
          jp z,CPFD_MN        ; if no file marked, just the current file

          call MENLIN
          ld a,(QUELLE)
          ld b,a
          ld a,(ZIEL)
          cp b
          jr nz,CPF_01
          ld de,ZGQ_TX        ; target equals source error!
          jp FEAUS

CPF_01:
          PRINT CPTX01          ; disk ready text
          call YESNO
          jr c,CPF_02           ; if disk ready, then go ahead
          ret

CPF_02:
          xor a

CPF_03:
          ld b,a
          ld a,(COUNT)
          sub b
          jr z,CPF_05
          ld a,b
          inc a
          call GFNADR
          dec hl
          ld b,a
          xor a
          or (hl)
          ld a,b
          jr z,CPF_03
          call SHOWDIR
          push af
          dec a
          call GFNADR
          ld de,FMASK
          ld bc,FNLEN
          ldir
          call COPYX
          jr c,CPF_04
          pop af
          call MARK
          jr CPF_03

CPF_04:
          pop af

CPF_05:
          jp LD_FMSK

COPYX:    ; copy file
          ld a,(ZIEL)
          cp "C"
          jr nz,CPX_01
          call MENLIN
          PRINT SERTXT      ; oder evtl. auch NMTX02?
          call YESNO
          jr c,CPX_01
          ret

CPX_01:
          ld hl,TRDATA
          ld (TRANAD),hl
          call OPENFF
          ret c

          call MENLIN
          ld a,(QUELLE)
          cp "C"
          jr z,CPX_02
          ld hl,FCB+9           ; 0f3d0h
          ld de,FCB2+9          ; 0f25fh
          ld bc,&001A
          ldir
          ld a,(BUFFCPD)
          or a
          jr z,CPX_03
          ld hl,FMASK1
          ld de,FCB2+9          ; 0f25fh
          ld bc,FNLEN
          ldir
          jr CPX_03

CPX_02:
          ld hl,FMASK
          ld de,FCB2+9          ; 0f25fh
          ld bc,FNLEN
          ldir

CPX_03:
          call CHDEV
          ld c,CR_FL
          ld de,FCB2
          ld a,&01
          ld (de),a
          call FILE
          jr c,CPX_06

CPX_04:   ; read from file in FCB 1
          push af           ; show progress in progress bar
          ld a,"."          ; "." is read progress
          call PRTAKKU
          pop af
          ld c,RD_FL
          ld de,FCB
          call FILE
          jr nc,CPX_05
          cp 162            ; error 162 end of file reached
          jr z,CPX_07
          ld a,b
          ld a,(QUELLE)
          cp "C"            ; serial interface
          jr nz,CPX_06
          ld a,b
          or a
          jr z,CPX_07
          jr CPX_06

CPX_05:   ; write to file in FCB2
          ld c,WR_FL
          ld de,FCB2
          call FILE
          push af           ; show progress in progress bar
          ld a,(CURX)
          dec a
          ld (CURX),a
          ld a,"o"          ; "o" is write progress
          call PRTAKKU
          pop af
          jr nc,CPX_04

CPX_06:   ; close all and delete target
          ; on error
          ld b,a
          call CLOSEALL
          ld de,FCB2
          ld c,DL_FL
          call FILE
          ld a,b
          call FEHLER
          scf
          jr CPX_09

CPX_07:
          ld a,(FCB+&25)      ; FCB  = $F3C7 : length
          ld (FCB2+&06),a     ; FCB2 = $F256

          ld a,(QUELLE)
          cp "C"
          jr z,CPX_08

          ld a,(FCB+&09+&0B)  ; SETCODE
          ld (FCB2+&09+&0B),a

CPX_08:
          ld hl,FCB+&1F       ; date and time etc.
          ld de,FCB2+&1F
          ld bc,&0004
          ldir

          ld de,FCB2
          call CLOSEF
          ret c

          ld de,FCB
          call CLOSEF
          ret c

CPX_09:
          jp LD_FMSK    ;dc79  c3 aa d5   . . .

          ;----------------------------------
OPENFF:   ; OPEN FILE FROM
          call SETFCB
          ld c,OP_FL
          ld de,FCB
          ld a,&01
          ld (de),a
          call FILE
          ret nc
          jp FEHLER

CLOSEALL: ; close all files
          ld de,FCB2
          call CLOSEF
          ret c                 ; return on error with C-Flag set
CLOSE1:
          ld de,FCB
CLOSEF:
          xor a
          ld (de),a
          push bc
          ld c,CL_FL            ; close file
          call FILE
          pop bc
          ret

FILE:      ; wrapper for file functions call IOCS
          push bc
          push de
          push hl
          call XFILE
          pop hl
          pop de
          pop bc
          and a
          ret z             ; return on z-flag -> no error
          ld a,(ERRCODE)    ; else get error code
          scf               ; and set c-flag
          ret

AOUT:     ; display accumulator as 2 hex digits
          push af
          push bc
          ld c,a
          ld a,$F0
          and c
          rrca
          rrca
          rrca
          rrca
          call BINASC
          call PRTAKKU
          ld a,$0F
          and c
          call BINASC
          call PRTAKKU
          pop bc
          pop af
          ret

BINASC:   ; convert accumulator to ascii
          cp &0A
          jp m,BA_01
          add a,&07

BA_01:
          add a,&30
          ret


; ------------------------------------------------------------
; read directory data from source drive (namely the files)
RDSRC:    ; read data from source drive
          ld a,(QUELLE)
          cp "C"                ; if "C" then COM1:
          jr nz,RS_03
          xor a
          ld (COUNT),a
          ld (MARKED),a
          ld (FPTR),a
          jp CLS

RS_03:    ; warum da drei NOPs sind ist mir unklar
;          nop      ;dd5b  00   .
;          nop      ;dd5c  00   .
;          nop      ;dd5d  00   .
          ld de,FCB                       ; FCB setzen
          ld c,CR_FC                      ; IOCS create fcb
          call FILE
          jr nc,RS_04
          jp FEHLER

RS_04:
          call SETFCB
          ld c,SFIRST             ; IOCS search first command
          call FILE
          jr nc,RS_06             ; no error, then continue at RS_06
          ld hl,COUNT
          ld (hl),000h

RS_05:
          call CLS
          cp 152                  ; error 152 - file not found
          ret z      ;dd7f  c8   .
          jp FEHLER    ;dd80  c3 28 de   . ( .

RS_06:
          ld hl,DIRMEM          ; set transfer addr. / after code
          ld (DMPTR),hl         ; dirmem pointer for operations

          xor a                 ; A=0
          ld (COUNT),a          ; count and
          ld (MARKED),a         ; marked = 0
          inc a                 ; and file pointer on first (1)
          ld (FPTR),a           ; store FPTR

NXT_1:    ; found first or subsequent
          call TFDATA           ; prepare display of first
          call SETFCB
          ld c,SNEXT            ; find next
          call FILE             ; and search
          jr nc,NXT_1           ; if found get the next
          cp 152                ; = 152 file not found
          scf
          ccf                   ; set and complement carry flag

          ret z                 ; return on error 152
          jr RS_05              ; jump to error handling

TFDATA:   ; copy data into display area
          push hl
          push de
          push bc
          push af
          ld a,(DMPTR)          ; to where
          ld l,a
          ld a,(DMPTR+1)
          ld h,a                ; HL=target
          ld de,FCB             ; DE=source
          ex de,hl
          ld bc,FNLEN
          ld a,(de)
          cp KILLED             ; files deleted?
          jr z,TFD_EX           ; then completed

          push de               ; keep destination
          ldir                  ; and transfer

          call DATELN           ; get date and length
          pop de
          ex de,hl
          ld bc,FBFLEN          ; length of the filename
          adc hl,bc             ; add
          dec hl                ; and delete mark
          xor a
          ld (hl),a             ; mark deleted
          inc hl
          ld a,l                ; and...
          ld (DMPTR),a
          ld a,h
          ld (DMPTR+1),a        ; save
          ld a,(COUNT)          ; number of files into A
          inc a                 ; increment number of files
          ld (COUNT),a          ; and save

TFD_EX:
          pop af      ;dde0  f1   .
          pop bc      ;dde1  c1   .
          pop de      ;dde2  d1   .
          pop hl      ;dde3  e1   .
          ret      ;dde4  c9   .


SETFCB:   ; fill FCB with data
          push af
          push hl
          ld a,(QUELLE)
          call Q_TEXT

          ld hl,FCB
          xor a
          ld (hl),a
          inc hl
          ld b,4

SFCB_1:
          ld a,(de)
          ld (hl),a
          inc de
          inc hl
          djnz SFCB_1

          ld de,FCB+&09   ; FILENAME
          ld hl,FMASK
          ld bc,FNLEN
          ldir
          ld de,FCB
          pop hl
          pop af
          ret

Q_TEXT:   ; load DE with source text
          CP "X"
          LD DE,X_TXT
          RET Z
          CP "Y"
          LD DE,Y_TXT
          RET Z
          CP "3"
          LD DE,S3_TXT
          RET Z
          CP "2"
          LD DE,S2_TXT
          RET Z
          CP "1"
          LD DE,S1_TXT
          RET Z
          LD DE,C1_TXT
          RET

FEHLER:
          PUSH AF
          CALL MENLIN

          CP &97      ; 151 file already exists
          LD DE,FT00
          JR Z,FEAUS1

          CP &98      ; 152 file not found
          LD DE,FT01
          JR Z,FEAUS1

          CP &9A      ; 154 file already open
          LD DE,FT02
          JR Z,FEAUS1

          CP &9B      ; 155 wrong drive
          LD DE,FT03
          JR Z,FEAUS1

          CP &9D      ; 157 invalid file name
          LD DE,FT04
          JR Z,FEAUS1

          CP &9F      ; 159 file protected
          LD DE,FT05
          JR Z,FEAUS1

          CP &A0      ; 160 no disk in drive
          LD DE,FT06
          JR Z,FEAUS1

          CP &A1      ; 161 disk not yet formatted
          LD DE,FT07
          JR Z,FEAUS1

          CP &A2      ; 162 reas/write error
          LD DE,FT08
          JR Z,FEAUS1

          CP &A3      ; 163 wrong disk (changed)
          LD DE,FT09
          JR Z,FEAUS1

          CP &A4      ; 164 disk full
          LD DE,FT10
          JR Z,FEAUS1

          CP &A7      ; 167 fatal disk error
          LD DE,FT11
          JR Z,FEAUS1

          CP &A8      ; 168 disk drive error
          LD DE,FT12
          JR Z,FEAUS1

          LD DE,FT13    ; if none of the above then error 99

FEAUS1:
          POP AF

FEAUS:    ; print error message
          push af
          call PRINTS
          ld de,00003h
          ld a,26
          call RVSCHR
          call BEEP1
          call INKEY2
          pop af
          scf
          ret


SWAP:     call SETWLD
          call SV_FMSK
          call SWP_01
          jr c,SWP_01
          ret

SWP_01:
          ld a,(QUELLE)
          push af
          ld a,(ZIEL)
          ld (QUELLE),a
          pop af
          ld (ZIEL),a
;le000h:
          call RDSRC
          ret nc
          ld a,(ERRCODE)
;le007h:
          cp &98            ; ERROR 152 file not found
          scf
          ccf
          ret z
          scf
          ret

C_ZIEL:   ; change target drive (target = ZIEL in German)
          call MENLIN
          PRINT Q_TXT
          ld de,00003h
          call CRSRSET
          PRINT Z_TXT         ; "TAR"
          call Q_IN
          cp MODE
          ret z
          ld (QUELLE+1),a
          ret

IMASK:    ; enter file mask
          call MENLIN
          ld hl,MI_TXT
          ld de,$0A03
          ld b,$0A
          ld c,$00
          call INPUT
          ret c
          ld hl,ASTR          ; A$
          ld de,FMASK
          ld a,(hl)
          cp CR               ; empty?
          jr z,NIX

          ; SET FILENAME
          call SETFN
          ret c
          jp RDSRC            ; and read new source

NIX:
          call SETWLD
          jp RDSRC


SETFN:    ; set FILENAME
          ; HL=input
          ; DE=target
          push hl
          ld b,13               ; 13 characters long incl. "."

SF_01:
          ld a,(hl)
          inc hl
          cp CR
          jr z,SF_02
          djnz SF_01

          ; error in filename
SF_ER:
          ld a,&9D
          call FEHLER
          pop hl
          ret

SF_02:
          pop hl
          push hl
          ld b,9              ; check name

SF_03:
          ld a,(hl)
          inc hl
          cp "."
          jr z,SF_04

          cp 00dh
          jr z,SF_04
          djnz SF_03

          jr SF_ER

SF_04:
          cp CR
          jr z,SF_06
          ld b,&04            ; check extension

SF_05:
          ld a,(hl)
          inc hl
          cp "."
          jr z,SF_ER
          cp CR
          jr z,SF_06
          djnz SF_05

          jr SF_ER    ;e08c  18 d3   . .

SF_06:    ; filename OK
          ; < 14 character incl. "."
          ; fname <= 8
          ; ext   <= 3 (provided available)
          pop hl

          ld b,&08            ; name first
          call SF_07
          ld b,&03
          ld a,(hl)
          cp CR
          jr z,SF_08
          inc hl

SF_07:    ; transfer data
          ld a,(hl)
          cp "*"            ; wildcards?
          jr z,SF_11
          cp "."
          jr z,SF_08
          cp CR
          jr z,SF_08
          ld (de),a
          inc hl
          inc de
          djnz SF_07

SF_08:
          ld a,b
          or a
          jr z,SF_10
          ld a," "          ; blanks

SF_09:    ; fill up with blanks or wildcards
          ld (de),a
          inc de
          djnz SF_09

SF_10:
          ret

SF_11:
          inc hl
          ld a,"?"
          jr SF_09

DISKCP:   ; copy a physical disk with CE-1600F
          ; juggling disks is required due to lack
          ; of memory

DISKCP:
          ;'Kopieren einer Diskette
          ;'in mehreren Schritten (5)
          ;setzt Modul S1: voraus
          ;hier die Holzhammermethode
          ;Hier ist die COPY-Routine
          ;insgesamt 10 Spuren mit 8 je Sektoren
          CALL ISDISK
          JR C,DK_01
          LD A,155
          JP FEHLER

DK_01:
          CALL CLS
          CALL TXTWTS
          RET Z

          LD A,&05
          LD HL,&4008
          EX AF,AF'
          EXX
          LD C,DREAD      ; **LESEN**
          LD A,&01        ; X:
          LD DE,0
          LD B,&1F        ; Anzahl Sektoren
          LD HL,DBUFF     ; Puffer
          CALL DO_COPY
          JP C,DC_EX

          CALL TXTWTA
          RET Z

          LD A,&05
          LD HL,&4008
          EX AF,AF'
          EXX
          LD C,DWRITE     ; **SCHREIBEN**
          LD A,&01        ; X:
          LD DE,0
          LD B,&1F        ; Anzahl Sektoren
          LD HL,DBUFF     ;Puffer
          CALL DO_COPY
          JP C,DC_EX

          CALL TXTWTS
          RET Z

          LD A,&05
          LD HL,&4008
          EX AF,AF'
          EXX
          LD C,DREAD      ; **LESEN**
          LD A,&01        ; X:
          LD DE,&0307
          LD B,&1F        ; Anzahl Sektoren
          LD HL,DBUFF     ; Puffer
          CALL DO_COPY
          JP C,DC_EX

          CALL TXTWTA
          RET Z

          LD A,&05
          LD HL,&4008
          EX AF,AF'
          EXX
          LD C,DWRITE     ; **SCHREIBEN**
          LD A,&01        ; X:
          LD DE,&0307
          LD B,&1F        ; Anzahl Sektoren
          LD HL,DBUFF     ; Puffer
          CALL DO_COPY
          JP C,DC_EX

          CALL TXTWTS
          RET Z

          LD A,&05
          LD HL,&4008
          EX AF,AF'
          EXX
          LD C,DREAD      ; **LESEN**
          LD A,&01        ; X:
          LD DE,&0706
          LD B,&1F        ; Anzahl Sektoren
          LD HL,DBUFF     ; Puffer
          CALL DO_COPY
          JP C,DC_EX

          CALL TXTWTA
          RET Z

          LD A,&05
          LD HL,&4008
          EX AF,AF'
          EXX
          LD C,DWRITE     ; **SCHREIBEN**
          LD A,&01        ; X:
          LD DE,&0706
          LD B,&1F        ; Anzahl Sektoren
          LD HL,DBUFF     ; Puffer
          CALL DO_COPY
          JP C,DC_EX

          CALL TXTWTS
          RET Z

          LD A,&05
          LD HL,&4008
          EX AF,AF'
          EXX
          LD C,DREAD      ; **LESEN**
          LD A,&01        ; X:
          LD DE,&0B05
          LD B,&1F        ; Anzahl Sektoren
          LD HL,DBUFF     ;Puffer
          CALL DO_COPY
          JP C,DC_EX

          CALL TXTWTA
          RET Z

          LD A,&05
          LD HL,&4008
          EX AF,AF'
          EXX
          LD C,DWRITE     ; **SCHREIBEN**
          LD A,&01        ; X:
          LD DE,&0B05
          LD B,&1F        ; Anzahl Sektoren
          LD HL,DBUFF     ; Puffer
          CALL DO_COPY
          JP C,DC_EX

          CALL TXTWTS
          RET Z

          LD A,&05
          LD HL,&4008
          EX AF,AF'
          EXX
          LD C,DREAD      ; **LESEN**
          LD A,&01        ; X:
          LD DE,&0F04
          LD B,&04        ; Anzahl Sektoren
          LD HL,DBUFF     ; Puffer
          CALL DO_COPY
          JP C,DC_EX

          CALL TXTWTA
          RET Z

          LD A,&05
          LD HL,&4008
          EX AF,AF'
          EXX
          LD C,DWRITE     ; **SCHREIBEN**
          LD A,&01        ; X:
          LD DE,&0F04
          LD B,&04        ; Anzahl Sektoren
          LD HL,DBUFF     ; Puffer
          CALL DO_COPY

DC_EX:
          LD A,&05
          LD HL,&4008
          EX AF,AF'
          EXX
          LD C,RESTORE
          LD A,&01        ; X:
          CALL BANKCALL
          CALL CLS
          JP RDSRC

DO_COPY:  ; calling the copy routine
          CALL BANKCALL
          RET NC
          JP DSK_ER

TXTWTA:
          LD DE,AIM_TX
          JR TXTWT

TXTWTS:
          LD DE,SRC_TX
TXTWT:
          CALL UPSCRL
          CALL MENLIN
          CALL PRINTS
          CALL INKEY
          CP MODE
          RET

ISDISK:   ; is CE-1600F connected?
          ; CY=1 yes
          ; CY=0 no
          LD A,(&F0AE)
          BIT 2,A
          JR NZ,RYES
          SCF
          CCF
          RET

ISMEP:    ; is MEP?
          ; CY=1 yes
          ; CY=0 no
          LD A,(&F0AE)
          BIT 0,A
          JR NZ,RYES
          SCF
          CCF
          RET

RYES:
          SCF
          RET


          ; while disassembling the code i found this "function" and have no clue
          ; what it is good for.
PRGINI:   call OINIT    ; old init process to prepare the rest
          call RDSRC

#IF PUBLIC = 1          ; header is needed for normal load from any drive
                        ; so 1 for load on 'real' PC-1600
                        ;    0 for load on emulated PC-1600
          call LOGO
          call WAIT
#ENDIF

MN_ENTR:
          ld a,1
          call SYMBOL
          call CLS

MN_LOOP:
          call MENUE
MNLP_02:
          ld a,(FPTR)
          call SHOWDIR
          ld de,00000h
          call CRSRSET
          ld de,ARROW
          call PRTSTR
          call INKEY
          cp DEF
          jr nz,MNLP_03
          ld a,(LAYER)
          cpl
          ld (LAYER),a
          jp MN_LOOP

MNLP_03:
          cp MODE

MNLP_04:
          jr nz,MNLP_05
          jp EXIT

MNLP_05:
          cp CUP          ; cursor up
          jr nz,MNLP_07    ; if not, next check
          ld a,(FPTR)     ; dec (FPTR)
          dec a
          cp &00
          jr z,MNLP_02
          ld (FPTR),a     ; and store

MNLP_06:
          jp MNLP_02

MNLP_07:
          cp CDN          ; cursor down
          jr nz,MNLP_08
          ld a,(FPTR)
          ld hl,COUNT
          cp (hl)
          jp nc,MNLP_02
          inc a
          ld (FPTR),a
          jr MNLP_02

MNLP_08:
          cp " "        ; space = toggle file mark
          jr nz,MNLP_09
          ld a,(FPTR)
          call MARK
          jr MNLP_02

MNLP_09:
          cp CLEFT        ; cursor left = begin of file list
          jr nz,MNLP_10
          ld a,&01
          ld (FPTR),a
          jr MNLP_02

MNLP_10:
          cp CRIGHT       ; cursor right = end of file list
          jr nz,MNLP_11
          ld a,(COUNT)
          ld (FPTR),a
          jp MNLP_02

MNLP_11:
          cp UPDN         ; updown key = demark all files
          jr nz,MNLP_12
          call DEMK_ALL
          jp MNLP_02

MNLP_12:
          cp RCL          ; RCL = toggle mark of all files
          jr nz,MNLP_13
          call TGL_MK
          jp MNLP_02

MNLP_13:
          cp ENTER        ; ENTER = execute program (BASIC/ML)
          jr nz,MNLP_14
          call RUNIT
          jp MN_LOOP

MNLP_14:
          cp KBII         ; KBII = swap source and target
          jp nz,MNLP_15
          call SWAP
          jp MN_LOOP

MNLP_15:
          push af      ;e2fb
          call MMKCHK      ; Makromaker check????
          pop af
          ld b,a              ; store key in B-reg
          ld a,(LAYER)        ; get menu layer
          or a
          ld a,b
          jp nz,MNLP_21       ; second layer, then further down
          cp F1               ; function key F1 "!"
          jr nz,MNLP_16
          call INFO
          jp MN_LOOP

MNLP_16:  ; file copy function
          cp F2               ; function key F2 """
          jr nz,MNLP_17
          call CP_FILE
          jp MN_LOOP
MNLP_17:
          cp F3               ; function key F3 "#"
          jr nz,MNLP_18
          call KILL
          jp MN_LOOP
MNLP_18:
          cp F4               ; function key F4 "&"
          jr nz,MNLP_19
          call IMASK
          jp MN_LOOP
MNLP_19:
          cp F5               ; function key F5 "%"
          jr nz,MNLP_20
          call C_QUELL
          jp MN_LOOP
MNLP_20:
          cp F6               ; function key F1 "!"
          jp nz,MNLP_02
          call C_ZIEL      ; change target drive'
          jp MN_LOOP

          ; ------------------------
          ; second menu layer active
          ; ------------------------
MNLP_21:
          cp F1               ; function key F1 "!"
          jr nz,MNLP_22
          call RENAME      ; rename file
          jp MN_LOOP
MNLP_22:
          cp F2               ; function key F2 """
          jr nz,MNLP_23
          call PROTECT      ; file protect function
          jp MN_LOOP
MNLP_23:
          cp F3               ; function key F3 "#"
          jr nz,MNLP_24
          call HEXDUMP      ; HEX/ASCII listing of file
          jp MN_LOOP
MNLP_24:
          cp F4               ; function key F4 "&"
          jr nz,MNLP_25
          call INITCOM
          jp MN_LOOP

MNLP_25:
          cp F5               ; function key F5 "%"
          jr nz,MNLP_26
          call FORMAT
          jp MN_LOOP

MNLP_26:
          cp F6               ; function key F1 "!"
          jp nz,MNLP_02
          call DISKCP
          jp MN_LOOP

EXIT:
          ld a,(BANK)
          out (031h),a
          call CLS
          call BASPARES     ; set BASIC parameter after Reset
          jp RESCA          ; CA after reset

; ------------------------------------------------------------------
; ---------- the original init process -----------------------------
; ------------------------------------------------------------------
OINIT:    ; all to prepare the rest (sub_e391h:)
          in a,($31)        ; get the current bank and store it
          ld (BANK),a

          ld a,"2"          ; standard from S2:
                            ; first define the source drive
          ld (QUELLE),a

          ; source is defined, now target to be found
          call ISMEP        ; first check if MEP is avaialbe
          ld a,"3"          ; start drive is "S3:" (if MEP is present)
          jr c, O_INI_01

          call ISDISK       ; else check if floppy is avaialble
          ld a,"X"
          jr c,O_INI_01     ; if floppy then continue
          ld a,"2"          ; if no floppy then use S2:
                            ; could eventually be extended here in regards
                            ; of checking if S2: or S1: is present and if
                            ; not use at least COM:

O_INI_01:
          ld (ZIEL),a
          ld hl,DIRMEM        ; 0e6f1h    e3ac  21 f1 e6   ! . .
          ld (DMPTR),hl
          xor a             ; A=0
          ld (COUNT),a      ; number of files on drive starts with 0
          ld (LAYER),a      ; menu layer 0 (=1st layer)
          ld (MARKED),a     ; and no files marked
          inc a
          ld (FPTR),a       ; and file pointer points to 1st file

          call SYMBOL

          call LOADCFG      ; load configuration file dwx.cfg
          call ITC_20

SETWLD:   ; set wildcards (e3de)
          push hl
          push de
          push bc
          push af
          ld hl,FMASK
          ld de,FMASK+1
          ld a,WLDCRD
          ld (hl),a
          ld bc,FNLEN-1
          ldir
          call SV_FMSK
          pop af
          pop bc
          pop de
          pop hl
          ret

SYMBOL:   ; set or reset the status symbols on the displaysa
          ld b,$00          ; DEF , I , II , III , Small , - , Shift , Busy
          call SMBLRD       ; reset busy symbol (Breg=00)
          res 0,a           ; bit 0 of A = busy
          call SMBLSET      ; reset busy symbol (Breg=00)
          ld b,002h         ; KBII, - , - , - , S , - , CTRL , bat
          call SMBLRD
          res 1,a           ; CTRL
          res 3,a           ; S
          res 7,a           ; KBII
          call SMBLSET      ; all symbols set accordingly
          ret

GFNADR:		; Get Filename Address
          ; calculates the address of the following file
          push af
          push bc
          push de
          ld hl,DIRMEM-FBFLEN
          ld de,FBFLEN
          inc a
          ld b,a

MK_01:
          add hl,de
          djnz MK_01
          pop de
          pop bc
          pop af
          ret

MARK:     ; toggle mark of current file
          ; file in A
          push hl
          push de
          push bc
          push af
          call GFNADR
          dec hl
          ld a,(hl)
          or a
          jr z,MK_EINS
          xor a
          ld (hl),a
          ld hl,MARKED
          dec (hl)
          jr MK_EX

MK_EINS:
          ld a,&01
          ld (hl),a
          ld hl,MARKED
          inc (hl)

MK_EX:
          pop af
          pop bc
          pop de
          pop hl
          ret

TGL_MK:   ; toggle all marked files to unmarked and vice versa
          push af
          push bc
          ld a,(COUNT)
          ld b,a
          ld a,001h

TGL_01:
          call MARK
          inc a
          djnz TGL_01
          pop bc
          pop af
          ret

DE_MARK:  ; de-mark file
          push hl
          push de
          push af
          call GFNADR
          dec hl
          ld a,(hl)
          or a
          jr z,DE_MKEX
          xor a
          ld (hl),a
          ld hl,MARKED
          dec (hl)

DE_MKEX:
          pop af
          pop de
          pop hl
          ret


DEMK_ALL: ; de-mark all files in list
          push af
          push bc
          ld a,(COUNT)
          ld b,a
          ld a,001h
DMKA_01:
          call DE_MARK
          inc a
          djnz DMKA_01
          pop bc
          pop af
          ret

PRINTS:
          push af
          push de
          call PRTSTR
          pop de
          pop af
          ret

          ld a,002h
          jr IN_06

INKEY2:   ; wait for key to be pressed with blinking cursor
          ld a,002h
          ld (CTYPE),a
          jr IN_06
CTYPE:   	DEFB 00

INKEYC:
					call INKEY
          cp MODE
          scf
          ret z
          ccf
          ret
INKEY:
          xor a
          ld (CTYPE),a
IN_06:
          push de
          push bc
IN_04:
          in a,(01bh)
          and 002h
          jr z,IN_05
          call BRRESET
IN_05:
          ld a,(CTYPE)
          call CRSRSTA
          call KEYGET
          push af
          xor a
          call CRSRSTA
          pop af
          cp 001h
          jr nz,IN_07
          ld b,000h
          call SMBLRD
          xor 002h
          call SMBLSET
          jr IN_04

IN_07:
          cp 002h
          jr nz,IN_08
          ld b,000h
          call SMBLRD
          xor 008h
          call SMBLSET
          jr IN_04

IN_08:
          cp 00fh
          jr nz,IN_09
          call OFF
          jr IN_04

IN_09:
          push af
          ld b,000h
          call SMBLRD
          res 1,a
          call SMBLSET
          pop af
          pop bc
          pop de
          ret
          cp 061h
          jr c,IN_10
          cp 07bh
          jr nc,IN_10
          sub 020h
IN_10:
          ret


MENUE:
          push de
          push af
          ld a,(BANK)       ; Bank
          out (031h),a      ; set
          call MENLIN       ; delete menu line
          ld a,(LAYER)
          or a
          jr nz,MN2_01
          ld de,M_TXT1
          call PRINTS
          ld de,01103h
          call CRSRSET
          ld a,(QUELLE)
          call Q_TEXT
          call PRINTS
          ld a,":"
          call PRTAKKU
          ld de,&1503
          call CRSRSET
          ld a,(QUELLE+1)
          call Q_TEXT
          call PRINTS
          ld a,":"
          call PRTAKKU
          jr MEN_EX
MN2_01:
          ld de,MN2_TX
          call PRINTS
MEN_EX:
          ld de,00003h
          ld a,26
          call RVSCHR
          pop af
          pop de
          ret

C_QUELL:  ; change source drive
          call MENLIN
          ld de,Q_TXT
          call PRINTS
          ld a,(QUELLE)
          ld (Q_BUFF),a
          call Q_IN
          cp MODE
          ret z

          ld (QUELLE),a
          call SETWLD
          call RDSRC
          jr nc,CQ_02
          ld a,(ERRCODE)
          cp 152
          ld a,&00
          ld (COUNT),a
          jr z,CQ_02
          ld a,(Q_BUFF)
          ld (QUELLE),a
          call RDSRC

CQ_02:
          call CLS
          ld a,&01    ; beginning from the 1st
          jp SHOWDIR

Q_IN:			; read key...
          call INKEY
          cp MODE
          ret z
          cp F2
          jr z,QI_C1
          cp F3
          jr z,QI_X
          cp F4
          jr z,QI_Y
          cp F5
          jr z,QI_3
          cp F6
          jr z,QI_2
          CP KBII
          JR Z,QI_1
          JR Q_IN

QI_C1:    ; serial interface
          LD A,"C"
          RET

QI_X:     ; X: drive
          LD A,"X"
          RET

QI_Y:     ; Y: drive
          LD A,"Y"
          RET

QI_3:     ; S3:
          LD A,"3"
          RET

QI_2:     ; S2:
          LD A,"2"
          RET

QI_1:     ; S1:
          LD A,"1"
          RET

SHOWDIR:  ; display the file list
          ; start at file # in accumulator
          ; 3 files are displayed in the
          ; forst 3 lines of the display
          ; it must be ensured that
          ; start < amount of files
          push af
          push bc
          push de
          push hl
          ld (Q_BUFF),a
          ld b,a
          ld a,(COUNT)
          cp 000h
          jr z,SD_EX
          ld hl,DIRMEM-FBFLEN
          ld de,FBFLEN
SD_1:
          add hl,de
          djnz SD_1
          ; (HL)= first file name
          ld (AKTPTR),hl
          xor a
          ld (LINE),a
SD_2:
          ld d,002h               ; X-pos = 2
          ld e,a                  ; Y-pos = accu (line)
          push af
          call CRSRSET
          pop af
          ld de,(AKTPTR)
          call SHOWFL
          call SHOWMK

          ld a,(Q_BUFF)
          inc a
          ld (Q_BUFF),a
          ld c,a
          ld a,(COUNT)
          cp c                ; bigger than?
          jr c,SD_EX          ; yes!

          ld hl,(AKTPTR)
          ld de,FBFLEN
          add hl,de
          ld (AKTPTR),hl

          ld a,(LINE)
          inc a
          ld (LINE),a
          cp 3
          jr c,SD_2

SD_EX:
          ld a,(LINE)
          cp $00
          jr nz,SD_03
          ld a,1
          call DEL1LIN
          ld a,2
          call DEL1LIN
          jr SD_04

SD_03:
          cp 001h
          jr nz,SD_04
          ld a,002h
          call DEL1LIN

SD_04:
          pop hl
          pop de
          pop bc
          pop af
          ret


SHOWFL:   ; display file
          push af
          push bc
          push de
          ld b,008h
SFI_01:
          ld a,(de)
          call PRTAKKU
          inc de
          djnz SFI_01
          ld a,02eh
          call PRTAKKU
          ld b,003h
SFI_02:
          ld a,(de)
          call PRTAKKU
          inc de
          djnz SFI_02
SFI_03:
          ld a,(de)
          or a
          jr z,SFI_04
          call PRTAKKU
          inc de
          jr SFI_03
SFI_04:
          pop de
          pop bc
          pop af
          ret

SHOWMK:   ; show mark (if file is marked)
          ; this routine inverts them
          push hl
          push de
          push af
          ex de,hl
          ld de,FBFLEN-1
          add hl,de
          ld a,(hl)
          or a
          jr z,SM_1
          ld a,(LINE)
          ld e,a
          ld d,2
          ld a,18
          call RVSCHR
SM_1:
          pop af
          pop de
          pop hl
          ret

MENLIN:   ; MENLIN
          push af
          push de
          ld a,003h
          call DEL1LIN
          ld de,00003h
          call CRSRSET
          pop de
          pop af
          ret

#IF PUBLIC=1
          ; display the splash screen on startup
          ; and wait some seconds
WAIT:
          ld bc,WAIT_1
W_LP1:
          ld de,WAIT_2
W_LP2:
          dec de
          ld a,d
          or e
          jr nz,W_LP2
          djnz W_LP1
          ret

LOGO:     ; show logo (splash screen)
          call CLS
          call HOME
          ld de,L_TXT
          call PRTLST
          ret
#ENDIF

PRTLST:   ; display more than one line of strings
          push af
          push bc
          push hl
          ld hl,&00FF
          ld (&F05F),hl
          ld b,&04
PL_01:
          call LONGP
          djnz PL_01
          pop hl
          pop bc
          pop af
          ret

LONGP:
          push af
          push bc
          push hl
          ld hl,(&F05F)
          ld a,l
          cp &03
          jr z,PD_01
          inc l
          jr PD_02

PD_01:
          call UPSCRL

PD_02:
          ld h,&00
          ld (&F05F),hl
          call PRTSTR
          pop hl
          pop bc
          pop af
          ret


; -----------------------------------------------------------------------------------------------
; all labels, data and texts of the DiskWorX application in this section as of here
; -----------------------------------------------------------------------------------------------

;#ENDIF


#IF PUBLIC = 1          ; header is needed for normal load from any drive
                        ; so 1 for load on 'real' PC-1600
                        ;    0 for load on emulated PC-1600
L_TXT:    ; logo or splash screen
          ;DEFB "+------------------------+"
          DEFB $C9,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$BB
          DEFB $BA
          DEFB  "     DiskWorX v2.0re    "
          DEFB $BA
          DEFB $BA
          DEFB  "  (c) 2025 by XianSoft  "
          DEFB $BA
          DEFB $C8,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$CD,$BC
          DEFB 00
          ;DEFB "+------------------------+"
#ENDIF

;              "12345678123"
CFGNAME:  defb "DWX     CFG"


BANK:      DEFB 0

QUELLE:   ; source drive (German: Quelle=source)
          DEFB "2"    ; S3: MEP

ZIEL:     ; target drive (German: Ziel=target)
          DEFB "2"    ; S2: disk drive

ZGQ_TX:
          DEFB "Target = Source!"
          DEFB &00

M_TXT1:
          DEFB " INF CPY KIL MSK"
          DEFB 0

MN2_TX:
          DEFB " NAM PRO HEX COM INI DCP"
          DEFB &00

Q_BUFF:
          DEFB &00

LINE:
          DEFB &00

AKTPTR:
          DEFW &0000

FPTR:     ; file pointer for the display
          DEFB &00

COUNT:    DEFB 0

LAYER:    DEFB 0

DMPTR:    ; file name pointer
          DEFW &0000

MARKED:   ; number of marked files (max 255)
          DEFB &00

X_TXT:
          DEFB "X"
          DEFS 3

Y_TXT:
          DEFB "Y"
          DEFS 3

S1_TXT:
          DEFB "S1"
          DEFS 2

S2_TXT:
          DEFB "S2"
          DEFS 2

S3_TXT:
          DEFB "S3"
          DEFS 2

C1_TXT:
          DEFB "COM"
          DEFS 1

ARROW:
          DEFB "->"
          DEFB &00

Q_TXT:
          DEFB "SRC: COM  X:  Y: S3: S2:"
          DEFB 0

FMASK:    ; file mask
          DEFS FNLEN          ; 12


MI_TXT:   ; input field for file mask
          DEFB "File Mask: "
          DEFB &00

; ------------------------------------------------
; display text for serial interface setup
; ------------------------------------------------
;
BAUDR:    DEFB "Baud:96  48  24  12  03"
          DEFB 00                         ; Terminator

PAR_TXT:  DEFB "Parity:         PO/PE NO"
          DEFB 00

PAR_EVOD: DEFB "Parity:         EVEN ODD"
          DEFB 00

BITS_TXT: DEFB "Bits:     5   6   7   8"
          DEFB 00

STPBTXT:  DEFB "Stopbits:         1   2"
          DEFB 00

XOXOTXT:  DEFB "XOn/XOff:        On  Off"
          DEFB 00

SHFT_IO:  DEFB "Shift In/Out     In  Out"
          DEFB 00
; --------------------------------------------------


; --------------------------------------------------
; some texts for disk functions and the INF function
; --------------------------------------------------

CPT1_TXT: DEFB "COPY "
          DEFB 00

CPT2_TXT: DEFB "TO  : "
          DEFB 00

BUFFCPD:
          DEFB 00

FRMTTX:
          DEFB "Format Disk X:?"
          DEFB &00

ANZERG:
          DEFB " Free on "
          DEFB 0
          DEFB ":"

ERGEB:
          DEFS 7

AF_TXT:
          DEFB "Number of Files: "
          DEFB 0

ANZBYT:
          DEFB " Bytes"
          DEFB &00

KLTX01:
          DEFB "Kill "
          DEFB &22
          DEFB &00

FMASK1:   DEFS FNLEN

DEL_TX:
          DEFB "Delete..."
          DEFB &00

BES_TXT:
          DEFB "Confirm each?"

JANEIN:		; JANEIN
          DEFB " [Y/N]:"
          DEFB &00

BES_TAG:
          DEFB &00

LOE_TXT:
          DEFB "Delete?"
          DEFB &00

POWERI:
          DEFB "I="
          DEFB 0

POWERE:
          DEFB ",E="
          DEFB 0

CPTX01:
          DEFB "Copy. Disk ready? "
          DEFB &00

RNTX01:
          DEFB "B"

RNTX04:
          DEFB "LOAD"
          DEFB &22
          DEFB &00

RNTX02:
          DEFS 20

RNTX03:
          DEFB "Start Program? "
          DEFB &00

NMTX01:
          DEFB "Name "
          DEFB &22
          DEFB &00

NMTX02:
          DEFB "  as "
          DEFB &22
          DEFB &00

NM_TXT:
          DEFB "Name as:"
          DEFB &00

SERTXT:
          DEFB "Interface ready? "
          DEFB 0

          ; some texts and definitions for disk copy
SRC_TX:
          DEFB "Insert Source-Disk"
          DEFB &00

AIM_TX:
          DEFB "Insert Target-Disk"
          DEFB &00

Z_TXT:    DEFB "TAR"    ; target = Ziel in German
          DEFB $00



; -------------------------------------------------------
; the error messages
; -------------------------------------------------------

FT00:
          DEFB "151 File already exists!"
          DEFB &00

FT01:
          DEFB "152 File not found!"
          DEFB &00

FT02:
          DEFB "154 File already open!"
          DEFB &00

FT03:
          DEFB "155 Device not present!"
          DEFB &00

FT04:
          DEFB "157 Incorrect file name!"
          DEFB &00

FT05:
          DEFB "159 Disk write protected!"
          DEFB &00

FT06:
          DEFB "160 No disk in drive!"
          DEFB &00

FT07:
          DEFB "161 Disk not formatted!"
          DEFB &00

FT08:
          DEFB "162 Write/read Error!"
          DEFB &00

FT09:
          DEFB "163 Wrong disk(change)!"
          DEFB &00

FT10:
          DEFB "164 Disk full!"
          DEFB &00

FT11:
          DEFB "167 Fatal Error!"
          DEFB &00

FT12:
          DEFB "168 Battery empty!"
          DEFB &00

FT13:
          DEFB "99 Miscellaneous Error!"
          DEFB &00

; -----------------------------------------------------------






ENDADR: defb $FF

DIRMEM:   ; space for the directory/file data allocated after program code
; ************************************************************************
; marker for the KDE editor kate, to treat this file as z80.asm
; kate: hl z80;
