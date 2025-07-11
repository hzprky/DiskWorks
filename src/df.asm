; ---------------------------------------------------
; * Module          df.asm
; * Purpose:        Definitions for DiskWorks 3.0/25
; *
; * Description:    all definition used for assembling
; *                 DiskWorks with ZASM
; * Version:        3.0
; * Date:           17.06.2025
; * Changes:        initial Version from old project
; * Release ref.:   3.0pre
; * Assembler:      ZASM 4.4
; * Author:         Xian
; * Copyright:      (c) 2025 by Christian Becker
; * License:        Creative Common????
; ---------------------------------------------------
;
; muss evtl. noch woanders hin...
START    EQU $D000 ; start address for DiskWorks
;
; -------------------------------------
; definitions and macros
; -------------------------------------
DBUFF     EQU $80D0 ; buffer for function diskcopy
MSTACK    EQU $F9C0 ; machine stack
HEADERCV  EQU $F21D ; header for receive
HEADRSND  EQU $FB60 ; header for send
XX        EQU $FA00 ; arithmetic variable of OS
YY        EQU $FA10 ; also
ZZ        EQU $FA08 ; also
ARGCOUNT  EQU $F88C ; argument counter for arithmetic operations
CPLUS     EQU $021A ; plus funktion for calculation
CMINUS    EQU $021D ; minus funktion for calculation
CMULT     EQU $0220 ; multiplication funktion for calculation
CDIVIS    EQU $0223 ; division funktion for calculation
CMOD      EQU $0226 ; MOD funktion for calculation
CDIV      EQU $0229 ; DIV funktion for calculation (integer division)
CGETR     EQU $0244 ; get last result from XX into string referencesd by DE-reg

; keyboard functions
KBUFSET   EQU $016C
KEYDRCT   EQU $0175
BRRESET   EQU $018A
KEYGET    EQU $0166

; definitions for display functions
PRTASTR   EQU $00EB
PRTSTR    EQU $0106
PRTAKKU   EQU $0100
BLANKS    EQU $013F
CRSRPOS   EQU $0118
CRSRSET   EQU $0115
CRSRSTA   EQU $011E
UPSCRL    EQU $012D
DNSCRL    EQU $0130
INS1LIN   EQU $0142
DEL1LIN   EQU $0145
CLS       EQU $0112
HOME      EQU $0109
SMBLRD    EQU $0139
SMBLSET   EQU $013C
RVSCHR    EQU $011B

; definitions for timer and ad functions
TIMER     EQU $01D5
TMREAD    EQU $03

; definitions for serial interface functions
SERIELL   EQU $01D8
SETCOM    EQU $01
SETDEV    EQU $12
COM1      EQU $01
COM2      EQU $02

; definitions for printer
PRINTER   EQU $4008
PINIT     EQU $00
PRESET    EQU $28
PTEXT     EQU $01
POUT      EQU $4023
PCRLF     EQU $2B
PCR       EQU $29
PLEFTM    EQU $07
PCSIZE    EQU $03

; common functions
OFF       EQU $0005
BASIC     EQU $6844
BANKCALL  EQU $019F
DREAD     EQU $84
DWRITE    EQU $85
RESTORE   EQU $82
STRCNV    EQU $0283
BASPARES  EQU $0312
RESCA     EQU $0253 ; CA nach Reset
BIN2BCD   EQU $024A
SOUT      EQU $01B7
ASCBIN    EQU $023E


; IOCS file funktions
XFILE     EQU $01DE
OP_FL     EQU $0F  ;OPENFILE
CL_FL     EQU $10  ;close file
SFIRST    EQU $11  ;SEARCH 1ST
SNEXT     EQU $12  ;SEARCH NXT
DL_FL     EQU $13  ;DELFILE
RD_FL     EQU $14  ; sequential READ FILE
WR_FL     EQU $15  ; sequential WRITE FILE
CR_FL     EQU $16  ;CREATE FILE
NM_FL     EQU $17  ;Rename file
CR_FC     EQU $1A  ;CREATE FCB
; ----------------------------------------------
GETALC    EQU $1B  ; get alloc get inf. about drive (not impl. in MEP)
SETATR    EQU $1E  ; set file attributes (not impl. in MEP)
; ----------------------------------------------
TRANAD    EQU $FC46 ; read/write address FCB
TRDATA    EQU $F400 ; reserved for default FCB
; ----------------------------------------------

; common definitions used in the different modules
WAIT_1    EQU $0200
WAIT_2    EQU $FFFF
WLDCRD    EQU "?"
FBFLEN    EQU 12+5+1+1
FNLEN     EQU 12
FNNR      EQU 30
KILLED    EQU $E5

; key codes

SOT       EQU $00
SHIFT     EQU $01
SML       EQU $02
CTRL      EQU $03
KB2       EQU $04
KBII      EQU $04
BS        EQU $05
CLEFT     EQU $08
UPDN      EQU $09
CDN       EQU $0A
LF        EQU $0A
CUP       EQU $0B
CRIGHT    EQU $0C
CR        EQU $0D
ENTER     EQU $0D
KON				EQU $0E		; on/break key
KOFF      EQU $0F
F1        EQU $11
F2        EQU $12
F3        EQU $13
F4        EQU $14
F5        EQU $15
F6        EQU $16
CL        EQU $18
RCL       EQU $19
EOF       EQU $1A
DEF       EQU $1B
MODE      EQU $1F
SPACE     EQU $20

; definitions for OS addresses
FCB       EQU $F3C7
FCB2      EQU $F256
ASTR      EQU $F8C0  ; A$
BSTR      EQU $F8D0  ; B$
CURX      EQU $F060
CURY      EQU $F05F
IBUFF     EQU $F21D
ERRCODE   EQU $F89B
ROMBITS   EQU $F0AE

; definitions for MarcoMaker component
TRENNER   EQU "^"
CFGDATA   EQU $CC00

; **** macro definitions ****

; -----------------------------------
; BANKCAL RST $20
;
BCALL:	.macro &A, &B
		RST $20
		DEFB &A
		DEFW &B
		.endm

;------------------------------------
; invert character
;
; D = X coordinate
; E = Y coordinate
; A = number of chars to be inverted
; return: CF=1 if coordinates out of bounds
; registers changed: -.-
;
INVERT:	.macro &A, &B
		LD DE, &A
		LD A, &B
		CALL RVSCHR
		.endm

; -----------------------------------
; set cursor position
;
; D = X coordinate
; E = Y coordinate
; return: CF=1 if coordinates out of bounds
; registers changed: AF, $F05F (Y), $F060 (X)
;
CURSOR:	.macro &A
		LD DE, &A
		CALL CRSRSET
		.endm

; -----------------------------------
; print string up to endcode in A
;
; DE = start address of string
; A  = string terminating character
; return: CF = 1 if last character in most right screen position
;         DE = address of least displayed character + 1
; registers changed: AF, $F060 (X), cursor type ($F067)
;
APRINT:	.macro &A, &B
		LD A, &A
		LD DE, &B
		CALL PRTASTR
		.endm

; -----------------------------------
; print string terminated by 0
;
; DE = start address of string
; return: CF = 1 if last character in most right screen position
;         DE = address of least displayed character + 1
; registers changed: AF, $F060 (X), cursor type ($F067)
;
PRINT:	.macro &A
		LD DE, &A
		CALL PRTSTR
		.endm

; -----------------------------------
; print string terminated by 0
;
; DE = start address of string
; return: CF = 1 if last character in most right screen position
;         DE = address of least displayed character + 1
; registers changed: AF, $F060 (X), cursor type ($F067)
;
PRINTX:	.macro &A
		LD DE, &A
		CALL PRTSTR
		.endm

; -----------------------------------
; print string terminated by 0 at certain position
; first position is set, then string is printed
; concatinated macro of two functions (see above)
;
PRINTAT:	.macro &A, &B
			LD DE, &A
			CALL CRSRSET
			LD DE, &B
			CALL PRTSTR
			.endm
;

; -----------------------------------
; give a short beep
;
BEEP:    	.macro
            PUSH AF
            PUSH BC
            LD A,&50        ; tone frequency
            LD BC,&00FF     ; tone length
            CALL SOUT
            POP BC
            POP AF
  			.endm
;

; -----------------------------------
; call routine from external ROM (MEP, CE-1600P/F)
;
; 019F BANKCALL
; parameters: bank - A'reg,
;             adr  - HL’reg
EXTCALL:	.macro &A, &B
			push af
			push hl
			LD A, &A
			LD HL,&B
			CALL BANKCALL
			pop hl
			pop af
			.endm
;

;------------------------------
; MEP related functions
CDIR:     .macro &A, &B
            LD DE,&A
            LD B,&B
            EXTCALL $7,$4020
          .endm
DIRMODE:  .macro
            EXTCALL $7,$4023
          .endm
FILEMODE: .macro
            EXTCALL $7,$4026
          .endm

; ---------------------------------


; --------- 4 kate -----------
; kate: hl z80;
