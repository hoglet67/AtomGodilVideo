L00DE   = $00DE
L00DF   = $00DF
L00E0   = $00E0
L00E1   = $00E1
L00E2   = $00E2
L00E3   = $00E3
L00E4   = $00E4
L00E5   = $00E5
L00E6   = $00E6
L00FB   = $00FB

IO8255_0 = $B000
IO6522_0 = $B800

IO8255_1 = IO8255_0 + 1
IO8255_2 = IO8255_0 + 2
IO8255_3 = IO8255_0 + 3

IO6522_1 = IO6522_0 + 1
IO6522_2 = IO6522_0 + 2
IO6522_3 = IO6522_0 + 3
IO6522_4 = IO6522_0 + 4
IO6522_5 = IO6522_0 + 5
IO6522_6 = IO6522_0 + 6
IO6522_7 = IO6522_0 + 7
IO6522_8 = IO6522_0 + 8
IO6522_9 = IO6522_0 + 9
IO6522_A = IO6522_0 + 10
IO6522_B = IO6522_0 + 11
IO6522_C = IO6522_0 + 12
IO6522_D = IO6522_0 + 13
IO6522_E = IO6522_0 + 14
IO6522_F = IO6522_0 + 15

LOAD = $9800

VGA80 = 1

IF (VGA80 = 1)
NUMROWS = 40
NUMCOLS = 80
ELSE
NUMROWS = 16
NUMCOLS = 32
ENDIF

SCREEN = $8000
SCREENEND = SCREEN + (NUMROWS - 1) * NUMCOLS

LFE4A = $fe71
WRCVEC = $0208

	org LOAD - 22
.AtmHeader
        EQUS    "OSWRCH80"
        org AtmHeader + 16
        EQUW	BeebDisStartAddr
        EQUW    BeebDisStartAddr
        EQUW	BeebDisEndAddr - BeebDisStartAddr


        org     LOAD

.BeebDisStartAddr

	LDA    #<LFE2B
	STA	WRCVEC
	LDA    #>LFE2B
	STA	WRCVEC+1
	RTS

.LF5E0
        STY     L00DE
        JSR     LF8AB

        JMP     LFD1B


.LF8AB
        PHA
        LDA     L00E6
        BMI     LF8B4

        LDA     #NUMROWS
        STA     L00E6
.LF8B4
        PLA
        RTS

.LFCBF
        CMP     #$06
        BEQ     LFCE0

        CMP     #$15
        BEQ     LFCE6

        LDY     L00E0
        BMI     LFCEE

        CMP     #$1B
        BEQ     LFCE0

        CMP     #$07
        BEQ     LFCEF

        JSR     LFD1D

        LDX     #$0A
        JSR     LFE9E

        BNE     LFD02

        JMP     LFE90

.LFCE0
        CLC
        LDX     #$00
        STX     IO8255_0
.LFCE6
        LDX     #$02
.LFCE8
        PHP
        ASL     L00DE,X
        PLP
        ROR     L00DE,X
.LFCEE
        RTS

.LFCEF
        PHP
        SEI
        LDA     #$05
        TAY
.LFCF4
        STA     IO8255_3
.LFCF7
        DEX
        BNE     LFCF7

        EOR     #$01
        INY
        BPL     LFCF4

        PLP
        RTS

        NOP
.LFD02
        CMP     #$20
        BCC     LFD1D

        ADC     #$1F
        BMI     LFD0C

        EOR     #$60
.LFD0C
        JSR     LFE44

        STA     (L00DE),Y
.LFD11
        INY
        CPY     #NUMCOLS
        BCC     LFD1B

        JSR     LFDC5

.LFD19
        LDY     #$00
.LFD1B
        STY     L00E0
.LFD1D
        PHA
        JSR     LFE44

        LDA     (L00DE),Y
        EOR     L00E1
        STA     (L00DE),Y
        PLA
        RTS

.LFD29
        JSR     LFE0E

        LDA     #$20
        JSR     LFE44

        STA     (L00DE),Y
        BPL     LFD1B

.LFD35
        JSR     LFE0E

        JMP     LFD1B

.LFD3B
        JSR     LFDC5

.LFD3E
        LDY     L00E0
        BPL     LFD1B

.LFD42
        LDY     #$80
        STY     L00E1
        LDY     #$00
        STY     IO8255_0
        LDA     #$20
.LFD4D
        STA     SCREEN,Y
        STA     SCREEN+$100,Y
IF (VGA80 = 1)
        STA     SCREEN+$200,Y
        STA     SCREEN+$300,Y
        STA     SCREEN+$400,Y
        STA     SCREEN+$500,Y
        STA     SCREEN+$600,Y
        STA     SCREEN+$700,Y
        STA     SCREEN+$800,Y
        STA     SCREEN+$900,Y
        STA     SCREEN+$a00,Y
        STA     SCREEN+$b00,Y
        STA     SCREEN+$c00,Y
ENDIF
        INY
        BNE     LFD4D

.LFD56
        LDA     #>SCREEN
        LDY     #$00
        STA     L00DF
        JMP     LF5E0

        NOP
.LFD60
        JSR     LFE13

        JMP     LFD1B

.LFD66
        CLC
        LDA     #NUMROWS
        STA     L00E6

.LFD6B
        LDX     #$08
        JSR     LFCE8

        JMP     LFD1D


.LFDC5
        LDY     L00E6
        BMI     LFDD5

        DEY
        BNE     LFDD3

.LFDCC
        JSR     LFE4A

        BCS     LFDCC

        LDY     #NUMROWS-1
.LFDD3
        STY     L00E6
.LFDD5
        LDA     L00DE
        LDY     L00DF

        CPY     #>SCREENEND
        BCC     LFE05

        CMP     #<SCREENEND
        BCC     LFE05

        LDY     #NUMCOLS
        JSR     LFE3F

.LFDE6
        LDA     SCREEN,Y
        STA     SCREEN-NUMCOLS,Y
        INY
        BNE     LFDE6

        JSR     LFE44

.LFDF2
        LDA     SCREEN+$100,Y
        STA     SCREEN+$100-NUMCOLS,Y
        INY
        BNE     LFDF2

IF (VGA80 = 1)
	JSR     CLEARMORE
ENDIF

        LDY     #NUMCOLS-1
        LDA     #$20
.LFDFF
        STA     (L00DE),Y
        DEY
        BPL     LFDFF

        RTS

.LFE05
        ADC     #NUMCOLS
        STA     L00DE
        BCC     LFE0D

        INC     L00DF
.LFE0D
        RTS

.LFE0E
        DEY
        BPL     LFE2A

        LDY     #NUMCOLS-1
.LFE13
        LDA     L00DE
        BNE     LFE22

        LDX     L00DF
        CPX     #>SCREEN
        BNE     LFE22

        PLA
        PLA
        JMP     LFD3E

.LFE22
        SBC     #NUMCOLS
        STA     L00DE
        BCS     LFE2A

        DEC     L00DF
.LFE2A
        RTS

.LFE2B
        JSR     LFED4

        PHP
        PHA
        CLD
        STY     L00E5
        STX     L00E4
        JSR     LFCBF

        PLA
.LFE39
        LDX     L00E4
        LDY     L00E5
        PLP
        RTS

.LFE3F
        BIT     IO8255_2
        BPL     LFE3F

.LFE44
        BIT     IO8255_2
        BMI     LFE44

        RTS

.LFE90
        LDA     LFEBC,X
        STA     L00E2
        LDA     #>LFD1B
        STA     L00E3
        TYA
        JMP     (L00E2)

.LFE9D
        DEX
.LFE9E
        CMP     LFEA4,X
        BCC     LFE9D

        RTS

.LFEA4
         EQUB    $00

.LFEA5
         EQUB    $08,$09,$0A,$0B,$0C,$0D,$0E,$0F,$1E,$7F

	
.LFEBC
        EQUB <LFD1D
        EQUB <LFD35
        EQUB <LFD11
        EQUB <LFD3B
        EQUB <LFD60
        EQUB <LFD42
        EQUB <LFD19
        EQUB <LFD66
        EQUB <LFD6B
        EQUB <LFD56
        EQUB <LFD29

.LFED4
        PHA
        CMP     #$02
        BEQ     LFF00

        CMP     #$03
        BEQ     LFF11

        CMP     L00FB
        BEQ     LFF0F

        LDA     IO6522_C
        AND     #$0E
        BEQ     LFF0F

        PLA
.LFEE9
        BIT     IO6522_1
        BMI     LFEE9

        STA     IO6522_1
        PHA
        LDA     IO6522_C
        AND     #$F0
        ORA     #$0C
        STA     IO6522_C
        ORA     #$02
        BNE     LFF0C

.LFF00
        LDA     #$7F
        STA     IO6522_3
        LDA     IO6522_C
        AND     #$F0
        ORA     #$0E
.LFF0C
        STA     IO6522_C
.LFF0F
        PLA
        RTS

.LFF11
        LDA     IO6522_C
        AND     #$F0
        BCS     LFF0C

.CLEARMORE

.LFDF2A
        LDA     SCREEN+$200,Y
        STA     SCREEN+$200-NUMCOLS,Y
        INY
        BNE     LFDF2A
.LFDF2B
        LDA     SCREEN+$300,Y
        STA     SCREEN+$300-NUMCOLS,Y
        INY
        BNE     LFDF2B
.LFDF2C
        LDA     SCREEN+$400,Y
        STA     SCREEN+$400-NUMCOLS,Y
        INY
        BNE     LFDF2C
.LFDF2D
        LDA     SCREEN+$500,Y
        STA     SCREEN+$500-NUMCOLS,Y
        INY
        BNE     LFDF2D
.LFDF2E
        LDA     SCREEN+$600,Y
        STA     SCREEN+$600-NUMCOLS,Y
        INY
        BNE     LFDF2E
.LFDF2F
        LDA     SCREEN+$700,Y
        STA     SCREEN+$700-NUMCOLS,Y
        INY
        BNE     LFDF2F
.LFDF2G
        LDA     SCREEN+$800,Y
        STA     SCREEN+$800-NUMCOLS,Y
        INY
        BNE     LFDF2G
.LFDF2H
        LDA     SCREEN+$900,Y
        STA     SCREEN+$900-NUMCOLS,Y
        INY
        BNE     LFDF2H
.LFDF2I
        LDA     SCREEN+$a00,Y
        STA     SCREEN+$a00-NUMCOLS,Y
        INY
        BNE     LFDF2I
.LFDF2J
        LDA     SCREEN+$b00,Y
        STA     SCREEN+$b00-NUMCOLS,Y
        INY
        BNE     LFDF2J
.LFDF2K
        LDA     SCREEN+$c00,Y
        STA     SCREEN+$c00-NUMCOLS,Y
        INY
        BNE     LFDF2K

	RTS
	
.BeebDisEndAddr

SAVE "OSWRCH80",AtmHeader,BeebDisEndAddr

