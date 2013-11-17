;----------------------------------------------
;ROB HUBBARDS MUSIC PLAYER
; C64 conversion by Kees van Oss 2012
;----------------------------------------------

asm_code = $2A00

org asm_code-22

.atm_header

;********************************************************************
; ATM Header for Atom emulator Wouter Ras


        EQUS    "HUBPLAY"

        EQUB    $00
        EQUB    $00
        EQUB    $00
        EQUB    $00
        EQUB    $00
        EQUB    $00
        EQUB    $00
        EQUB    $00
        EQUB    $00


        EQUW    asm_code
        EQUW    start_asm
        EQUW    eind_asm-start_asm

;********************************************************************


.exec
.start_asm
;-------------------------------------------------------------------------------
;|The main program                                            		       |
;-------------------------------------------------------------------------------

irqtime		= 20000			; 50 times/sec

irqvec		= $204
via_T1		= $b804
via_ACR		= $b80b
via_IER		= $b80e

.start          lda #$00                ;Initialize the playback of the
                jsr initmusic           ;first tune

                sei
                lda #<raster            ;Set interrupt vector
                sta irqvec
                lda #>raster
                sta irqvec+1

		lda #<(irqtime)		; Set IRQ timer
		sta via_T1
		lda #>(irqtime)
		sta via_T1+1

		lda via_ACR		; Continuous IRQ on T1
		and #$7f
		ora #$40
		sta via_ACR

		lda #$c0		; Enable T1-IRQ
		sta via_IER

                cli
                rts

.raster		lda via_T1		; Clear IRQ flag
		txa
		pha
		tya
		pha

                jsr playmusic            ;Play one frame of music

		pla
		tay
		pla
		tax
		pla
		rti

	include "hubplay.inc"

.eind_asm

save "HUBPLAY", atm_header, eind_asm
