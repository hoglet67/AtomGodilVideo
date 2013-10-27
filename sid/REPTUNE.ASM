;----------------------------------------------
;REPTUNE
;----------------------------------------------

asm_code = $3000
header =   1		; Header Wouter Ras emulator

org asm_code-22*header

IF (header = 1)
;********************************************************************
; ATM Header for Atom emulator Wouter Ras

	EQUS    "REPTUNE"

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
	EQUW	eind_asm-start_asm

;********************************************************************
ENDIF


.exec
.start_asm
	include "sidplay.inc"
	include "reptune.inc"
.eind_asm

save "REPTUNE", $2fea, eind_asm
 