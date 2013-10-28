;rob hubbard
;monty on the run music driver

;this player was used (with small mods)
;for his first approx 30 musix

sid	= $9dc0

	jmp initmusic
	jmp playmusic
	jmp musicoff


;====================================
;init music
;====================================

.initmusic
	lda #$00         ;music num
	ldy #$00
	asl a
	sta tempstore
	asl a
	clc
	adc tempstore    ;now music num*6
	tax
.l1
	lda songs,x      ;copy ptrs to this
	sta currtrkhi,y  ;music's tracks to
	inx              ;current tracks
	iny
	cpy #$06
	bne l1

	lda #$00         ;clear control regs
	sta sid+$4
	sta sid+$b
	sta sid+$12
	sta sid+$17

	lda #$0f         ;full volume
	sta sid+$18

	lda #$40         ;flag init music
	sta mstatus

	rts

;====================================
;music off
;====================================

.musicoff
	lda #$c0         ;flag music off
	sta mstatus
	rts

;====================================
;play music
;====================================

.playmusic
	inc counter

	bit mstatus      ;test music status
	bmi moff         ;$80 and $c0 is off
	bvc contplay     ;$40 init, else play


;==========
;init the song (mstatus $40)
;==========

	lda #$00         ;init counter
	sta counter

	ldx #3-1
.l2
	sta posoffset,x  ;init pos offsets
	sta patoffset,x  ;init pat offsets
	sta lengthleft,x ;get note right away
	sta notenum,x
	dex
	bpl l2

	sta mstatus      ;signal music play
	jmp contplay

;==========
;music is off (mstatus $80 or $c0)
;==========

.moff
	bvc l3            ;if mstatus $c0 then
	lda #$00
	sta sid+$4        ;kill voice 1,2,3
	sta sid+$b        ;control registers
	sta sid+$12

	lda #$0f         ;full volume still
	sta sid+$18

	lda #$80         ;flag no need to kill
	sta mstatus      ;sound next time
.l3
	jmp musicend     ;end

;==========
;music is playing (mstatus otherwise)
;==========

.contplay
	ldx #3-1         ;number of chanels

	dec speed        ;check the speed
	bpl mainloop

	lda resetspd     ;reset speed if needed
	sta speed

.mainloop
	lda regoffsets,x ;save offset to regs
	sta tmpregofst   ;for this channel
	tay

;check whether a new note is needed
	lda speed        ;if speed not reset
	cmp resetspd     ;then skip notework
	beq checknewnote
	jmp vibrato

.checknewnote
	lda currtrkhi,x  ;put base addr.w of
	sta $82          ;this track in $2
	lda currtrklo,x
	sta $83

	dec lengthleft,x ;check whether a new
	bmi getnewnote   ;note is needed

	jmp soundwork    ;no new note needed

;==========
;notework
;a new note is needed. get the pattern
;number/cc from this position
;==========

.getnewnote

  ldy posoffset,x  ;get the data from
  lda ($82),y      ;the current position

  cmp #$ff         ;pos $ff restarts
  beq restart

  cmp #$fe         ;pos $fe stops music
  bne getnotedata  ;on all channels
  jmp musicend

;cc of $ff restarts this track from the
;first position

.restart

  lda #$00         ;get note immediately
  sta lengthleft,x ;and reset pat,pos
  sta posoffset,x
  sta patoffset,x
  jmp getnewnote


;get the note data from this pattern

.getnotedata

  tay
  lda patptl,y     ;put base addr.w of
  sta $84          ;the pattern in $4
  lda patpth,y
  sta $85

  lda #$00         ;default no portamento
  sta portaval,x

  ldy patoffset,x  ;get offset into .pth

  lda #$ff         ;default no append
  sta appendfl

;1st byte is the length of the note 0-31
;bit5 signals no release (see sndwork)
;bit6 signals appended note
;bit7 signals a new instrument
;     or portamento coming up

  lda ($84),y      ;get length of note
  sta savelnthcc,x
  sta templnthcc
  and #$1f
  sta lengthleft,x

  bit templnthcc   ;test for append
  bvs appendnote

  inc patoffset,x  ;pt to next data

  lda templnthcc   ;2nd byte needed?
  bpl getpitch

;2nd byte needed as 1st byte negative
;2nd byte is the instrument number(+ve)
;or portamento speed(-ve)

  iny
  lda ($84),y      ;get instr/portamento
  bpl l4

  sta portaval,x   ;save portamento val
  jmp l5
.l4
 sta instrnr,x    ;save instr nr
.l5
 inc patoffset,x

;3rd byte is the pitch of the note
;get the 'base frequency' here

.getpitch

  iny
  lda ($84),y      ;get pitch of note
  sta notenum,x
  asl a            ;pitch*2
  tay
  lda frequenzlo,y ;save the appropriate
  sta tempfreq     ;base frequency
  lda frequenzhi,y
  ldy tmpregofst
  sta sid+$1,y
  sta savefreqhi,x
  lda tempfreq
  sta sid+$0,y
  sta savefreqlo,x
  jmp l6

.appendnote

  dec appendfl     ;clever eh?


;fetch all the initial values from the
;instrument data structure

.l6
  ldy tmpregofst
  lda instrnr,x    ;instr num
  stx tempstore
  asl a            ;instr num*8
  asl a
  asl a
  tax

  lda instr+2,x    ;get control reg val
  sta tempctrl
  lda instr+2,x
  and appendfl     ;implement append
  sta sid+$4,y

  lda instr+0,x    ;get pulse width lo
  sta sid+$2,y

  lda instr+1,x    ;get pulse width hi
  sta sid+$3,y

  lda instr+3,x    ;get attack/decay
  sta sid+$5,y

  lda instr+4,x    ;get sustain/release
  sta sid+$6,y

  ldx tempstore    ;save control reg val
  lda tempctrl
  sta voicectrl,x


;4th byte checks for the end of pattern
;if eop found, inc the position and
;reset patoffset for new pattern

  inc patoffset,x  ;preview 4th byte
  ldy patoffset,x
  lda ($84),y

  cmp #$ff         ;check for eop
  bne l7

  lda #$00         ;end of pat reached
  sta patoffset,x  ;inc position for
  inc posoffset,x  ;the next time
.l7
  jmp loopcont


;==========
;soundwork
;the instrument and effects processing
;routine when no new note was needed

.soundwork

;release routine
;set off a release when the length of
;the note is exceeded
;bit4 of the 1st note-byte can specify
;for no release

  ldy tmpregofst

  lda savelnthcc,x ;check for no release
  and #$20         ;specified
  bne vibrato

  lda lengthleft,x ;check for length of
  bne vibrato      ;exceeded

  lda voicectrl,x  ;length exceeded so
  and #$fe         ;start the release
  sta sid+$4,y      ;and kill adsr
  lda #$00
  sta sid+$5,y
  sta sid+$6,y


;vibrato routine
;(does alot of work)

.vibrato

  lda instrnr,x    ;instr num
  asl a
  asl a
  asl a            ;instr num*8
  tay
  sty instnumby8   ;save instr num*8

  lda instr+7,y    ;get instr fx byte
  sta instrfx

  lda instr+6,y    ;get pulse speed
  sta pulsevalue

  lda instr+5,y    ;get vibrato depth
  sta vibrdepth
  beq pulsework    ;check for no vibrato

  lda counter      ;this is clever!!
  and #7           ;the counter's turned
  cmp #4           ;into an oscillating
  bcc l8            ;value (01233210)
  eor #7
.l8
  sta oscilatval

  lda notenum,x    ;get base note
  asl a            ;note*2
  tay              ;get diff btw note
  sec              ;and note+1 frequency
  lda frequenzlo+2,y
  sbc frequenzlo,y
  sta tmpvdiflo
  lda frequenzhi+2,y
  sbc frequenzhi,y
.l9
  lsr a              ;divide difference by
  ror tmpvdiflo    ;2 for each vibrdepth
  dec vibrdepth
  bpl l9
  sta tmpvdifhi

  lda frequenzlo,y ;save note frequency
  sta tmpvfrqlo
  lda frequenzhi,y
  sta tmpvfrqhi

  lda savelnthcc,x ;no vibrato if note
  and #$1f         ;length less than 8
  cmp #8
  bcc l10

  ldy oscilatval
.l11
  dey              ;depending on the osc
  bmi l10            ;value, add the vibr
  clc              ;freq that many times
  lda tmpvfrqlo    ;to the base freq
  adc tmpvdiflo
  sta tmpvfrqlo
  lda tmpvfrqhi
  adc tmpvdifhi
  sta tmpvfrqhi
  jmp l11
.l10
  ldy tmpregofst   ;save the final
  lda tmpvfrqlo    ;frequencies
  sta sid+$0,y
  lda tmpvfrqhi
  sta sid+$1,y


;pulse-width timbre routine
;depending on the control/speed byte in
;the instrument datastructure, the pulse
;width is of course inc/decremented to
;produce timbre

;strangely the delay value is also the
;size of the inc/decrements

.pulsework

  lda pulsevalue   ;check for pulsework
  beq portamento   ;needed this instr

  ldy instnumby8
  and #$1f
  dec pulsedelay,x ;pulsedelay-1
  bpl portamento

  sta pulsedelay,x ;reset pulsedelay

  lda pulsevalue   ;restrict pulse speed
  and #$e0         ;from $00-$1f
  sta pulsespeed

  lda pulsedir,x   ;pulsedir 0 is up and
  bne pulsedown    ;1 is down

  lda pulsespeed   ;pulse width up
  clc
  adc instr+0,y    ;add the pulsespeed
  pha              ;to the pulse width
  lda instr+1,y
  adc #$00
  and #$0f
  pha
  cmp #$0e         ;go pulsedown when
  bne dumpulse     ;the pulse value
  inc pulsedir,x   ;reaches max ($0exx)
  jmp dumpulse

.pulsedown

  sec              ;pulse width down
  lda instr+0,y
  sbc pulsespeed   ;sub the pulsespeed
  pha              ;from the pulse width
  lda instr+1,y
  sbc #$00
  and #$0f
  pha
  cmp #$08         ;go pulseup when
  bne dumpulse     ;the pulse value
  dec pulsedir,x   ;reaches min ($08xx)

.dumpulse

  stx tempstore    ;dump pulse width to
  ldx tmpregofst   ;chip and back into
  pla              ;the instr data str
  sta instr+1,y
  sta sid+$3,x
  pla
  sta instr+0,y
  sta sid+$2,x
  ldx tempstore


;portamento routine
;portamento comes from the second byte
;if it's a negative value

.portamento

  ldy tmpregofst
  lda portaval,x   ;check for portamento
  beq drums        ;none

  and #$7e         ;toad unwanted bits
  sta tempstore

  lda portaval,x   ;bit0 signals up/down
  and #$01
  beq portup

  sec              ;portamento down
  lda savefreqlo,x ;sub portaval from
  sbc tempstore    ;current frequency
  sta savefreqlo,x
  sta sid+$0,y
  lda savefreqhi,x
  sbc #$00         ;(word arithmetic)
  sta savefreqhi,x
  sta sid+$1,y
  jmp drums

.portup

  clc              ;portamento up
  lda savefreqlo,x ;add portval to
  adc tempstore    ;current frequency
  sta savefreqlo,x
  sta sid+$0,y
  lda savefreqhi,x
  adc #$00
  sta savefreqhi,x
  sta sid+$1,y


;bit0 instrfx are the drum routines
;the actual drum timbre depends on the
;crtl register value for the instrument
;ctrlreg 0 is always noise
;ctrlreg x is noise for 1st vbl and x
;from then on

;see that the drum is made by rapid hi
;to low frequency slide with fast attack
;and decay

.drums

  lda instrfx      ;check if drums
  and #$01         ;needed this instr
  beq skydive

  lda savefreqhi,x ;don't bother if freq
  beq skydive      ;can't go any lower

  lda lengthleft,x ;or if the note has
  beq skydive      ;finished

  lda savelnthcc,x ;check if this is the
  and #$1f         ;first vbl for this
  sec              ;instrument-note
  sbc #$01
  cmp lengthleft,x
  ldy tmpregofst
  bcc firstime

  lda savefreqhi,x ;not the first time
  dec savefreqhi,x ;so dec freqhi for
  sta sid+$1,y      ;drum sound

  lda voicectrl,x  ;if ctrlreg is 0 then
  and #$fe         ;noise is used always
  bne dumpctrl

.firstime

  lda savefreqhi,x ;noise is used for
  sta sid+$1,y      ;the first vbl also
  lda #$80         ;(set noise)

.dumpctrl

  sta sid+$4,y


;bit1 instrfx is the skydive
;a long portamento-down from the note
;to zerofreq

.skydive

  lda instrfx      ;check if skydive
  and #$02         ;needed this instr
  beq octarp

  lda counter      ;every 2nd vbl
  and #$01
  beq octarp

  lda savefreqhi,x ;check if skydive
  beq octarp        ;already complete

  dec savefreqhi,x ;decr and save the
  ldy tmpregofst   ;high byte freq
  sta sid+$1,y


;bit2 instrfx is an octave arpeggio
;pretty tame huh?

.octarp

  lda instrfx      ;check if arpt needed
  and #$04
  beq loopcont

  lda counter      ;only 2 arpt values
  and #$01
  beq l12

  lda notenum,x    ;odd, note+12
  clc
  adc #$0c
  jmp l13
.l12
  lda notenum,x    ;even, note
.l13
  asl a            ;dump the corresponding
  tay              ;frequencies
  lda frequenzlo,y
  sta tempfreq
  lda frequenzhi,y
  ldy tmpregofst
  sta sid+$1,y
  lda tempfreq
  sta sid+$0,y


;==========
;end of dbf loop

.loopcont

  dex              ;dbf mainloop
  bmi musicend
  jmp mainloop

.musicend

  rts


;====================================
;frequenz data
;====================================

.frequenzlo
 equs $16
.frequenzhi
 equs $01
 equs $27,$01,$38,$01,$4b,$01
 equs $5f,$01,$73,$01,$8a,$01,$a1,$01
 equs $ba,$01,$d4,$01,$f0,$01,$0e,$02
 equs $2d,$02,$4e,$02,$71,$02,$96,$02
 equs $bd,$02,$e7,$02,$13,$03,$42,$03
 equs $74,$03,$a9,$03,$e0,$03,$1b,$04
 equs $5a,$04,$9b,$04,$e2,$04,$2c,$05
 equs $7b,$05,$ce,$05,$27,$06,$85,$06
 equs $e8,$06,$51,$07,$c1,$07,$37,$08
 equs $b4,$08,$37,$09,$c4,$09,$57,$0a
 equs $f5,$0a,$9c,$0b,$4e,$0c,$09,$0d
 equs $d0,$0d,$a3,$0e,$82,$0f,$6e,$10
 equs $68,$11,$6e,$12,$88,$13,$af,$14
 equs $eb,$15,$39,$17,$9c,$18,$13,$1a
 equs $a1,$1b,$46,$1d,$04,$1f,$dc,$20
 equs $d0,$22,$dc,$24,$10,$27,$5e,$29
 equs $d6,$2b,$72,$2e,$38,$31,$26,$34
 equs $42,$37,$8c,$3a,$08,$3e,$b8,$41
 equs $a0,$45,$b8,$49,$20,$4e,$bc,$52
 equs $ac,$57,$e4,$5c,$70,$62,$4c,$68
 equs $84,$6e,$18,$75,$10,$7c,$70,$83
 equs $40,$8b,$70,$93,$40,$9c,$78,$a5
 equs $58,$af,$c8,$b9,$e0,$c4,$98,$d0
 equs $08,$dd,$30,$ea,$20,$f8,$2e,$fd


.regoffsets	 equs $00,$07,$0e
.tmpregofst	 equs $00
.posoffset	 equs $00,$00,$00
.patoffset	 equs $00,$00,$00
.lengthleft	 equs $00,$00,$00
.savelnthcc	 equs $00,$00,$00
.voicectrl	 equs $00,$00,$00
.notenum	 equs $00,$00,$00
.instrnr	 equs $00,$00,$00
.appendfl	 equs $00
.templnthcc	 equs $00
.tempfreq	 equs $00
.tempstore	 equs $00
.tempctrl	 equs $00
.vibrdepth	 equs $00
.pulsevalue	 equs $00
.tmpvdiflo	 equs $00
.tmpvdifhi	 equs $00
.tmpvfrqlo	 equs $00
.tmpvfrqhi	 equs $00
.oscilatval	 equs $00
.pulsedelay	 equs $00,$00,$00
.pulsedir	 equs $00,$00,$00
.speed		 equs $00
.resetspd	 equs $01
.instnumby8	 equs $00
.mstatus	 equs $c0
.savefreqhi	 equs $00,$00,$00
.savefreqlo	 equs $00,$00,$00
.portaval	 equs $00,$00,$00
.instrfx	 equs $00
.pulsespeed	 equs $00
.counter	 equs $00
.currtrkhi	 equs $00,$00,$00
.currtrklo	 equs $00,$00,$00

;====================================
;monty on the run main theme
;====================================

.songs
 equs <montymaintr1
 equs <montymaintr2
 equs <montymaintr3
 equs >montymaintr1
 equs >montymaintr2
 equs >montymaintr3


;====================================
;pointers to the patterns

;low pointers
.patptl
 equs <pth00
 equs <pth01
 equs <pth02
 equs <pth03
 equs <pth04
 equs <pth05
 equs <pth06
 equs <pth07
 equs <pth08
 equs <pth09
 equs <pth0a
 equs <pth0b
 equs <pth0c
 equs <pth0d
 equs <pth0e
 equs <pth0f
 equs <pth10
 equs <pth11
 equs <pth12
 equs <pth13
 equs <pth14
 equs <pth15
 equs <pth16
 equs <pth17
 equs <pth18
 equs <pth19
 equs <pth1a
 equs <pth1b
 equs <pth1c
 equs <pth1d
 equs <pth1e
 equs <pth1f
 equs <pth20
 equs <pth21
 equs <pth22
 equs <pth23
 equs <pth24
 equs <pth25
 equs <pth26
 equs <pth27
 equs <pth28
 equs <pth29
 equs <pth2a
 equs <pth2b
 equs <pth2c
 equs <pth2d
 equs 0
 equs <pth2f
 equs <pth30
 equs <pth31
 equs <pth32
 equs <pth33
 equs <pth34
 equs <pth35
 equs <pth36
 equs <pth37
 equs <pth38
 equs <pth39
 equs <pth3a
 equs <pth3b

;high pointers
.patpth
 equs >pth00
 equs >pth01
 equs >pth02
 equs >pth03
 equs >pth04
 equs >pth05
 equs >pth06
 equs >pth07
 equs >pth08
 equs >pth09
 equs >pth0a
 equs >pth0b
 equs >pth0c
 equs >pth0d
 equs >pth0e
 equs >pth0f
 equs >pth10
 equs >pth11
 equs >pth12
 equs >pth13
 equs >pth14
 equs >pth15
 equs >pth16
 equs >pth17
 equs >pth18
 equs >pth19
 equs >pth1a
 equs >pth1b
 equs >pth1c
 equs >pth1d
 equs >pth1e
 equs >pth1f
 equs >pth20
 equs >pth21
 equs >pth22
 equs >pth23
 equs >pth24
 equs >pth25
 equs >pth26
 equs >pth27
 equs >pth28
 equs >pth29
 equs >pth2a
 equs >pth2b
 equs >pth2c
 equs >pth2d
 equs 0
 equs >pth2f
 equs >pth30
 equs >pth31
 equs >pth32
 equs >pth33
 equs >pth34
 equs >pth35
 equs >pth36
 equs >pth37
 equs >pth38
 equs >pth39
 equs >pth3a
 equs >pth3b


;====================================
;tracks
;====================================

;track1
.montymaintr1
 equs $11,$14,$17,$1a,$00,$27,$00,$28
 equs $03,$05,$00,$27,$00,$28,$03,$05
 equs $07,$3a,$14,$17,$00,$27,$00,$28
 equs $2f,$30,$31,$31,$32,$33,$33,$34
 equs $34,$34,$34,$34,$34,$34,$34,$35
 equs $35,$35,$35,$35,$35,$36,$12,$37
 equs $38,$09,$2a,$09,$2b,$09,$0a,$09
 equs $2a,$09,$2b,$09,$0a,$0d,$0d,$0f
 equs $ff

;track2
.montymaintr2
 equs $12,$15,$18,$1b,$2d,$39,$39
 equs $39,$39,$39,$39,$2c,$39,$39,$39
 equs $39,$39,$39,$2c,$39,$39,$39,$01
 equs $01,$29,$29,$2c,$15,$18,$39,$39
 equs $39,$39,$39,$39,$39,$39,$39,$39
 equs $39,$39,$39,$39,$39,$39,$39,$39
 equs $39,$39,$39,$39,$39,$39,$39,$39
 equs $39,$39,$39,$39,$39,$01,$01,$01
 equs $29,$39,$39,$39,$01,$01,$01,$29
 equs $39,$39,$39,$39,$ff

;track3
.montymaintr3
 equs $13,$16,$19
 equs $1c,$02,$02,$1d,$1e,$02,$02,$1d
 equs $1f,$04,$04,$20,$20,$06,$02,$02
 equs $1d,$1e,$02,$02,$1d,$1f,$04,$04
 equs $20,$20,$06,$08,$08,$08,$08,$21
 equs $21,$21,$21,$22,$22,$22,$23,$22
 equs $24,$25,$3b,$26,$26,$26,$26,$26
 equs $26,$26,$26,$26,$26,$26,$26,$26
 equs $26,$26,$26,$02,$02,$1d,$1e,$02
 equs $02,$1d,$1f,$2f,$2f,$2f,$2f,$2f
 equs $2f,$2f,$2f,$2f,$2f,$2f,$2f,$2f
 equs $0b,$0b,$1d,$1d,$0b,$0b,$1d,$0b
 equs $0b,$0b,$0c,$0c,$1d,$1d,$1d,$10
 equs $0b,$0b,$1d,$1d,$0b,$0b,$1d,$0b
 equs $0b,$0b,$0c,$0c,$1d,$1d,$1d,$10
 equs $0b,$1d,$0b,$1d,$0b,$1d,$0b,$1d
 equs $0b,$0c,$1d,$0b,$0c,$23,$0b,$0b
 equs $ff


;====================================
;patterns
;====================================

.pth00
 equs $83,$00,$37,$01,$3e,$01,$3e,$03
 equs $3d,$03,$3e,$03,$43,$03,$3e,$03
 equs $3d,$03,$3e,$03,$37,$01,$3e,$01
 equs $3e,$03,$3d,$03,$3e,$03,$43,$03
 equs $42,$03,$43,$03,$45,$03,$46,$01
 equs $48,$01,$46,$03,$45,$03,$43,$03
 equs $4b,$01,$4d,$01,$4b,$03,$4a,$03
 equs $48,$ff

.pth27
 equs $1f,$4a,$ff

.pth28
 equs $03,$46,$01,$48,$01,$46,$03,$45
 equs $03,$4a,$0f,$43,$ff

.pth03
 equs $bf,$06
 equs $48,$07,$48,$01,$4b,$01,$4a,$01
 equs $4b,$01,$4a,$03,$4b,$03,$4d,$03
 equs $4b,$03,$4a,$3f,$48,$07,$48,$01
 equs $4b,$01,$4a,$01,$4b,$01,$4a,$03
 equs $4b,$03,$4d,$03,$4b,$03,$48,$3f
 equs $4c,$07,$4c,$01,$4f,$01,$4e,$01
 equs $4f,$01,$4e,$03,$4f,$03,$51,$03
 equs $4f,$03,$4e,$3f,$4c,$07,$4c,$01
 equs $4f,$01,$4e,$01,$4f,$01,$4e,$03
 equs $4f,$03,$51,$03,$4f,$03,$4c,$ff

.pth05
 equs $83,$04,$26,$03,$29,$03,$28,$03
 equs $29,$03,$26,$03,$35,$03,$34,$03
 equs $32,$03,$2d,$03,$30,$03,$2f,$03
 equs $30,$03,$2d,$03,$3c,$03,$3b,$03
 equs $39,$03,$30,$03,$33,$03,$32,$03
 equs $33,$03,$30,$03,$3f,$03,$3e,$03
 equs $3c,$03,$46,$03,$45,$03,$43,$03
 equs $3a,$03,$39,$03,$37,$03,$2e,$03
 equs $2d,$03,$26,$03,$29,$03,$28,$03
 equs $29,$03,$26,$03,$35,$03,$34,$03
 equs $32,$03,$2d,$03,$30,$03,$2f,$03
 equs $30,$03,$2d,$03,$3c,$03,$3b,$03
 equs $39,$03,$30,$03,$33,$03,$32,$03
 equs $33,$03,$30,$03,$3f,$03,$3e,$03
 equs $3c,$03,$34,$03,$37,$03,$36,$03
 equs $37,$03,$34,$03,$37,$03,$3a,$03
 equs $3d

.pth3a
 equs $03,$3e,$07,$3e,$07,$3f,$07
 equs $3e,$03,$3c,$07,$3e,$57,$ff

.pth07
 equs $8b
 equs $00,$3a,$01,$3a,$01,$3c,$03,$3d
 equs $03,$3f,$03,$3d,$03,$3c,$0b,$3a
 equs $03,$39,$07,$3a,$81,$06,$4b,$01
 equs $4d,$01,$4e,$01,$4d,$01,$4e,$01
 equs $4d,$05,$4b,$81,$00,$3a,$01,$3c
 equs $01,$3d,$03,$3f,$03,$3d,$03,$3c
 equs $03,$3a,$03,$39,$1b,$3a,$0b,$3b
 equs $01,$3b,$01,$3d,$03,$3e,$03,$40
 equs $03,$3e,$03,$3d,$0b,$3b,$03,$3a
 equs $07,$3b,$81,$06,$4c,$01,$4e,$01
 equs $4f,$01,$4e,$01,$4f,$01,$4e,$05
 equs $4c,$81,$00,$3b,$01,$3d,$01,$3e
 equs $03,$40,$03,$3e,$03,$3d,$03,$3b
 equs $03,$3a,$1b,$3b,$8b,$05,$35,$03
 equs $33,$07,$32,$03,$30,$03,$2f,$0b
 equs $30,$03,$32,$0f,$30,$0b,$35,$03
 equs $33,$07,$32,$03,$30,$03,$2f,$1f
 equs $30,$8b,$00,$3c,$01,$3c,$01,$3e
 equs $03,$3f,$03,$41,$03,$3f,$03,$3e
 equs $0b,$3d,$01,$3d,$01,$3f,$03,$40
 equs $03,$42,$03,$40,$03,$3f,$03,$3e
 equs $01,$3e,$01,$40,$03,$41,$03,$40
 equs $03,$3e,$03,$3d,$03,$3e,$03,$3c
 equs $03,$3a,$01,$3a,$01,$3c,$03,$3d
 equs $03,$3c,$03,$3a,$03,$39,$03,$3a
 equs $03,$3c,$ff

.pth09
 equs $83,$00,$32,$01,$35,$01,$34,$03
 equs $32,$03,$35,$03,$34,$03,$32,$03
 equs $35,$01,$34,$01,$32,$03,$32,$03
 equs $3a,$03,$39,$03,$3a,$03,$32,$03
 equs $3a,$03,$39,$03,$3a,$ff

.pth2a
 equs $03,$34,$01,$37,$01,$35,$03,$34
 equs $03,$37,$03,$35,$03,$34,$03,$37
 equs $01,$35,$01,$34,$03,$34,$03,$3a
 equs $03,$39,$03,$3a,$03,$34,$03,$3a
 equs $03,$39,$03,$3a,$ff

.pth2b
 equs $03,$39,$03,$38,$03,$39,$03,$3a
 equs $03,$39,$03,$37,$03,$35,$03,$34
 equs $03,$35,$03,$34,$03,$35,$03,$37
 equs $03,$35,$03,$34,$03,$32,$03,$31
 equs $ff

.pth0a
 equs $03
 equs $37,$01,$3a,$01,$39,$03,$37,$03
 equs $3a,$03,$39,$03,$37,$03,$3a,$01
 equs $39,$01,$37,$03,$37,$03,$3e,$03
 equs $3d,$03,$3e,$03,$37,$03,$3e,$03
 equs $3d,$03,$3e,$03,$3d,$01,$40,$01
 equs $3e,$03,$3d,$03,$40,$01,$3e,$01
 equs $3d,$03,$40,$03,$3e,$03,$40,$03
 equs $40,$01,$43,$01,$41,$03,$40,$03
 equs $43,$01,$41,$01,$40,$03,$43,$03
 equs $41,$03,$43,$03,$43,$01,$46,$01
 equs $45,$03,$43,$03,$46,$01,$45,$01
 equs $43,$03,$46,$03,$45,$03,$43,$01
 equs $48,$01,$49,$01,$48,$01,$46,$01
 equs $45,$01,$46,$01,$45,$01,$43,$01
 equs $41,$01,$43,$01,$41,$01,$40,$01
 equs $3d,$01,$39,$01,$3b,$01,$3d,$ff

.pth0d
 equs $01,$3e,$01,$39,$01,$35,$01,$39
 equs $01,$3e,$01,$39,$01,$35,$01,$39
 equs $03,$3e,$01,$41,$01,$40,$03,$40
 equs $01,$3d,$01,$3e,$01,$40,$01,$3d
 equs $01,$39,$01,$3d,$01,$40,$01,$3d
 equs $01,$39,$01,$3d,$03,$40,$01,$43
 equs $01,$41,$03,$41,$01,$3e,$01,$40
 equs $01,$41,$01,$3e,$01,$39,$01,$3e
 equs $01,$41,$01,$3e,$01,$39,$01,$3e
 equs $03,$41,$01,$45,$01,$43,$03,$43
 equs $01,$40,$01,$41,$01,$43,$01,$40
 equs $01,$3d,$01,$40,$01,$43,$01,$40
 equs $01,$3d,$01,$40,$01,$46,$01,$43
 equs $01,$45,$01,$46,$01,$44,$01,$43
 equs $01,$40,$01,$3d,$ff

.pth0f
 equs $01,$3e,$01
 equs $39,$01,$35,$01,$39,$01,$3e,$01
 equs $39,$01,$35,$01,$39,$01,$3e,$01
 equs $39,$01,$35,$01,$39,$01,$3e,$01
 equs $39,$01,$35,$01,$39,$01,$3e,$01
 equs $3a,$01,$37,$01,$3a,$01,$3e,$01
 equs $3a,$01,$37,$01,$3a,$01,$3e,$01
 equs $3a,$01,$37,$01,$3a,$01,$3e,$01
 equs $3a,$01,$37,$01,$3a,$01,$40,$01
 equs $3d,$01,$39,$01,$3d,$01,$40,$01
 equs $3d,$01,$39,$01,$3d,$01,$40,$01
 equs $3d,$01,$39,$01,$3d,$01,$40,$01
 equs $3d,$01,$39,$01,$3d,$01,$41,$01
 equs $3e,$01,$39,$01,$3e,$01,$41,$01
 equs $3e,$01,$39,$01,$3e,$01,$41,$01
 equs $3e,$01,$39,$01,$3e,$01,$41,$01
 equs $3e,$01,$39,$01,$3e,$01,$43,$01
 equs $3e,$01,$3a,$01,$3e,$01,$43,$01
 equs $3e,$01,$3a,$01,$3e,$01,$43,$01
 equs $3e,$01,$3a,$01,$3e,$01,$43,$01
 equs $3e,$01,$3a,$01,$3e,$01,$43,$01
 equs $3f,$01,$3c,$01,$3f,$01,$43,$01
 equs $3f,$01,$3c,$01,$3f,$01,$43,$01
 equs $3f,$01,$3c,$01,$3f,$01,$43,$01
 equs $3f,$01,$3c,$01,$3f,$01,$45,$01
 equs $42,$01,$3c,$01,$42,$01,$45,$01
 equs $42,$01,$3c,$01,$42,$01,$48,$01
 equs $45,$01,$42,$01,$45,$01,$4b,$01
 equs $48,$01,$45,$01,$48,$01,$4b,$01
 equs $4a,$01,$48,$01,$4a,$01,$4b,$01
 equs $4a,$01,$48,$01,$4a,$01,$4b,$01
 equs $4a,$01,$48,$01,$4a,$01,$4c,$01
 equs $4e,$03,$4f,$ff

.pth11
 equs $bf,$06,$56,$1f,$57,$1f,$56,$1f
 equs $5b,$1f,$56,$1f,$57,$1f,$56,$1f
 equs $4f,$ff

.pth12
 equs $bf,$0c,$68,$7f,$7f,$7f,$7f,$7f
 equs $7f,$7f,$ff

.pth13
 equs $bf,$08,$13,$3f,$13,$3f,$13,$3f
 equs $13,$3f,$13,$3f,$13,$3f,$13,$1f
 equs $13,$ff

.pth14
 equs $97,$09,$2e,$03,$2e,$1b,$32,$03
 equs $32,$1b,$31,$03,$31,$1f,$34,$43
 equs $17,$32,$03,$32,$1b,$35,$03,$35
 equs $1b,$34,$03,$34,$0f,$37,$8f,$0a
 equs $37,$43,$ff

.pth15
 equs $97,$09,$2b,$03,$2b,$1b,$2e,$03
 equs $2e,$1b,$2d,$03,$2d,$1f,$30,$43
 equs $17,$2e,$03,$2e,$1b,$32,$03,$32
 equs $1b,$31,$03,$31,$0f,$34,$8f,$0a
 equs $34,$43,$ff

.pth16
 equs $0f,$1f,$0f,$1f,$0f,$1f,$0f,$1f
 equs $0f,$1f,$0f,$1f,$0f,$1f,$0f,$1f
 equs $0f,$1f,$0f,$1f,$0f,$1f,$0f,$1f
 equs $0f,$1f,$0f,$1f,$0f,$1f,$0f,$1f
 equs $ff

.pth17
 equs $97,$09,$33,$03,$33,$1b,$37,$03
 equs $37,$1b,$36,$03,$36,$1f,$39,$43
 equs $17,$37,$03,$37,$1b,$3a,$03,$3a
 equs $1b,$39,$03,$39,$2f,$3c,$21,$3c
 equs $21,$3d,$21,$3e,$21,$3f,$21,$40
 equs $21,$41,$21,$42,$21,$43,$21,$44
 equs $01,$45,$ff

.pth18
 equs $97,$09,$30,$03,$30,$1b,$33,$03
 equs $33,$1b,$32,$03,$32,$1f,$36,$43
 equs $17,$33,$03,$33,$1b,$37,$03,$37
 equs $1b,$36,$03,$36,$2f,$39,$21,$39
 equs $21,$3a,$21,$3b,$21,$3c,$21,$3d
 equs $21,$3e,$21,$3f,$21,$40,$21,$41
 equs $01,$42,$ff

.pth19
 equs $0f,$1a,$0f,$1a,$0f,$1a,$0f,$1a
 equs $0f,$1a,$0f,$1a,$0f,$1a,$0f,$1a
 equs $0f,$1a,$0f,$1a,$0f,$1a,$0f,$1a
 equs $0f,$1a,$0f,$1a,$0f,$1a,$0f,$1a
 equs $ff

.pth1a
 equs $1f,$46,$bf,$0a,$46,$7f,$7f,$ff

.pth1b
 equs $1f,$43,$bf,$0a,$43,$7f,$ff

.pth1c
 equs $83,$02,$13,$03,$13,$03,$1e,$03
 equs $1f,$03,$13,$03,$13,$03,$1e,$03
 equs $1f,$03,$13,$03,$13,$03,$1e,$03
 equs $1f,$03,$13,$03,$13,$03,$1e,$03
 equs $1f,$03,$13,$03,$13,$03,$1e,$03
 equs $1f,$03,$13,$03,$13,$03,$1e,$03
 equs $1f,$03,$13,$03,$13,$03,$1e,$03
 equs $1f,$03,$13,$03,$13,$03,$1e,$03
 equs $1f,$ff

.pth29
 equs $8f,$0b,$38,$4f,$ff

.pth2c
 equs $83,$0e,$32,$07,$32,$07,$2f,$07
 equs $2f,$03,$2b,$87,$0b,$46,$83,$0e
 equs $2c,$03,$2c,$8f,$0b,$32,$ff

.pth2d
 equs $43,$83,$0e,$32,$03,$32,$03,$2f
 equs $03,$2f,$03,$2c,$87,$0b,$38,$ff

.pth39
 equs $83,$01
 equs $43,$01,$4f,$01,$5b,$87,$03,$2f
 equs $83,$01,$43,$01,$4f,$01,$5b,$87
 equs $03,$2f,$83,$01,$43,$01,$4f,$01
 equs $5b,$87,$03,$2f,$83,$01,$43,$01
 equs $4f,$01,$5b,$87,$03,$2f,$83,$01
 equs $43,$01,$4f,$01,$5b,$87,$03,$2f
 equs $83,$01,$43,$01,$4f,$01,$5b,$87
 equs $03,$2f

.pth01
 equs $83,$01,$43,$01,$4f,$01,$5b,$87
 equs $03,$2f,$83,$01,$43,$01,$4f,$01
 equs $5b,$87,$03,$2f,$ff

.pth02
 equs $83,$02,$13,$03,$13,$03,$1f,$03
 equs $1f,$03,$13,$03,$13,$03,$1f,$03
 equs $1f,$ff

.pth1d
 equs $03,$15,$03,$15,$03,$1f,$03,$21
 equs $03,$15,$03,$15,$03,$1f,$03,$21
 equs $ff

.pth1e
 equs $03,$1a,$03,$1a,$03,$1c,$03,$1c
 equs $03,$1d,$03,$1d,$03,$1e,$03,$1e
 equs $ff

.pth1f
 equs $03,$1a,$03,$1a,$03,$24,$03,$26
 equs $03,$13,$03,$13,$07,$1f,$ff

.pth04
 equs $03,$18,$03,$18,$03,$24,$03,$24
 equs $03,$18,$03,$18,$03,$24,$03,$24
 equs $03,$20,$03,$20,$03,$2c,$03,$2c
 equs $03,$20,$03,$20,$03,$2c,$03,$2c
 equs $ff

.pth20
 equs $03,$19,$03,$19,$03
 equs $25,$03,$25,$03,$19,$03,$19,$03
 equs $25,$03,$25,$03,$21,$03,$21,$03
 equs $2d,$03,$2d,$03,$21,$03,$21,$03
 equs $2d,$03,$2d,$ff

.pth06
 equs $03,$1a,$03,$1a
 equs $03,$26,$03,$26,$03,$1a,$03,$1a
 equs $03,$26,$03,$26,$03,$15,$03,$15
 equs $03,$21,$03,$21,$03,$15,$03,$15
 equs $03,$21,$03,$21,$03,$18,$03,$18
 equs $03,$24,$03,$24,$03,$18,$03,$18
 equs $03,$24,$03,$24,$03,$1f,$03,$1f
 equs $03,$2b,$03,$2b,$03,$1f,$03,$1f
 equs $03,$2b,$03,$2b,$03,$1a,$03,$1a
 equs $03,$26,$03,$26,$03,$1a,$03,$1a
 equs $03,$26,$03,$26,$03,$15,$03,$15
 equs $03,$21,$03,$21,$03,$15,$03,$15
 equs $03,$21,$03,$21,$03,$18,$03,$18
 equs $03,$24,$03,$24,$03,$18,$03,$18
 equs $03,$24,$03,$24,$03,$1c,$03,$1c
 equs $03,$28,$03,$28,$03,$1c,$03,$1c
 equs $03,$28,$03,$28

.pth3b
 equs $83,$04,$36,$07
 equs $36,$07,$37,$07,$36,$03,$33,$07
 equs $32,$57,$ff

.pth08
 equs $83,$02,$1b,$03,$1b,$03,$27,$03
 equs $27,$03,$1b,$03,$1b,$03,$27,$03
 equs $27,$ff

.pth21
 equs $03,$1c,$03,$1c,$03,$28,$03,$28
 equs $03,$1c,$03,$1c,$03,$28,$03,$28
 equs $ff

.pth22
 equs $03,$1d,$03,$1d,$03,$29,$03,$29
 equs $03,$1d,$03,$1d,$03,$29,$03,$29
 equs $ff

.pth23
 equs $03,$18,$03,$18,$03,$24,$03,$24
 equs $03,$18,$03,$18,$03,$24,$03,$24
 equs $ff

.pth24
 equs $03,$1e,$03,$1e,$03,$2a,$03,$2a
 equs $03,$1e,$03,$1e,$03,$2a,$03,$2a
 equs $ff

.pth25
 equs $83,$05,$26,$01,$4a,$01,$34,$03
 equs $29,$03,$4c,$03,$4a,$03,$31,$03
 equs $4a,$03,$24,$03,$22,$01,$46,$01
 equs $30,$03,$25,$03,$48,$03,$46,$03
 equs $2d,$03,$46,$03,$24,$ff

.pth0b
 equs $83,$02,$1a,$03,$1a,$03,$26,$03
 equs $26,$03,$1a,$03,$1a,$03,$26,$03
 equs $26,$ff

.pth0c
 equs $03,$13,$03,$13,$03,$1d,$03,$1f
 equs $03,$13,$03,$13,$03,$1d,$03,$1f
 equs $ff

.pth26
 equs $87,$02,$1a,$87,$03,$2f,$83,$02
 equs $26,$03,$26,$87,$03,$2f,$ff

.pth10
 equs $07,$1a,$4f,$47,$ff

.pth0e
 equs $03,$1f,$03,$1f,$03,$24,$03,$26
 equs $07,$13,$47,$ff

.pth30
 equs $bf,$0f,$32,$0f,$32,$8f,$90,$30
 equs $3f,$32,$13,$32,$03,$32,$03,$35
 equs $03,$37,$3f,$37,$0f,$37,$8f,$90
 equs $30,$3f,$32,$13,$32,$03,$2d,$03
 equs $30,$03,$32,$ff

.pth31
 equs $0f,$32
 equs $af,$90,$35,$0f,$37,$a7,$99,$37
 equs $07,$35,$3f,$32,$13,$32,$03,$32
 equs $a3,$e8,$35,$03,$37,$0f,$35,$af
 equs $90,$37,$0f,$37,$a7,$99,$37,$07
 equs $35,$3f,$32,$13,$32,$03,$2d,$a3
 equs $e8,$30,$03,$32,$ff

.pth32
 equs $07,$32,$03
 equs $39,$13,$3c,$a7,$9a,$37,$a7,$9b
 equs $38,$07,$37,$03,$35,$03,$32,$03
 equs $39,$1b,$3c,$a7,$9a,$37,$a7,$9b
 equs $38,$07,$37,$03,$35,$03,$32,$03
 equs $39,$03,$3c,$03,$3e,$03,$3c,$07
 equs $3e,$03,$3c,$03,$39,$a7,$9a,$37
 equs $a7,$9b,$38,$07,$37,$03,$35,$03
 equs $32,$af,$90,$3c,$1f,$3e,$43,$03
 equs $3e,$03,$3c,$03,$3e,$ff

.pth33
 equs $03,$3e
 equs $03,$3e,$a3,$e8,$3c,$03,$3e,$03
 equs $3e,$03,$3e,$a3,$e8,$3c,$03,$3e
 equs $03,$3e,$03,$3e,$a3,$e8,$3c,$03
 equs $3e,$03,$3e,$03,$3e,$a3,$e8,$3c
 equs $03,$3e,$af,$91,$43,$1f,$41,$43
 equs $03,$3e,$03,$41,$03,$43,$03,$43
 equs $03,$43,$a3,$e8,$41,$03,$43,$03
 equs $43,$03,$43,$a3,$e8,$41,$03,$43
 equs $03,$45,$03,$48,$a3,$fd,$45,$03
 equs $44,$01,$43,$01,$41,$03,$3e,$03
 equs $3c,$03,$3e,$2f,$3e,$bf,$98,$3e
 equs $43,$03,$3e,$03,$3c,$03,$3e,$ff

.pth34
 equs $03,$4a,$03,$4a,$a3,$f8,$48,$03
 equs $4a,$03,$4a,$03,$4a,$a3,$f8,$48
 equs $03,$4a,$ff

.pth35
 equs $01,$51,$01,$54,$01
 equs $51,$01,$54,$01,$51,$01,$54,$01
 equs $51,$01,$54,$01,$51,$01,$54,$01
 equs $51,$01,$54,$01,$51,$01,$54,$01
 equs $51,$01,$54,$ff

.pth36
 equs $01,$50,$01,$4f
 equs $01,$4d,$01,$4a,$01,$4f,$01,$4d
 equs $01,$4a,$01,$48,$01,$4a,$01,$48
 equs $01,$45,$01,$43,$01,$44,$01,$43
 equs $01,$41,$01,$3e,$01,$43,$01,$41
 equs $01,$3e,$01,$3c,$01,$3e,$01,$3c
 equs $01,$39,$01,$37,$01,$38,$01,$37
 equs $01,$35,$01,$32,$01,$37,$01,$35
 equs $01,$32,$01,$30,$ff

.pth37
 equs $5f,$5f,$5f
 equs $47,$83,$0e,$32,$07,$32,$07,$2f
 equs $03,$2f,$07,$2f,$97,$0b,$3a,$5f
 equs $5f,$47,$8b,$0e,$32,$03,$32,$03
 equs $2f,$03,$2f,$47,$97,$0b,$3a,$5f
 equs $5f,$47,$83,$0e,$2f,$0b,$2f,$03
 equs $2f,$03,$2f,$87,$0b,$30,$17,$3a
 equs $5f,$8b,$0e,$32,$0b,$32,$0b,$2f
 equs $0b,$2f,$07,$2c,$07,$2c,$ff

.pth38
 equs $87
 equs $0b,$34,$17,$3a,$5f,$5f,$84,$0e
 equs $32,$04,$32,$05,$32,$04,$2f,$04
 equs $2f,$05,$2f,$47,$97,$0b,$3a,$5f
 equs $5f,$84,$0e,$32,$04,$32,$05,$32
 equs $04,$2f,$04,$2f,$05,$2f,$ff

.pth2f
 equs $03,$1a,$03,$1a,$03
 equs $24,$03,$26,$03,$1a,$03,$1a,$03
 equs $18,$03,$19,$03,$1a,$03,$1a,$03
 equs $24,$03,$26,$03,$1a,$03,$1a,$03
 equs $18,$03,$19,$03,$18,$03,$18,$03
 equs $22,$03,$24,$03,$18,$03,$18,$03
 equs $16,$03,$17,$03,$18,$03,$18,$03
 equs $22,$03,$24,$03,$18,$03,$18,$03
 equs $16,$03,$17,$03,$13,$03,$13,$03
 equs $1d,$03,$1f,$03,$13,$03,$13,$03
 equs $1d,$03,$1e,$03,$13,$03,$13,$03
 equs $1d,$03,$1f,$03,$13,$03,$13,$03
 equs $1d,$03,$1e,$03,$1a,$03,$1a,$03
 equs $24,$03,$26,$03,$1a,$03,$1a,$03
 equs $18,$03,$19,$03,$1a,$03,$1a,$03
 equs $24,$03,$26,$03,$1a,$03,$1a,$03
 equs $18,$03,$19,$ff


;====================================
;instruments
;====================================

.instr
 equs $80,$09,$41,$48,$60,$03,$81,$00
 equs $00,$08,$81,$02,$08,$00,$00,$01
 equs $a0,$02,$41,$09,$80,$00,$00,$00
 equs $00,$02,$81,$09,$09,$00,$00,$05
 equs $00,$08,$41,$08,$50,$02,$00,$04
 equs $00,$01,$41,$3f,$c0,$02,$00,$00
 equs $00,$08,$41,$04,$40,$02,$00,$00
 equs $00,$08,$41,$09,$00,$02,$00,$00
 equs $00,$09,$41,$09,$70,$02,$5f,$04
 equs $00,$09,$41,$4a,$69,$02,$81,$00
 equs $00,$09,$41,$40,$6f,$00,$81,$02
 equs $80,$07,$81,$0a,$0a,$00,$00,$01
 equs $00,$09,$41,$3f,$ff,$01,$e7,$02
 equs $00,$08,$41,$90,$f0,$01,$e8,$02
 equs $00,$08,$41,$06,$0a,$00,$00,$01
 equs $00,$09,$41,$19,$70,$02,$a8,$00
 equs $00,$02,$41,$09,$90,$02,$00,$00
 equs $00,$00,$11,$0a,$fa,$00,$00,$05
 equs $00,$08,$41,$37,$40,$02,$00,$00
 equs $00,$08,$11,$07,$70,$02,$00,$00

 equs $00,$00,$41,$fa,$00,$11,$08,$00	; Nyan
