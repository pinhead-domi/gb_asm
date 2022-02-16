INCLUDE "hardware.inc"

SECTION "Header", ROM0[$100]

	nop
	jp EntryPoint


; Interrupts
SECTION	"vblank interrupt",ROM0[$0040]
    jp	$FF80
SECTION	"LCDC interrupt",ROM0[$0048]
    reti
SECTION	"Timer overflow interrupt",ROM0[$0050]
    jp TimerRoutine
SECTION	"Serial interrupt",ROM0[$0058]
    reti
SECTION	"p1234 interrupt",ROM0[$0060]
    reti

SECTION "main", ROM0[$150]

DMACopy:
	push af
	ld a, HIGH(oam_buffer)
	ld [$FF46], a
	ld a, 40
DMAWait:
	dec a
	jr nz, DMAWait
	pop af
	reti
DMAEnd:

EntryPoint:
	; Shut down audio circuitry
	di
	ld a, 0
	ld [rNR52], a

;copy DMA routine to HRAM
	ld de, DMACopy
	ld hl, $FF80
	ld bc, DMAEnd - DMACopy
	call MemCpy

	xor a
	ld bc, 40*4
	ld hl, $C100
CleanC100:
	xor a
	ld [hli], a
	dec bc
	ld a, b
	or c
	jr nz, CleanC100

	ld hl, position
	ld [hl], $80
	ld hl, stepping
	ld [hl], 0

	xor a
	ld hl, delayval
	ld [hli], a
	ld [hl],a

	; Do not turn the LCD off outside of VBlank
WaitVBlank:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlank

	; Turn the LCD off
	ld a, 0
	ld [rLCDC], a

	; Copy the tile data
	ld de, Tiles
	ld hl, $9000
	ld bc, TilesEnd - Tiles
	call MemCpy

	; Copy the tilemap
	ld de, Tilemap
	ld hl, $9800
	ld bc, TilemapEnd - Tilemap
	call MemCpy

	ld de, Smiley
	ld hl, $8000
	ld bc, SmileyEnd - Smiley
	call MemCpy

	; Turn the LCD on
	ld a, LCDCF_ON | LCDCF_BGON
	ld [rLCDC], a

	; During the first (blank) frame, initialize display registers
	;ld a, %11100110
	;ld [rBGP], a
    ld a, %11000011  ; $91 plus bit 2
    ld [$ff40], a

	ld a, %00011011
	ld hl, $FF47
	ld [hli], a
	cpl
	ld [hli], a
	cpl
	ld [hl], a

SetupVars:
	ld hl, oam_buffer
	ld a, $80
	ld [hli], a
	ld a, $1A
	ld [hli], a

	xor a
	ld[hli], a
	ld[hl], a

	ld hl, velocity_y
	ld [hl], a

  ld a, 1
  ld hl, grounded
  ld [hl], a

WaitVBlankAgain:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlankAgain

SetupTimer:
	xor a
	ld [$FF06], a ;TMA resets TIMA to 0
	ld [$FF05], a ;Not certain if necesarry
	ld [timer_reset], a
	
	ld a, %00000100 ; 1 -> Timer enable, 00 -> clock/1024
	ld [$FF07], a

	;call $FF80
	ld a, %000000101 ; enable timer + vblank interrupts
	ld [$FFFF], a 
	ei

	ld hl, velocity_y
	ld [hl], 0

; Main loop
Again:
	
  ld b, 1
	call DelayTimer
 
  call ReadInput
	bit 0, h
  jr nz, Logic
  call Jump

Logic:
  call Gravity
  call Move

  jr Again

; Sets the players velocity_y if it is currently not airborn
Jump:
  push af
    ld a, [grounded]
    cp 1
    jr nz, JumpEnd
    xor a
    ld [grounded], a
    ld a, -12
    ld [velocity_y], a
JumpEnd:
  pop af
  ret

; b: number of timer cycles
DelayTimer:
	push af
DelayTimerLoop:
		halt 
		ld a, [timer_reset]
		cp a, 1
		jr nz, DelayTimerLoop
		xor a
		ld [timer_reset], a
		dec b
		jr nz, DelayTimerLoop
	pop af
	ret

TimerRoutine:
	push af
		ld a, 1
		ld [timer_reset], a
	pop af
	reti

; Moves the player by its current velocity, should its vertial position result in
; anything greater or equal to $80, it is reset to $80 and the player will be marked
; as grounded.
Move:
	push af
	push hl
		ld hl, velocity_y
		ld a, [position]
    add a, [hl]
    cp $80
    jr c, StorePosition
    xor a
    ld [velocity_y], a
    ld a, 1
    ld [grounded], a
    ld a, $80
    ld [position], a
StorePosition:
    ld [position], a
    ld [oam_buffer], a
	pop hl
	pop af
	ret

; Increases players velocity_y if not grounded
Gravity:
  push af
    ld a, [grounded]
    cp 1
    jr z, GravityEnd
    ld a, [velocity_y]
    add 1
    ld [velocity_y], a
GravityEnd:
  pop af
  ret

ReadInput:
	ld a, %11101111
	ld [$FF00], a
	
	ld a,[$FF00]
	rra
	rl l
	rra
	rl l
	rra
	rr h
	rra 
	rr h

	ld a, %11011111
	ld [$FF00], a

	rr l
	rr h
	rr l
	rr h

	ld a,[$FF00]
	rra 
	rr h
	rra 
	rr h
	rra 
	rr h
	rra 
	rr h
	ld l, 255
	ret

;  de: Source
;  hl: Destination
;  bc: Number of bytes
MemCpy:
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or c
	jr nz, MemCpy
	ret

SECTION "DMA Bytecode", ROM0
DMA_BYTECODE:
	db $F5, $3E, $C1, $EA, $46, $FF, $3E, $28, $3D, $20, $FD, $F1, $D9

SECTION "Tile data", ROM0

Tiles:
	db $00,$ff, $00,$ff, $00,$ff, $00,$ff, $00,$ff, $00,$ff, $00,$ff, $00,$ff
	db $00,$ff, $00,$80, $00,$80, $00,$80, $00,$80, $00,$80, $00,$80, $00,$80
	db $00,$ff, $00,$7e, $00,$7e, $00,$7e, $00,$7e, $00,$7e, $00,$7e, $00,$7e
	db $00,$ff, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01
	db $00,$ff, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00
	db $00,$ff, $00,$7f, $00,$7f, $00,$7f, $00,$7f, $00,$7f, $00,$7f, $00,$7f
	db $00,$ff, $03,$fc, $00,$f8, $00,$f0, $00,$e0, $20,$c0, $00,$c0, $40,$80
	db $00,$ff, $c0,$3f, $00,$1f, $00,$0f, $00,$07, $04,$03, $00,$03, $02,$01
	db $00,$80, $00,$80, $7f,$80, $00,$80, $00,$80, $7f,$80, $7f,$80, $00,$80
	db $00,$7e, $2a,$7e, $d5,$7e, $2a,$7e, $54,$7e, $ff,$00, $ff,$00, $00,$00
	db $00,$01, $00,$01, $ff,$01, $00,$01, $01,$01, $fe,$01, $ff,$01, $00,$01
	db $00,$80, $80,$80, $7f,$80, $80,$80, $00,$80, $ff,$80, $7f,$80, $80,$80
	db $00,$7f, $2a,$7f, $d5,$7f, $2a,$7f, $55,$7f, $ff,$00, $ff,$00, $00,$00
	db $00,$ff, $aa,$ff, $55,$ff, $aa,$ff, $55,$ff, $fa,$07, $fd,$07, $02,$07
	db $00,$7f, $2a,$7f, $d5,$7f, $2a,$7f, $55,$7f, $aa,$7f, $d5,$7f, $2a,$7f
	db $00,$ff, $80,$ff, $00,$ff, $80,$ff, $00,$ff, $80,$ff, $00,$ff, $80,$ff
	db $40,$80, $00,$80, $7f,$80, $00,$80, $00,$80, $7f,$80, $7f,$80, $00,$80
	db $00,$3c, $02,$7e, $85,$7e, $0a,$7e, $14,$7e, $ab,$7e, $95,$7e, $2a,$7e
	db $02,$01, $00,$01, $ff,$01, $00,$01, $01,$01, $fe,$01, $ff,$01, $00,$01
	db $00,$ff, $80,$ff, $50,$ff, $a8,$ff, $50,$ff, $a8,$ff, $54,$ff, $a8,$ff
	db $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80
	db $ff,$00, $ff,$00, $ff,$00, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e
	db $ff,$01, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $fe,$01
	db $7f,$80, $ff,$80, $7f,$80, $ff,$80, $7f,$80, $ff,$80, $7f,$80, $ff,$80
	db $ff,$00, $ff,$00, $ff,$00, $aa,$7f, $d5,$7f, $aa,$7f, $d5,$7f, $aa,$7f
	db $f8,$07, $f8,$07, $f8,$07, $80,$ff, $00,$ff, $aa,$ff, $55,$ff, $aa,$ff
	db $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $ff,$80, $7f,$80, $ff,$80
	db $d5,$7f, $aa,$7f, $d5,$7f, $aa,$7f, $d5,$7f, $aa,$7f, $d5,$7f, $aa,$7f
	db $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $eb,$3c
	db $54,$ff, $aa,$ff, $54,$ff, $aa,$ff, $54,$ff, $aa,$ff, $54,$ff, $aa,$ff
	db $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $00,$ff
	db $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $2a,$ff
	db $ff,$01, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $80,$ff
	db $7f,$80, $ff,$80, $7f,$80, $ff,$80, $7f,$80, $ff,$80, $7f,$80, $aa,$ff
	db $ff,$00, $ff,$00, $ff,$00, $ff,$00, $ff,$00, $ff,$00, $ff,$00, $2a,$ff
	db $ff,$01, $fe,$01, $ff,$01, $fe,$01, $fe,$01, $fe,$01, $fe,$01, $80,$ff
	db $7f,$80, $ff,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $00,$ff
	db $fe,$01, $fe,$01, $fe,$01, $fe,$01, $fe,$01, $fe,$01, $fe,$01, $80,$ff
	db $3f,$c0, $3f,$c0, $3f,$c0, $1f,$e0, $1f,$e0, $0f,$f0, $03,$fc, $00,$ff
	db $fd,$03, $fc,$03, $fd,$03, $f8,$07, $f9,$07, $f0,$0f, $c1,$3f, $82,$ff
	db $55,$ff, $2a,$7e, $54,$7e, $2a,$7e, $54,$7e, $2a,$7e, $54,$7e, $00,$7e
	db $01,$ff, $00,$01, $01,$01, $00,$01, $01,$01, $00,$01, $01,$01, $00,$01
	db $54,$ff, $ae,$f8, $50,$f0, $a0,$e0, $60,$c0, $80,$c0, $40,$80, $40,$80
	db $55,$ff, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00
	db $55,$ff, $6a,$1f, $05,$0f, $02,$07, $05,$07, $02,$03, $03,$01, $02,$01
	db $54,$ff, $80,$80, $00,$80, $80,$80, $00,$80, $80,$80, $00,$80, $00,$80
	db $55,$ff, $2a,$1f, $0d,$07, $06,$03, $01,$03, $02,$01, $01,$01, $00,$01
	db $55,$ff, $2a,$7f, $55,$7f, $2a,$7f, $55,$7f, $2a,$7f, $55,$7f, $00,$7f
	db $55,$ff, $aa,$ff, $55,$ff, $aa,$ff, $55,$ff, $aa,$ff, $55,$ff, $00,$ff
	db $15,$ff, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00
	db $55,$ff, $6a,$1f, $0d,$07, $06,$03, $01,$03, $02,$01, $03,$01, $00,$01
	db $54,$ff, $a8,$ff, $54,$ff, $a8,$ff, $50,$ff, $a0,$ff, $40,$ff, $00,$ff
	db $00,$7e, $2a,$7e, $d5,$7e, $2a,$7e, $54,$7e, $ab,$76, $dd,$66, $22,$66
	db $00,$7c, $2a,$7e, $d5,$7e, $2a,$7e, $54,$7c, $ff,$00, $ff,$00, $00,$00
	db $00,$01, $00,$01, $ff,$01, $02,$01, $07,$01, $fe,$03, $fd,$07, $0a,$0f
	db $00,$7c, $2a,$7e, $d5,$7e, $2a,$7e, $54,$7e, $ab,$7e, $d5,$7e, $2a,$7e
	db $00,$ff, $a0,$ff, $50,$ff, $a8,$ff, $54,$ff, $a8,$ff, $54,$ff, $aa,$ff
	db $dd,$62, $bf,$42, $fd,$42, $bf,$40, $ff,$00, $ff,$00, $f7,$08, $ef,$18
	db $ff,$00, $ff,$00, $ff,$00, $ab,$7c, $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e
	db $f9,$07, $fc,$03, $fd,$03, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $fe,$01
	db $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7c
	db $f7,$18, $eb,$1c, $d7,$3c, $eb,$3c, $d5,$3e, $ab,$7e, $d5,$7e, $2a,$ff
	db $ff,$01, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $a2,$ff
	db $7f,$c0, $bf,$c0, $7f,$c0, $bf,$e0, $5f,$e0, $af,$f0, $57,$fc, $aa,$ff
	db $ff,$01, $fc,$03, $fd,$03, $fc,$03, $f9,$07, $f0,$0f, $c1,$3f, $82,$ff
	db $55,$ff, $2a,$ff, $55,$ff, $2a,$ff, $55,$ff, $2a,$ff, $55,$ff, $00,$ff
	db $45,$ff, $a2,$ff, $41,$ff, $82,$ff, $41,$ff, $80,$ff, $01,$ff, $00,$ff
	db $54,$ff, $aa,$ff, $54,$ff, $aa,$ff, $54,$ff, $aa,$ff, $54,$ff, $00,$ff
	db $15,$ff, $2a,$ff, $15,$ff, $0a,$ff, $15,$ff, $0a,$ff, $01,$ff, $00,$ff
	db $01,$ff, $80,$ff, $01,$ff, $80,$ff, $01,$ff, $80,$ff, $01,$ff, $00,$ff
TilesEnd:

Smiley:
	DB $3C,$3C,$42,$7E,$A5,$DB,$A5,$DB
	DB $81,$FF,$A5,$DB,$5A,$66,$3C,$3C
SmileyEnd:

SECTION "Tilemap", ROM0

Tilemap:
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $01, $02, $03, $01, $04, $03, $01, $05, $00, $01, $05, $00, $06, $04, $07, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $08, $09, $0a, $0b, $0c, $0d, $0b, $0e, $0f, $08, $0e, $0f, $10, $11, $12, $13, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $14, $15, $16, $17, $18, $19, $1a, $1b, $0f, $14, $1b, $0f, $14, $1c, $16, $1d, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $1e, $1f, $20, $21, $22, $23, $24, $22, $25, $1e, $22, $25, $26, $22, $27, $1d, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $01, $28, $29, $2a, $2b, $2c, $2d, $2b, $2e, $2d, $2f, $30, $2d, $31, $32, $33, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $08, $34, $0a, $0b, $11, $0a, $0b, $35, $36, $0b, $0e, $0f, $08, $37, $0a, $38, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $14, $39, $16, $17, $1c, $16, $17, $3a, $3b, $17, $1b, $0f, $14, $3c, $16, $1d, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $1e, $3d, $3e, $3f, $22, $27, $21, $1f, $20, $21, $22, $25, $1e, $22, $40, $1d, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $41, $42, $43, $44, $30, $33, $41, $45, $43, $41, $30, $43, $41, $30, $33, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
TilemapEnd:

SECTION "Variables", wram0[$C000]
position:
	ds 1
velocity_y:
	ds 1
stepping:
	ds 1
fall_buffer:
	ds 1
delayval:
	ds 2
timer_reset:
	ds 1
grounded:
  ds 1

SECTION "OAM Buffer", WRAM0[$C100]
oam_buffer:
	ds 4 * 40
	
