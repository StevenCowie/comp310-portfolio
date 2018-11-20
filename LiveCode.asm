    .inesprg 1
    .ineschr 1
    .inesmap 0
    .inesmir 1

; ---------------------------------------------------------------------------

PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
OAMADDR   = $2003
OAMDATA   = $2004
PPUSCROLL = $2005
PPUADDR   = $2006
PPUDATA   = $2007
OAMDMA    = $4014
JOY1      = $4016
JOY2      = $4017

BUTTON_A      = %10000000
BUTTON_B      = %01000000
BUTTON_SELECT = %00100000
BUTTON_START  = %00010000
BUTTON_UP     = %00001000
BUTTON_DOWN   = %00000100
BUTTON_LEFT   = %00000010
BUTTON_RIGHT  = %00000001

    .rsset $0000
joypad1_state          .rs 1
bullet_active          .rs 1
nametable_address      .rs 2
scroll_x               .rs 1
scroll_page            .rs 1
player_speed           .rs 2    ; In subpixels per frame -- 16 bits
player_position_sub    .rs 1    ; in subpixels
temp_x                 .rs 1
temp_y                 .rs 1

ENEMY_SQUAD_WIDTH      = 6
ENEMY_SQUAD_HEIGHT     = 4
NUM_ENEMIES            = ENEMY_SQUAD_WIDTH * ENEMY_SQUAD_HEIGHT
ENEMY_SPACING          = 16

    .rsset $0200
sprite_player      .rs 4
sprite_bullet      .rs 4
sprite_chainsaw    .rs 4
sprite_enemy_0     .rs 4

    .rsset $0000
SPRITE_Y           .rs 1
SPRITE_TILE        .rs 1
SPRITE_ATTRIB      .rs 1
SPRITE_X           .rs 1



GRAVITY            = 10               ; In subpixels per frame^2
JUMP               = -(2 * 256)   ; in subpixels / frame
SCREEN_BOTTOM_Y    = 240 - 20  

    .bank 0
    .org $C000

; Initialisation code based on https://wiki.nesdev.com/w/index.php/Init_code
RESET:
    SEI        ; ignore IRQs
    CLD        ; disable decimal mode
    LDX #$40
    STX $4017  ; disable APU frame IRQ
    LDX #$ff
    TXS        ; Set up stack
    INX        ; now X = 0
    STX PPUCTRL  ; disable NMI
    STX PPUMASK  ; disable rendering
    STX $4010  ; disable DMC IRQs

    ; Optional (omitted):
    ; Set up mapper and jmp to further init code here.

    ; If the user presses Reset during vblank, the PPU may reset
    ; with the vblank flag still true.  This has about a 1 in 13
    ; chance of happening on NTSC or 2 in 9 on PAL.  Clear the
    ; flag now so the vblankwait1 loop sees an actual vblank.
    BIT PPUSTATUS

    ; First of two waits for vertical blank to make sure that the
    ; PPU has stabilized
vblankwait1:  
    BIT PPUSTATUS
    BPL vblankwait1

    ; We now have about 30,000 cycles to burn before the PPU stabilizes.
    ; One thing we can do with this time is put RAM in a known state.
    ; Here we fill it with $00, which matches what (say) a C compiler
    ; expects for BSS.  Conveniently, X is still 0.
    TXA
clrmem:
    LDA #0
    STA $000,x
    STA $100,x
    STA $300,x
    STA $400,x
    STA $500,x
    STA $600,x
    STA $700,x  ; Remove this if you're storing reset-persistent data

    ; We skipped $200,x on purpose.  Usually, RAM page 2 is used for the
    ; display list to be copied to OAM.  OAM needs to be initialized to
    ; $EF-$FF, not 0, or you'll get a bunch of garbage sprites at (0, 0).

    LDA #$FF
    STA $200,x

    INX
    BNE clrmem

    ; Other things you can do between vblank waits are set up audio
    ; or set up other mapper registers.
   
vblankwait2:
    BIT PPUSTATUS
    BPL vblankwait2

    ; End of initialisation code

    JSR InitialiseGame

    LDA #%10000000 ; Enable NMI
    STA PPUCTRL

    LDA #%00011000 ; Enable sprites and background
    STA PPUMASK

    LDA #0
    STA PPUSCROLL  ; Set x scroll
    STA PPUSCROLL  ; Set y scroll

    ; Enter an infinite loop
forever:
    JMP forever

; ------------------------------------------------------

InitialiseGame: ; Begin subroutine

    ; Reset the PPU high/low latch
    LDA PPUSTATUS

    ; Write address $3F00 (background palette) to the PPU
    LDA #$3F
    STA PPUADDR
    LDA #$00
    STA PPUADDR

    ; Write the background palette
    LDA #$31
    STA PPUDATA
    LDA #$2D
    STA PPUDATA
    LDA #$3C
    STA PPUDATA
    LDA #$3D
    STA PPUDATA
    LDA #$31
    STA PPUDATA
    LDA #$09
    STA PPUDATA
    LDA #$19
    STA PPUDATA
    LDA #$29
    STA PPUDATA

    ; Write address $3F10 (sprite palette) to the PPU
    LDA #$3F
    STA PPUADDR
    LDA #$10
    STA PPUADDR

    ; Write the background colour
    LDA #$0C
    STA PPUDATA

    ; Write the palette colours
    LDA #$0C
    STA PPUDATA
    LDA #$1C
    STA PPUDATA
    LDA #$2C
    STA PPUDATA

    ; Write sprite data for sprite 0
    LDA #120    ; Y pos
    STA sprite_player + SPRITE_Y
    LDA #0      ; Tile No.
    STA sprite_player + SPRITE_TILE
    LDA #0   ; Attributes (different palettes?)
    STA sprite_player + SPRITE_ATTRIB
    LDA #128    ; X pos
    STA sprite_player + SPRITE_X

    ; Enemies
    LDX #0
    LDA ENEMY_SQUAD_HEIGHT * ENEMY_SPACING
    STA temp_y
    LDA ENEMY_SQUAD_WIDTH * ENEMY_SPACING
    STA temp_x
    STA sprite_enemy_0 + SPRITE_X, x
    LDA temp_y
    STA sprite_enemy_0+ SPRITE_Y, x
    LDA #1
    STA sprite_enemy_0+SPRITE_TILE, x
    LDA #0
    STA sprite_enemy_0+SPRITE_ATTRIB, x

    ; Load nametable data 
    LDA #$20        ; Write address $2000 to PPUADDR register
    STA PPUADDR
    LDA #$00
    STA PPUADDR

    LDA #LOW(NametableData)
    STA nametable_address
    LDA #HIGH(NametableData)
    STA nametable_address+1
LoadNametable_OuterLoop:
    LDY #0
LoadNametable_InnerLoop:
    LDA [nametable_address], Y
    BEQ LoadNametable_End
    STA PPUDATA
    INY
    BNE LoadNametable_InnerLoop
    INC nametable_address+1
    JMP LoadNametable_OuterLoop
LoadNametable_End:

    ; Load attribute data
    LDA #$23        ; Write address $23C0 to PPUADDR register
    STA PPUADDR
    LDA #$C0
    STA PPUADDR

    LDA #%00000000
    LDX #64
LoadAttributes_Loop:
    STA PPUDATA
    DEX
    BNE LoadAttributes_Loop
    
    ; Load nametable data 
    LDA #$24        ; Write address $2000 to PPUADDR register
    STA PPUADDR
    LDA #$00
    STA PPUADDR

    LDA #LOW(NametableData)
    STA nametable_address
    LDA #HIGH(NametableData)
    STA nametable_address+1
LoadNametable2_OuterLoop:
    LDY #0
LoadNametable2_InnerLoop:
    LDA [nametable_address], Y
    BEQ LoadNametable2_End
    STA PPUDATA
    INY
    BNE LoadNametable2_InnerLoop
    INC nametable_address+1
    JMP LoadNametable2_OuterLoop
LoadNametable2_End:

    ; Load attribute data
    LDA #$27        ; Write address $23C0 to PPUADDR register
    STA PPUADDR
    LDA #$C0
    STA PPUADDR

    LDA #%01010101
    LDX #64
LoadAttributes2_Loop:
    STA PPUDATA
    DEX
    BNE LoadAttributes2_Loop
    


    RTS ; End subroutine
; ----------------------------------------------------------------------------

; NMI is called on every frame
NMI:
    ; Initialise controller 1
    LDA #1
    STA JOY1
    LDA #0
    STA JOY1

    ;Read joypad state
    LDX #0
    STX joypad1_state
ReadController:
    LDA JOY1
    LSR A
    ROL joypad1_state
    INX 
    CPX #8
    BNE ReadController


    ; React to Right button
    LDA joypad1_state
    AND #BUTTON_RIGHT
    BEQ ReadRight_Done ; if ((JOY1 & 1)) != 0 {
    LDA sprite_player + SPRITE_X
    CLC 
    ADC #1
    STA sprite_player + SPRITE_X
    LDA sprite_chainsaw + SPRITE_X
    CLC 
    ADC #1
    STA sprite_chainsaw + SPRITE_X
                ; }
ReadRight_Done:

;      ; Read Down button
;     LDA joypad1_state
;     AND #BUTTON_DOWN
;     BEQ ReadDown_Done ; if ((JOY1 & 1)) != 0 {
;     LDA sprite_player + SPRITE_Y
;     CLC 
;     ADC #1
;     STA sprite_player + SPRITE_Y
;                 ; }
; ReadDown_Done:

    ; React to Left button
    LDA joypad1_state
    AND #BUTTON_LEFT
    BEQ ReadLeft_Done ; if ((JOY1 & 1)) != 0 {
    LDA sprite_player + SPRITE_X
    SEC 
    SBC #1
    STA sprite_player + SPRITE_X
    LDA sprite_chainsaw + SPRITE_X
    SEC 
    SBC #1
    STA sprite_chainsaw + SPRITE_X
                ; }
ReadLeft_Done:

     ; Read Up button
    LDA joypad1_state
    AND #BUTTON_UP
    BEQ ReadUp_Done ; if ((JOY1 & 1)) != 0 {
    ; Set player speed
    LDA #LOW(JUMP)
    STA player_speed
    LDA #HIGH(JUMP)
    STA player_speed+1
    LDA sprite_chainsaw + SPRITE_Y
    SEC 
    SBC #1
    STA sprite_chainsaw + SPRITE_Y
ReadUp_Done:

    ; React to A button
    LDA joypad1_state
    AND #BUTTON_A
    BEQ ReadA_Done 
    ; Spawn a bullet if one isn't active
    LDA bullet_active
    BNE ReadA_Done
    ; No bullet active, so make one
    LDA #1
    STA bullet_active
    LDA sprite_player + SPRITE_Y    ; Y pos
    STA sprite_bullet + SPRITE_Y
    LDA #2      ; Tile No.
    STA sprite_bullet + SPRITE_TILE
    LDA #0   ; Attributes
    STA sprite_bullet + SPRITE_ATTRIB
    LDA sprite_player + SPRITE_X    ; X pos
    STA sprite_bullet + SPRITE_X
ReadA_Done:

    ; React to B button
    LDA joypad1_state
    AND #BUTTON_B
    BEQ ReadB_Done 
    ; Spawn a chainsaw
    LDA sprite_player + SPRITE_Y ; y pos
    STA sprite_chainsaw + SPRITE_Y
    LDA #4      ; Tile number
    STA sprite_chainsaw + SPRITE_TILE
    LDA #0
    STA sprite_chainsaw + SPRITE_ATTRIB
    LDA sprite_player + SPRITE_X ; x pos
    CLC
    ADC #8
    STA sprite_chainsaw + SPRITE_X
ReadB_Done:

    ; Update the bullet
    LDA bullet_active
    BEQ UpdateBullet_Done
    LDA sprite_bullet + SPRITE_X
    CLC
    ADC #1
    STA sprite_bullet + SPRITE_X
    BCC UpdateBullet_Done
    ; If carry flag is clear, bullet has left right of screen -- destroy
    LDA #0
    STA bullet_active
UpdateBullet_Done:



    ; Update player sprite (GRAVITY)
    ; First, update speed
    LDA player_speed    ; Low 8 bits
    CLC
    ADC #LOW(GRAVITY)
    STA player_speed
    LDA player_speed+1  ; High 8 bits
    ADC #HIGH(GRAVITY)  ; NB: *don't* clear the carry flag!
    STA player_speed+1

    ; Second, update position
    LDA player_position_sub ; Low 8 bits
    CLC
    ADC player_speed
    STA player_position_sub
    LDA sprite_player+SPRITE_Y ; High 8 bits
    ADC player_speed+1         ; NB: *don't* clear the carry flag!
    STA sprite_player+SPRITE_Y

    ; Check for top or bottom of screen
    CMP #SCREEN_BOTTOM_Y ; Accumulator already contains player y position
    BCC UpdatePlayer_NoClamp
    ; Check sign of speed
    LDA player_speed+1
    BMI UpdatePlayer_ClampToTop
    LDA #SCREEN_BOTTOM_Y-1      ; Clamp to bottom
    JMP UpdatePlayer_DoClamping
UpdatePlayer_ClampToTop:
    LDA #0                      ; Clamp to top
UpdatePlayer_DoClamping:        
    STA sprite_player+SPRITE_Y
    LDA #0                  ; Set player speed to zero
    STA player_speed        ; (both bytes)
    STA player_speed+1
UpdatePlayer_NoClamp:



    ; Scroll
;     LDA scroll_x
;     CLC
;     ADC #1
;     STA scroll_x
;     STA PPUSCROLL
;     BCC Scroll_NoWrap
;     ; scroll_x has wrapped, so switch scroll_page
;     LDA scroll_page
;     EOR #1
;     STA scroll_page
;     ORA #%10000000
;     STA PPUCTRL
; Scroll_NoWrap:
;     LDA #0
;     STA PPUSCROLL


    ; copy sprite data to ppu
    LDA #0
    STA OAMADDR 
    LDA #$02
    STA OAMDMA

    RTI         ; Return from interrupt

; ---------------------------------------------------------------------------

NametableData:
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03  
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03  
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$10,$11,$12,$13,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$20,$21,$22,$23,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03  
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $05,$05,$05,$05,$05,$05,$30,$31,$32,$33,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$30,$31,$32,$33,$05,$05,$05,$05,$05,$05,$05,$05 
    .db $05,$05,$05,$05,$05,$05,$30,$31,$32,$33,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$30,$31,$32,$33,$05,$05,$05,$05,$05,$05,$05,$05
    .db $00 ; null terminator

; ---------------------------------------------------------------------------

    .bank 1
    .org $FFFA
    .dw NMI
    .dw RESET
    .dw 0

; ---------------------------------------------------------------------------

    .bank 2
    .org $0000
    ; TODO: add graphics
    .incbin "sprites.chr"