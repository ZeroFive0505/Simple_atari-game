    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

JetXPos         byte         ; player 0 x-position
JetYPos         byte         ; player 0 y-position
EnemyXpos       byte         ; player 1 x-position
EnemyYpos       byte         ; player 1 y-position
MissileXPos     byte         ; missile x-position
MissileYPos     byte         ; missile y-position
MissileSound    byte         ; missile sound life time
MissileLauch    byte         ; missile has been lauched
Score           byte         ; 2-digit score stored as BCD
Timer           byte         ; 2-digit timer stored as BCD
Temp            byte         ; auxiliary variable to store temp values
OnesDigitOffset word         ; lookup table offset for the score Ones digit
TensDigitOffset word         ; lookup table offset for the score Tens digit
JetSpritePtr    word         ; pointer to player0 sprite lookup table
JetColorPtr     word         ; pointer to player0 color lookup table
EnemySpritePtr  word         ; pointer to player1 sprite lookup table
EnemyColorPtr   word         ; pointer to player1 color lookup table
Enemy2SpritePtr word
Enemy2ColorPtr  word
Enemy3SpritePtr word
Enmey3ColorPtr  word
EnemyType       byte         ; enemy tyep value
EnemyHealth     byte         ; enemy health
EnemyDir        byte         ; enemy direction
JetAnimOffset   byte         ; player0 frame offset for sprite animation
Random          byte         ; used to generate random bomber x-position
RandomType      byte         ; for random type
JetWeapon       byte         ; check variable for weapon use
ScoreSprite     byte         ; store the sprite bit pattern for the score
TimerSprite     byte         ; store the sprite bit pattern for the timer
TerrainColor    byte         ; store the color of the terrain playfield
RiverColor      byte         ; store the color of the river playfield

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9               ; player0 sprite height (# rows in lookup table)
ENEMY_HEIGHT = 9            ; player1 sprite height (# rows in lookup table)
DIGITS_HEIGHT = 5            ; scoreboard digit height (# rows in lookup table)
MISSILE_TYPE = 0            ; enemy missile type and health 1
PLANE_TYPE1 = 1             ; enemy plane type 1 and health 2
PLANE_TYPE2 = 2             ; enemy plane type 2 and health 3
DIR_LEFT = 0
DIR_RIGHT = 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

Reset:
    CLEAN_START              ; call macro to reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #68
    sta JetXPos              ; JetXPos = 68
    lda #10
    sta JetYPos              ; JetYPos = 10
    lda #62
    sta EnemyXpos           
    lda #83
    sta EnemyYpos            
    lda #%11010100
    sta Random               ; Random = $D4
    lda #%00101011       
    sta RandomType
    lda #0
    sta Score                ; Score = 0
    sta Timer                ; Timer = 0
    lda #1
    sta JetWeapon
    lda #MISSILE_TYPE         ; first enemy is missile
    sta EnemyType
    lda #1
    sta EnemyHealth          ; enemy health
    lda #DIR_LEFT
    sta EnemyDir             ; setting the direction

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare a MACRO to ehck if we should display the missile 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        lda #%00000000
        cpx MissileYPos      ; compare X (currnet scanline) with missile Y position
        bne .SkipMissileDraw ; if (X != missile Y position), then skip draw
.DrawMissile:                ; else:
        lda #%00000010       ;   enable missile 0 display
        inc MissileYPos      ;   MissileYPos++
.SkipMissileDraw:
        sta ENAM0            ; store the correct value in the TIA missile register
    ENDM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to the correct lookup table adresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<JetSprite
    sta JetSpritePtr         ; lo-byte pointer for jet sprite lookup table
    lda #>JetSprite
    sta JetSpritePtr+1       ; hi-byte pointer for jet sprite lookup table

    lda #<JetColor
    sta JetColorPtr          ; lo-byte pointer for jet color lookup table
    lda #>JetColor
    sta JetColorPtr+1        ; hi-byte pointer for jet color lookup table

    lda #<EnemySprite
    sta EnemySpritePtr      ; lo-byte pointer for bomber sprite lookup table
    lda #>EnemySprite
    sta EnemySpritePtr+1    ; hi-byte pointer for bomber sprite lookup table

    lda #<EnemyColor
    sta EnemyColorPtr       ; lo-byte pointer for bomber color lookup table
    lda #>EnemyColor
    sta EnemyColorPtr+1     ; hi-byte pointer for bomber color lookup table

    lda #<Enemy2Sprite
    sta Enemy2SpritePtr
    lda #>Enemy2Sprite
    sta Enemy2SpritePtr+1

    lda #<Enemy2Color
    sta Enemy2ColorPtr
    lda #>Enemy2Color
    sta Enemy2ColorPtr+1

    lda #<Enemy3Sprite
    sta Enemy3SpritePtr
    lda #>Enemy3Sprite
    sta Enemy3SpritePtr+1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK               ; turn on VBLANK
    sta VSYNC                ; turn on VSYNC


    ldx #3
VSYNC3Lines:
    sta WSYNC                ; display 3 recommended lines of VSYNC
    dex
    bne VSYNC3Lines
    lda #0
    sta VSYNC                ; turn off VSYNC


    ldx #32
VBLANK32Lines:
    sta WSYNC            ; display the recommended lines of VBLANK
    dex
    bne VBLANK32Lines

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JetXPos
    ldy #0
    jsr SetObjectXPos        ; set player0 horizontal position

    lda EnemyXpos
    ldy #1
    jsr SetObjectXPos        ; set player1 horizontal position

    lda MissileXPos
    ldy #2
    jsr SetObjectXPos        ; set missile horizontal position

    jsr CalculateDigitOffset ; calculate scoreboard digits lookup table offset

    jsr GenerateJetSound     ; configure and enable our jet engine audio
    jsr GenerateMissileSound         ; play the missile sound effect

    sta WSYNC
    sta HMOVE                ; apply the horizontal offsets previously set

    lda #0
    sta VBLANK               ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0                   ; reset TIA registers before displaying the score
    sta COLUBK
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF

    lda #$1E
    sta COLUPF               ; set the scoreboard playfield color with yellow

    ldx #DIGITS_HEIGHT       ; start X counter with 5 (height of digits)

.ScoreDigitLoop:
    ldy TensDigitOffset      ; get the tens digit offset for the Score
    lda Digits,Y             ; load the bit pattern from lookup table
    and #$F0                 ; mask/remove the graphics for the ones digit
    sta ScoreSprite          ; save the score tens digit pattern in a variable

    ldy OnesDigitOffset      ; get the ones digit offset for the Score
    lda Digits,Y             ; load the digit bit pattern from lookup table
    and #$0F                 ; mask/remove the graphics for the tens digit
    ora ScoreSprite          ; merge it with the saved tens digit sprite
    sta ScoreSprite          ; and save it
    sta WSYNC                ; wait for the end of scanline
    sta PF1                  ; update the playfield to display the Score sprite

    ldy TensDigitOffset+1    ; get the left digit offset for the Timer
    lda Digits,Y             ; load the digit pattern from lookup table
    and #$F0                 ; mask/remove the graphics for the ones digit
    sta TimerSprite          ; save the timer tens digit pattern in a variable

    ldy OnesDigitOffset+1    ; get the ones digit offset for the Timer
    lda Digits,Y             ; load digit pattern from the lookup table
    and #$0F                 ; mask/remove the graphics for the tens digit
    ora TimerSprite          ; merge with the saved tens digit graphics
    sta TimerSprite          ; and save it

    jsr Sleep12Cycles        ; wastes some cycles

    sta PF1                  ; update the playfield for Timer display

    ldy ScoreSprite          ; preload for the next scanline
    sta WSYNC                ; wait for next scanline

    sty PF1                  ; update playfield for the score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1    ; increment all digits for the next line of data

    jsr Sleep12Cycles        ; waste some cycles

    dex                      ; X--
    sta PF1                  ; update the playfield for the Timer display
    bne .ScoreDigitLoop      ; if dex != 0, then branch to ScoreDigitLoop

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the remaining visible scanlines of our main game (2-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    lda TerrainColor
    sta COLUPF               ; set the terrain background color

    lda RiverColor
    sta COLUBK               ; set the river background color

    lda #%00000001
    sta CTRLPF               ; enable playfield reflection
    lda #$F0
    sta PF0                  ; setting PF0 bit pattern
    lda #$FC
    sta PF1                  ; setting PF1 bit pattern
    lda #0
    sta PF2                  ; setting PF2 bit pattern

    ldx #85                  ; X counts the number of remaining scanlines
.GameLineLoop:
    DRAW_MISSILE             ; macro to check if we should draw the missile

.AreWeInsideJetSprite:
    txa                      ; transfer X to A
    sec                      ; make sure carry flag is set before subtraction
    sbc JetYPos              ; subtract sprite Y-coordinate
    cmp #JET_HEIGHT          ; are we inside the sprite height bounds?
    bcc .DrawSpriteP0        ; if result < SpriteHeight, call the draw routine
    lda #0                   ; else, set lookup index to zero
.DrawSpriteP0:
    clc                      ; clear carry flag before addition
    adc JetAnimOffset        ; jump to correct sprite frame address in memory
    tay                      ; load Y so we can work with the pointer
    lda (JetSpritePtr),Y     ; load player0 bitmap data from lookup table
    sta WSYNC                ; wait for scanline
    sta GRP0                 ; set graphics for player0
    lda (JetColorPtr),Y      ; load player color from lookup table
    sta COLUP0               ; set color of player 0

.AreWeInsideEnemySprite:
    txa                      ; transfer X to A
    sec                      ; make sure carry flag is set before subtraction
    sbc EnemyYpos            ; subtract sprite Y-coordinate
    cmp #ENEMY_HEIGHT        ; are we inside the sprite height bounds?
    bcc .DrawSpriteP1        ; if result < SpriteHeight, call the draw routine
    lda #0                   ; else, set lookup index to zero
.DrawSpriteP1:
    tay                      ; load Y so we can work with the pointer

    lda #%00000101
    sta NUSIZ1               ; stretch player 1 sprite


    lda EnemyType
    cmp #MISSILE_TYPE
    beq .EnemyTypeMissile
    cmp #PLANE_TYPE1
    beq .EnemyTypePlane1
    cmp #PLANE_TYPE2
    beq .EnemyTypePlane2

.EnemyTypeMissile:
    lda (EnemySpritePtr),Y   ; load player1 bitmap data from lookup table
    sta WSYNC                ; wait for scanline
    sta GRP1                 ; set graphics for player1
    lda (EnemyColorPtr),Y    ; load player color from lookup table
    sta COLUP1               ; set color of player 1
    jmp .EndDrawEnemy
.EnemyTypePlane1:
    lda (Enemy2SpritePtr),Y
    sta WSYNC
    sta GRP1
    lda (Enemy2ColorPtr),Y
    sta COLUP1
    jmp .EndDrawEnemy
.EnemyTypePlane2:
    lda (Enemy3SpritePtr),Y
    sta WSYNC
    sta GRP1
    lda (Enmey3ColorPtr),Y
    sta COLUP1


.EndDrawEnemy:
    dex                      ; X--
    bne .GameLineLoop        ; repeat next main game scanline until finished

    lda #0
    sta JetAnimOffset        ; reset jet animation frame to zero each frame

    sta WSYNC                ; wait for a scanline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK               ; turn on VBLANK again

    ldx #30
Overscan:
    sta WSYNC                ; display recommended lines of VBlank Overscan
    dex
    bne Overscan
   
    lda #0
    sta VBLANK               ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player 0 movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000           ; bit pattern to check if P0 joystick is up
    bit SWCHA                ; if (bit pattern is not joystick up)
    bne CheckP0Down          ;     then: skip to test next input option
    lda JetYPos
    cmp #60
    beq CheckP0Down        ; if player y-position is equal to 60 then stop moving it up
.P0IsUp:                     ;     else:
    inc JetYPos              ;         increment jet Y position
    lda #0
    sta JetAnimOffset        ;         set jet frame of animation to first one

CheckP0Down:
    lda #%00100000           ; bit pattern to check if P0 joystick is down
    bit SWCHA                ; if (bit pattern is not joystick down)
    bne CheckP0Left          ;     then: skip to test next input option
    lda JetYPos
    cmp #1
    beq CheckP0Left          ; in player y-position is equal to 1 then stop moving it down
.P0IsDown:                   ;     else:
    dec JetYPos              ;         decrement jet Y position
    lda #0
    sta JetAnimOffset        ;         set jet frame of animation to first one

CheckP0Left:
    lda #%01000000           ; bit pattern to check if P0 joystick is left
    bit SWCHA                ; if (bit pattern is not joystick left)
    bne CheckP0Right         ;     then: skip to test next input option
    lda JetXPos
    cmp #31
    beq CheckP0Right      ; if player x-position is equal to 31 then stop moving to left
.P0IsLeft:                   ;     else:
    dec JetXPos              ;         decrement jet X position
    lda #JET_HEIGHT
    sta JetAnimOffset        ;         set jet to second frame of animation

CheckP0Right:
    lda #%10000000           ; bit pattern to check if P0 joystick is right
    bit SWCHA                ; if (bit pattern is not joystick right)
    bne CheckButtonPressed        ;     then: skip to test next input option
    lda JetXPos
    cmp #102
    beq CheckButtonPressed        ; if player x-position is equal to 80 then stop moving to right
.P0IsRight:                  ;     else:
    inc JetXPos              ;         increment jet X position
    lda #JET_HEIGHT
    sta JetAnimOffset        ;         set jet to second frame of animation

CheckButtonPressed:
    lda #%10000000           ; if button is pressed
    bit INPT4
    bne EndInputCheck
    lda JetWeapon
    cmp #0
    beq EndInputCheck
    jmp .ButtonPressed

.ButtonPressed:
    lda JetXPos
    clc
    adc #4
    sta MissileXPos          ; set the missile X position equal to the player 0
    lda JetYPos
    clc
    adc #8
    sta MissileYPos          ; set the missile Y position equal to the player 0
    lda #0
    sta MissileSound
    lda #1
    sta MissileLauch
    lda #0
    sta JetWeapon

EndInputCheck:               ; fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateEnemyPosition:
    lda EnemyYpos
    clc
    cmp #0                   ; compare enemy y-position with 0
    bmi .ResetEnemyPosition ; if it is < 0, then reset y-position to the top
    dec EnemyYpos            ; else, decrement enemy y-position for next frame

    lda EnemyType
    cmp #PLANE_TYPE1
    beq .LRMove
    jmp EndPositionUpdate
.LRMove:
    lda EnemyDir
    cmp #DIR_LEFT
    beq .LEFTMove
    cmp #DIR_RIGHT
    beq .RIGHTMove
.LEFTMove:
    lda EnemyXpos
    cmp #31
    bmi .MoveToRight
    dec EnemyXpos
    jmp EndPositionUpdate
.RIGHTMove:
    lda EnemyXpos
    cmp #102
    bpl .MoveToLeft
    inc EnemyXpos
    jmp EndPositionUpdate
.MoveToRight:
    lda #DIR_RIGHT
    sta EnemyDir
    jmp EndPositionUpdate
.MoveToLeft:
    lda #DIR_LEFT
    sta EnemyDir
    jmp EndPositionUpdate

.ResetEnemyPosition:
    jsr GetRandomEnemyPos   ; call subroutine for random enemy position
    
.SetScoreValues:
    sed                      ; set BCD mode for score and timer values

    lda Timer
    clc
    adc #1
    sta Timer                ; add 1 to the Timer (BCD does not like INC)

    cld                      ; disable BCD mode adfter updating Score and Timer

EndPositionUpdate:           ; fallback for the position update code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    lda #%10000000           ; CXPPMM bit 7 detects P0 and P1 collision
    bit CXPPMM               ; check CXPPMM bit 7 with the above pattern
    bne .P0P1Collided        ; if collision between P0 and P1 happened, branch
    jsr SetTerrainRiverColor ; else, set playfield color to green/blue
    jmp CheckCollisionM0P1   ; check the next possible collision
.P0P1Collided:
    jsr GameOver             ; call GameOver subroutine

CheckCollisionM0P1:
    lda #%10000000           ; CXM0P bit 7 detects M0 and P1 collision 
    bit CXM0P                ; check CXM0P bit 7 with the above pattern
    bne .M0P1Collided        ; collision missile 0 and player 1 happened
    jmp EndCollisionCheck
.M0P1Collided
    sed
    lda Score
    clc
    adc #1
    sta Score                ; adds 1 to the Score using decimal mode
    cld
    lda #0
    sta MissileYPos          ; reset the missile position
    lda #1
    sta JetWeapon
    lda EnemyHealth
    sec
    sbc #1
    cmp #0
    beq .RespawnEnemy
    sta EnemyHealth
    jmp EndCollisionCheck
.RespawnEnemy:
    jsr GetRandomEnemyPos


EndCollisionCheck:           ; fallback
    sta CXCLR                ; clear all collision flags before the next frame


    lda MissileYPos
    cmp #85
    bpl ResetMissile
    jmp EndFrame
ResetMissile:
    ldx #1
    stx JetWeapon

EndFrame:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame           ; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate the audio for the jet engine sound based on the jet y position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GenerateJetSound subroutine
    lda #3
    sta AUDV0                ; set the new audio volume

    lda JetYPos              ; loads the accumulator with the jet y-position
    lsr
    lsr
    lsr                      ; divide the accumulator by 8 (using right-shifts)
    sta Temp                 ; save the Y/8 value in the temp variable
    lda #31
    sec
    sbc Temp                 ; subtract 31 - (Y/8)
    sta AUDF0                ; set the new audio frequency/pitch register

    lda #8
    sta AUDC0                ; set the new audio tone type register

    rts

GenerateMissileSound subroutine
    lda MissileLauch
    cmp #1
    beq .PlayMissileSound
    jmp .StopSound

.PlayMissileSound
    lda #10
    sta AUDV1

    lda #20
    sta AUDF1

    lda #7
    sta AUDC1

    inc MissileSound
    lda MissileSound

    cmp #10
    beq .StopSound
    jmp .KeepPlay

.StopSound:
    lda #0
    sta AUDV1
    sta MissileLauch
.KeepPlay:
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the colors for the terrain and river to green & blue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetTerrainRiverColor subroutine
    lda #$C2
    sta TerrainColor         ; set terrain color to green
    lda #$84
    sta RiverColor           ; set river color to blue
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of our object
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC                ; start a fresh new scanline
    sec                      ; make sure carry-flag is set before subtracion
.Div15Loop
    sbc #15                  ; subtract 15 from accumulator
    bcs .Div15Loop           ; loop until carry-flag is clear
    eor #7                   ; handle offset range from -8 to 7
    asl
    asl
    asl
    asl                      ; four shift lefts to get only the top 4 bits
    sta HMP0,Y               ; store the fine offset to the correct HMxx
    sta RESP0,Y              ; fix object position in 15-step increment
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine

    lda #68
    sta JetXPos              ; JetXPos = 68
    lda #10
    sta JetYPos              ; JetYPos = 10

    jsr GetRandomEnemyPos

    lda #0
    sta Score                ; Score = 0
    sta Timer
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate a Linear-Feedback Shift Register random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a LFSR random number for the X-position of the bomber.
;; Divide the random value by 4 to limit the size of the result to match river.
;; Add 30 to compensate for the left green playfield
;; The routine also sets the Y-position of the bomber to the top of the screen.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomEnemyPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random              ; performs a series of shifts and bit operations
    lsr
    lsr                     ; divide the value by 4 with 2 right shifts
    sta EnemyXpos           ; save it to the variable Enemy x-position
    lda #30
    adc EnemyXpos           ; adds 30 + Enemy x-position to compensate for left PF
    sta EnemyXpos           ; and sets the new value to the bomber x-position

    lda #96
    sta EnemyYpos           ; set the y-position to the top of the screen


    lda RandomType
    asl
    eor RandomType
    asl
    eor RandomType
    asl
    asl
    eor RandomType
    asl
    rol RandomType
    lsr
    lsr
.Div3Loop:
    cmp #3
    bmi .EndDiv3Loop
    sbc #3
    cmp #3
    bpl .Div3Loop

.EndDiv3Loop:
    sta EnemyType
    cmp #MISSILE_TYPE
    beq .Set1Health
    cmp #PLANE_TYPE1
    beq .Set1Health
    cmp #PLANE_TYPE2
    beq .Set2Health

.Set1Health:
    lda #1
    sta EnemyHealth
    jmp .EndHealthSet
.Set2Health
    lda #2
    sta EnemyHealth
.EndHealthSet:
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The scoreboard is stored using BCD, so the display shows hex numbers.
;; This converts the high and low nibbles of the variable Score and Timer
;; into the offsets of digits lookup table so the values can be displayed.
;; Each digit has a height of 5 bytes in the lookup table.
;;
;; For the low nibble we need to multiply by 5
;;   - we can use left shifts to perform multiplication by 2
;;   - for any number N, the value of N*5 = (N*2*2)+N
;;
;; For the upper nibble, since its already times 16, we need to divide it
;; and then multiply by 5:
;;   - we can use right shifts to perform division by 2
;;   - for any number N, the value of (N/16)*5 is equal to (N/4)+(N/16)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    ldx #1                   ; X register is the loop counter
.PrepareScoreLoop            ; this will loop twice, first X=1, and then X=0

    lda Score,X              ; load A with Timer (X=1) or Score (X=0)
    and #$0F                 ; remove the tens digit by masking 4 bits 00001111
    sta Temp                 ; save the value of A into Temp
    asl                      ; shift left (it is now N*2)
    asl                      ; shift left (it is now N*4)
    adc Temp                 ; add the value saved in Temp (+N)
    sta OnesDigitOffset,X    ; save A in OnesDigitOffset+1 or OnesDigitOffset

    lda Score,X              ; load A with Timer (X=1) or Score (X=0)
    and #$F0                 ; remove the ones digit by masking 4 bits 11110000
    lsr                      ; shift right (it is now N/2)
    lsr                      ; shift right (it is now N/4)
    sta Temp                 ; save the value of A into Temp
    lsr                      ; shift right (it is now N/8)
    lsr                      ; shift right (it is now N/16)
    adc Temp                 ; add the value saved in Temp (N/16+N/4)
    sta TensDigitOffset,X    ; store A in TensDigitOffset+1 or TensDigitOffset

    dex                      ; X--
    bpl .PrepareScoreLoop    ; while X >= 0, loop to pass a second time

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

JetSprite:
    .byte #%00000000
    .byte #%00101000;$1E
    .byte #%10101010;$0E
    .byte #%01111100;$06
    .byte #%00111000;$04
    .byte #%00111000;$0E
    .byte #%00010000;$0E
    .byte #%00010000;$0E
    .byte #%00010000;$0E

JetSpriteTurn:
    .byte #%00000000
    .byte #%00101000;$1E
    .byte #%01111100;$0E
    .byte #%00111000;$06
    .byte #%00010000;$04
    .byte #%00010000;$0E
    .byte #%00010000;$0E
    .byte #%00010000;$0E
    .byte #%00010000;$0E

EnemySprite:
    .byte #%00000000
    .byte #%00010000;$02
    .byte #%00010000;$0E
    .byte #%00010000;$0E
    .byte #%00111000;$0E
    .byte #%00010000;$0E
    .byte #%00010000;$0E
    .byte #%00111000;$0E
    .byte #%00010000;$1E
Enemy2Sprite:
    .byte #%00000000
    .byte #%00010000;$30
    .byte #%00010000;$00
    .byte #%01010100;$0E
    .byte #%11111110;$00
    .byte #%01010100;$00
    .byte #%00010000;$00
    .byte #%00010000;$00
    .byte #%00111000;$00
Enemy3Sprite:
    .byte #%000000000
    .byte #%00011000;$00
    .byte #%00111100;$80
    .byte #%00011000;$00
    .byte #%01011010;$80
    .byte #%01011010;$00
    .byte #%01111110;$00
    .byte #%01111110;$00
    .byte #%01000010;$00

JetColor:
    .byte #$00
    .byte #$1E;
    .byte #$0E;
    .byte #$06;
    .byte #$04;
    .byte #$0E;
    .byte #$0E;
    .byte #$0E;
    .byte #$0E;

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

EnemyColor:
    .byte #$00
    .byte #$02;
    .byte #$0E;
    .byte #$0E;
    .byte #$0E;
    .byte #$0E;
    .byte #$0E;
    .byte #$0E;
    .byte #$1E;
Enemy2Color:
    .byte #$00
    .byte #$30;
    .byte #$00;
    .byte #$0E;
    .byte #$00;
    .byte #$00;
    .byte #$00;
    .byte #$00;
    .byte #$00;
Enemy3Color:
    .byte #$00
    .byte #$00;
    .byte #$80;
    .byte #$00;
    .byte #$80;
    .byte #$00;
    .byte #$00;
    .byte #$00;
    .byte #$00;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                ; move to position $FFFC
    word Reset               ; write 2 bytes with the program reset address
    word Reset               ; write 2 bytes with the interruption vector
