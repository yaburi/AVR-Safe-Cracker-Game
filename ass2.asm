.include "m2560def.inc"

;==================================================
; DEFINES
;==================================================
.def temp = r16
.def temp2 = r17
.def temp3 = r18
.def debounce = r20
.def potTarget = r19
.def currKey = r21
.def row = r22
.def col = r23
.def rmask = r24
.def cmask = r25


;==================================================
; MACROS
;==================================================
.macro clear_one
	ldi YL, low(@0)
	ldi YH, high(@0)
	clr temp
	st Y, temp
.endmacro

.macro clear_two
    ldi YL, low(@0)     ; load the memory address to Y
    ldi YH, high(@0)
    clr temp 
    st Y+, temp         ; clear the two bytes at @0 in SRAM
    st Y, temp
.endmacro
.macro motor_on
	;push r18
	in r18, PORTE
	ori r18, 0b00100000
	out PORTE, r18
	;pop r18
.endmacro

.macro motor_off
	;push r18
	in r18, PORTE
	andi r18, 0b11011111
	out PORTE, r18
	;pop r18
.endmacro
.macro set_var
	;push temp
	;push YH
	;push YL
	ldi YH, high(@0)
	ldi YL, low(@0)
	ldi temp, @1
	st Y, temp
	;pop YL
	;pop YH
	;pop temp

.endmacro

.macro do_lcd_command
	;push temp
	ldi temp, @0
	rcall lcd_command
	rcall lcd_wait
	;pop temp
.endmacro

.macro do_lcd_data_a
		;push temp
		mov temp, @0
		call lcd_data
		call lcd_wait
		;pop temp
.endmacro

.macro do_lcd_data
	;push temp
	ldi temp, @0
	rcall lcd_data
	rcall lcd_wait
	;pop temp
.endmacro

.macro lcd_set
	sbi PORTA, @0
.endmacro

.macro lcd_clr
	cbi PORTA, @0
.endmacro

.macro adc_read
	;push temp
	ldi temp, (1<<REFS0)|(0<<ADLAR)|(0<<MUX0)
	sts ADMUX, temp
	ldi temp, (1<<MUX5)
	sts ADCSRB, temp
	ldi temp, (1<<ADEN)|(1<<ADSC)|(5<<ADPS0)|(1<<ADIE)
	sts ADCSRA, temp
	sei
	;pop temp
.endmacro

.macro set_code
	;push temp
	;push XH
	;push XL
	;push YH
	;push YL
	ldi XH, high(@0)
	ldi XL, low(@0)
	ld temp, X
	ldi YH, high(Code)
	ldi YL, low(Code) 
	st Y, temp
	;pop YL
	;pop YH
	;pop XL
	;pop XH
	;pop temp
.endmacro
.macro scan_keypad
/*
	NUMBERS:
	0..0
	1 = 1
	2 = 2
	3 = 3
	.....
	LETTERS:
	A ..10
	B ..11
	C ..12
	D ..13

	SYMBOLS
	*..14
	#..15
*/
	push temp
	push temp2
	push temp3
	push row
	push col
	push rmask
	push cmask

	codeLoop:
		
		cpi col, 4
		brne continue_scan
		set_var CDTime, 1
		clear_two TempCounter
		motor_off
		;jmp convert_end
		continue_scan:
		ldi cmask, INITCOLMASK	; initial column mask
		clr col					; initial column

	colloop:
		cpi col, 4		
		breq codeLoop				; if all keys are scanned, repeat
		
		sts PORTL, cmask		; otherwise scan a column (columns are PORTL [output])

		ldi temp, 0xFF			; for slowing down the scan operation

	delay:
		dec temp
		brne delay		; repeats delay until 0xFF becomes 0x00

		lds temp, PINL 			; Read PORTL (rows) (rows are PINL [input])
		andi temp, ROWMASK 	; Get the keypad output value 			; If button pressed isn't in this row, ROWMASK will still be 0xF
		cpi temp, 0xF 			; Check if any row is low				; Therefore we will go to next column since ROWMASK == 0xF
		breq nextcol 			; If yes, find which row is low

		ldi rmask, INITROWMASK 	; Initialize for row check
		clr row
		;ldi temp, 0xFF
	rowloop:
		cpi row, 4
		breq nextcol			; the row scan is over
		mov temp2, temp
		and temp2, rmask		; check un-masked bit
		breq convert			; if bit is clear, the key is pressed
		inc row					; else move to the next row
		lsl rmask
		jmp rowloop

	nextcol:
		lsl cmask
		inc col
		jmp colloop

	convert:
		cpi col, 3				; if key is in column 3
		breq letters			; they are letters so do nothing

		cpi row, 3				; if the key is in row 3
		breq symbols			; they are symbols, so do nothing

		mov temp, row			; these are the numbers 1-9
		lsl temp
		add temp, row
		add temp, col
		subi temp, -1
		jmp convert_end

	letters: ;col 3
		cpi row, 0
		breq A
		cpi row, 1
		breq B
		cpi row, 2
		breq C
		cpi row, 3
		breq D
		jmp convert_end
	; Symbols
	symbols: ; row 3
		cpi col, 0
		breq star
		cpi col, 1
		breq zero
		cpi col, 2
		breq hash
		jmp convert_end

	A:
		ldi temp, 10
		jmp convert_end

	B:
		ldi temp, 11
		jmp convert_end

	C:
		ldi temp, 12
		jmp convert_end

	D:
		ldi temp, 13
		jmp convert_end

	star:
		ldi temp, 14
		jmp convert_end

	zero:
		ldi temp, 0
		jmp convert_end

	hash:
		ldi temp, 15
		jmp convert_end

	convert_end:
		mov currKey, temp
		
		pop cmask
		pop rmask
		pop col
		pop row
		pop temp3
		pop temp2
		pop temp
		;andi temp, 0b00001111	; this clears the first four bits, and keeps the setted last 4 bits

		;out PORTC, currKey
		;jmp HandleKey
.endmacro

;==================================================
; DSEG
;==================================================
.dseg
Screen:				; Current screen
	.byte 1
CDOn:				; Toggle countdown
	.byte 1			
CDTime:				; Current countdown time
	.byte 1
PrintNow:			; Print flag (set off every second in Timer0)
	.byte 1
PotGood:			; Flag to determine if POT is in correct pos 
	.byte 1
PotCountdown:		; 500ms countdown of POT
	.byte 1
PotValue:
	.byte 1
PreviousKey:
	.byte 1
InputNumbers:
	.byte 1
Code:				; THe current code used in the round
	.byte 1
Code1:
	.byte 1
Code2:
	.byte 1
Code3:
	.byte 1
Round:
	.byte 1
NewRound:
	.byte 1
SecondCounter:		; Two-byte counter for counting seconds
	.byte 2

TempCounter:		; Used to determine if one second has passed
	.byte 2

MilliCounter:
	.byte 2	        ; Debounce counter. Used to determine 
                         ; if 50ms have passed
HalfCounter:
	.byte 2


;==================================================
; EQUs
;==================================================
; Screens
.equ STARTSCREEN	= 1
.equ CDSCREEN		= 2
.equ RESETPOTSCREEN	= 3
.equ FINDPOTSCREEN	= 4
.equ FINDCODESCREEN = 5
.equ ENTERCODESCREEN= 6
.equ COMPLETESCREEN = 7
.equ TIMEOUTSCREEN  = 8
; LCD
.equ LCD_RS = 7
.equ LCD_E 	= 6
.equ LCD_RW = 5
.equ LCD_BE = 4
; Keypad
.equ PORTLDIR = 0xF0    ; PF7-4: output, PF3-0, input
.equ INITCOLMASK = 0xEF ; scan from the rightmost column,
.equ INITROWMASK = 0x01 ; scan from the top row
.equ ROWMASK = 0x0F     ; for obtaining input from Port D

;==================================================
; INTERRUPT TABLE
;==================================================
.cseg
	jmp RESET
.org INT0addr
    jmp EXT_INT0
.org INT1addr
    jmp EXT_INT1
.org OVF2addr
	jmp Timer2OVF
.org OVF0addr
    jmp Timer0OVF        ; Jump to the interrupt handler for
                        ; Timer0 overflow.
.org ADCCaddr
	jmp ADCCOVF


;==================================================
; RESET
;==================================================
RESET:
	; Disables global interrupts
	cli
	
	; Initialise stack pointers
	ldi temp, low(RAMEND)
	out SPL, temp
	ldi temp, high(RAMEND)
	out SPH, temp

	; Clears registers
	clr temp
	clr temp2
	clr temp3
	clr debounce
	clr potTarget
	
	; Sets up dseg variables
	clear_one Screen
	clear_one CDOn
	clear_one CDTime
	clear_one PrintNow
	clear_one PotGood
	clear_one PotCountdown
	clear_one PotValue
	clear_one PreviousKey
	clear_one Round
	clear_one NewRound
	clear_one Code1
	clear_one Code2
	clear_one Code3
	clear_two SecondCounter
	clear_two TempCounter
	clear_two MilliCounter

	; Sets up ports
	ser temp
	out DDRC, temp ; LEDs
	out DDRF, temp ; LCD Display
	out DDRA, temp ; LCD Control
	ldi temp, 0b00000011
	out DDRG, temp // Port G: LED Bar, 0-1 (Top two)
	ldi temp, PORTLDIR  ; Keypad
	sts DDRL, temp
	//motor and lights
	ldi r16, 0b00111000
	out DDRE, r16 	

	clr temp
	out PORTF, r16 
	out PORTA, r16
	out PORTC, r16	
	out PORTG, r16
	out PORTE, r16	
	;out PORTB, r16
			
	out DDRD, temp	; Push Buttons

	; Sets up Push Buttons (MAY NEED TO CHANGE)
	ldi temp, (1<<ISC11) | (1<<ISC01)		; setting int1 and int0 for falling edge trigger, each pair contains "10"
	sts EICRA, temp							; store temp back into EICRA
	in temp, EIMSK							; store current state of EIMSK in temp
	ori temp, (1<<INT0) | (1<<INT1)			; enable INT0 and INT1 in temp
	out EIMSK, temp							; write it back to EIMSK

	; Sets up Timer0
	ldi temp, 0b00000010
	out TCCR0B, temp
	ldi temp, 1<<TOIE0		; turns overflow interrupt bit on
	sts TIMSK0, temp

	; Sets up Timer2
	ldi temp, 0b00000010
	sts TCCR2B, temp
	ldi temp, 1<<TOIE2		; turns overflow interrupt bit on
	sts TIMSK2, temp

	; Sets up Timer5
	ldi temp, 0b00000100
	sts TCCR5B, temp
	clr temp
	sts TCCR5C, temp
	sts TCCR5A, temp

	; Sets up LCD
	do_lcd_command 0b00111000 ; sets interface data length (8-bit); 2 line display; 5x7 font size
	call sleep_5ms
	do_lcd_command 0b00111000 ; 
	call sleep_1ms
	do_lcd_command 0b00111000 ; 
	do_lcd_command 0b00111000 ; 	
	do_lcd_command 0b00001000 ; display off??
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001100 ; cursor off; not visible; not flashing

;==================================================
; START SCREEN
;==================================================		
Start:
	; Change screen mode
	set_var Screen, STARTSCREEN

	do_lcd_data '2'
	do_lcd_data '1'
	do_lcd_data '2'
	do_lcd_data '1'
	do_lcd_data ' '
	do_lcd_data '1'
	do_lcd_data '6'
	do_lcd_data 's'
	do_lcd_data '1'

	do_lcd_command 0b11000000		; pushes cursor to new line

	do_lcd_data 'S'
	do_lcd_data 'a'
	do_lcd_data 'f'
	do_lcd_data 'e'
	do_lcd_data ' '
	do_lcd_data 'C'
	do_lcd_data 'r'
	do_lcd_data 'a'
	do_lcd_data 'c'
	do_lcd_data 'k'
	do_lcd_data 'e'
	do_lcd_data 'r'



	sei
	
		

	jmp main	

;==================================================
; COUNTDOWN SCREEN
;==================================================
CountDownScreen:
	; Change screen mode
	set_var Screen, CDSCREEN
	
	clr temp2
	
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110  ; goes to start of display
	do_lcd_command 0b00001100 ; cursor off; not visible; not flashing

	do_lcd_data '2'
	do_lcd_data '1'
	do_lcd_data '2'
	do_lcd_data '1'
	do_lcd_data ' '
	do_lcd_data '1'
	do_lcd_data '6'
	do_lcd_data 's'
	do_lcd_data '1'

	do_lcd_command 0b11000000		; pushes cursor to new line

	do_lcd_data 'S'
	do_lcd_data 't'
	do_lcd_data 'a'
	do_lcd_data 'r'
	do_lcd_data 't'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'g'
	do_lcd_data ' '
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data ' '
	do_lcd_data '3'
	do_lcd_data '.'
	do_lcd_data '.'
	do_lcd_data '.'
	
	do_lcd_command 0b00010000
	do_lcd_command 0b00010000
	do_lcd_command 0b00010000
	do_lcd_command 0b00010000

	;clear_two TempCounter
	set_var CDTime, 3

	; Set countdown flag
	set_var CDOn, 1
	; Sets the game to first round 
	set_var Round, 1 
	; sets new round flag to 1	
	set_var NewRound, 1
	; Countdown Loop
	CDScreenLoop:
		; Loops to check when PrintNow flag is on
		ldi ZH, high(PrintNow)
		ldi ZL, low(PrintNow)
		ld temp3, Z
		cpi temp3, 1
		brne CDScreenLoop
		
		; Once it's on, 1 second has passed
		ldi YH, high(CDTime)
		ldi YL, low(CDTime)
		ld temp, Y
		cpi temp, 0
		;breq testScreen ; TEST SCREEN BREQ
		breq startGame ; Goes to next screen

		; Outputting countdown number to LCD
		push temp
		ori temp, 0x30	; Make  ASCII (0x30 = 48)
		do_lcd_data_a temp
		pop temp

		do_lcd_command 0b00010000 ; Cursor Left
		set_var PrintNow, 0
		rjmp CDScreenLoop
/*
;TEST
testScreen:
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110  ; goes to start of display
	do_lcd_command 0b00001100 ; cursor off; not visible; not flashing

	ldi YH, high(Code1)
	ldi YL, low(Code1)
	ld temp, Y
	ori temp, 0x30
	do_lcd_data_a temp

	ldi YH, high(Code2)
	ldi YL, low(Code2)
	ld temp, Y
	ori temp, 0x30
	do_lcd_data_a temp

	ldi YH, high(Code3)
	ldi YL, low(Code3)
	ld temp, Y
	ori temp, 0x30
	do_lcd_data_a temp

	hhere:
		rjmp hhere
*/
;==================================================
; RESET POT SCREEN
;==================================================		
startGame:
	set_var Screen, RESETPOTSCREEN
	set_var CDOn, 0

	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001100 ; cursor off; not visible; not flashing

	do_lcd_data 'R'
	do_lcd_data 'e'
	do_lcd_data 's'
	do_lcd_data 'e'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'P'
	do_lcd_data 'O'
	do_lcd_data 'T'
	do_lcd_data ' '
	do_lcd_data 't'
	do_lcd_data 'o'
	do_lcd_data ' '
	do_lcd_data '0'

	do_lcd_command 0b11000000		; pushes cursor to new line

	do_lcd_data 'R'
	do_lcd_data 'e'
	do_lcd_data 'm'
	do_lcd_data 'a'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'g'
	do_lcd_data ':'
	do_lcd_data ' '


	; Set countdown flag
	set_var CDOn, 1

	; Set PotGood flag. i.e. Pot countdown flag (timer2)
	set_var PotGood, 0 ; pot is not good, 

	; Set PotTarget flag
	;set_var PotTarget, 0
	
	; Set PotCountdown (500ms - TIMER2) flag
	set_var PotCountdown, 1
	
	ldi XH, high(NewRound)
	ldi XL, low(NewRound)
	ld temp, X
	cpi temp, 0 ;If it is not a new round	
	
	breq POTZeroLoop
	
	; Set countdown timer (WILL BE CHANGED BASED ON DIFFICULTY)
	set_var CDTime, 20 ; in the future call the difficulty function 
	;Sets Pot Target:
	lds potTarget, TCNT5L ; sets Target based on timer 5
	//Potentiometer is 6 bit value
  	lsr potTarget
	lsr potTarget

	; POT read loop
	POTZeroLoop:
		; Pot is in correct position for 500ms
		ldi XH, high(PotCountdown)
		ldi XL, low(PotCountdown)
		ld temp, X
		cpi temp, 0
		brne PotNotZero
		
		jmp findPot

		PotNotZero: ; Pot is not zero or may be zero but not for 500ms
		; If not, load PotValue
		ldi YH, high(PotValue)
		ldi YL, low(PotValue)		

		adc_read
		
		; Sees if POT is in correct position (Zero)
		ld temp2, Y	; only looks at lower byte (due to bit shifting)
		cpi temp2, 0 ; only looks at lower byte (due to bit shifting)
		brne POTZeroWrong ; If not, keep reading
		
		; POT is in correct position
		;ser temp3
		;out PORTC, temp3	
		set_var PotGood, 1 ; pot is good, stop countdown 
		rjmp resetPOTTimer

		; Constantly shows the value of POT in LED (TESTING PURPOSES)
		POTZeroWrong: 
			rcall sleep_5ms
			rcall sleep_5ms
			rcall sleep_5ms
			;out PORTC, temp2
			set_var PotCountdown, 1
			set_var PotGood, 0 ; if pot is not good, continue countdown 

		; Countdown Timer
		resetPOTTimer:
			; Print flag
			ldi ZH, high(PrintNow)
			ldi ZL, low(PrintNow)
			ld temp3, Z
			cpi temp3, 1
			;brne resetPOTLoop
			brne POTZeroLoop		
		
			ldi YH, high(CDTime)
			ldi YL, low(CDTime)
			ld temp, Y
			cpi temp, 0
			brne printResetPotTime
			jmp gameOverScreen

			printResetPotTime:
			; Outputting countdown number to LCD
			push temp
			ori temp, 0x30	; Make  ASCII (0x30 = 48)
			do_lcd_data_a temp
			pop temp

			; Cursor Left		
			do_lcd_command 0b00010000
			set_var PrintNow, 0
			rjmp POTZeroLoop
			
		
;==================================================
; FIND POT SCREEN
;==================================================
findPOT:

	set_var Screen, FINDPOTSCREEN
	set_var CDOn, 1 ; continue countdown

	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001100 ; cursor off; not visible; not flashing

	do_lcd_data 'F'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'd'
	do_lcd_data ' '
	do_lcd_data 'P'
	do_lcd_data 'O'
	do_lcd_data 'T'
	do_lcd_data ' '
	do_lcd_data 'P'
	do_lcd_data 'o'
	do_lcd_data 's'

	do_lcd_command 0b11000000		; pushes cursor to new line

	do_lcd_data 'R'
	do_lcd_data 'e'
	do_lcd_data 'm'
	do_lcd_data 'a'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'g'
	do_lcd_data ':'
	do_lcd_data ' '
	
	;PotTarget is random
	;Player will need to hold the correct pot for 1 sec.

	
;	print:
;		out PORTC, potTarget
;		jmp print
	; Set countdown flag

	; Set PotGood flag. i.e. Pot countdown flag (timer2)
	set_var PotGood, 0 ; pot is not good, 

	; Set PotCountdown (500ms - TIMER2) flag
	set_var PotCountdown, 2 ; 2 = 1Sec

	POTFindLoop:
		; Pot is in correct position for 1S
		ldi XH, high(PotCountdown)
		ldi XL, low(PotCountdown)
		ld temp, X
		cpi temp, 0
		brne PotNotCorrect
		
		set_var CDOn, 0
	;	set_var PotGood, 1
		jmp findCode ; NEED TO CHANGE

		PotNotCorrect: ; Pot is not correct or may be  but not for 1s
		; If not, load PotValue
		ldi YH, high(PotValue)
		ldi YL, low(PotValue)		

		adc_read
		
		; Sees if POT is in correct position (Zero)
		ld temp2, Y	; only looks at lower byte (due to bit shifting)
		cp temp2, potTarget ; only looks at lower byte (due to bit shifting)
		brne POTWrong ; If not, keep reading
		
		; POT is in correct position
		ser temp3
		out PORTC, temp3
		ldi temp3, 0b00000011 ; within 16 ADC counts
		out PORTG, temp3	
		set_var PotGood, 1 ; pot is good, stop countdown 
		rjmp FindPOTTimer

		; Constantly shows the value of POT in LED (TESTING PURPOSES)
		
		POTWrong: 

			rcall sleep_5ms
			rcall sleep_5ms
			rcall sleep_5ms
			;out PORTC, temp2
			
			cp temp2, potTarget ; flag is still set, this is not neccessary
		
			brlt potLessThan ; current position is less than target
			;
			; FLAG IS > 
			; go back to start game (i.e. reset pot to zero)
			clr temp3
			out PORTC, temp3
			out PORTG, temp3
			set_var NewRound, 0 
			jmp	startGame	
			;
			; FLAG IS LESS THAN
			;(handles within 48 and 32 ADC counts)
			potLessThan:
			set_var PotCountdown, 2
			set_var PotGood, 0
			;Checks if POT is within 32 ADC counts
			mov temp3, temp2
			subi temp3, -0b00000010
			cp potTarget, temp3
			brlt ADC32

			;Checks if POT is within 48 ADC counts
			mov temp3, temp2
			subi temp3, -0b00000011
			cp potTarget, temp3
			brlt ADC48
			
			jmp FindPOTTimer
			
				ADC32:
				ser temp3
				out PORTC, temp3
				ldi temp3, 0b00000001
				out PORTG, temp3
				rjmp FindPOTTimer
				
				ADC48:
				ser temp3
				out PORTC, temp3
				clr temp3
				out PORTG, temp3
				rjmp FindPOTTimer

		; Countdown Timer
		FindPOTTimer:
			; Print flag
			ldi ZH, high(PrintNow)
			ldi ZL, low(PrintNow)
			ld temp3, Z
			cpi temp3, 1
			brne jumpPotFindLoop		
		
			ldi YH, high(CDTime)
			ldi YL, low(CDTime)
			ld temp, Y
			cpi temp, 0
			brne printPotTime
			jmp gameOverScreen

			printPotTime:
			; Outputting countdown number to LCD
			push temp
			ori temp, 0x30	; Make  ASCII (0x30 = 48)
			do_lcd_data_a temp
			pop temp

			; Cursor Left		
			do_lcd_command 0b00010000
			set_var PrintNow, 0
			rjmp POTFindLoop
		
		jumpPotFindLoop:
			jmp POTFindLoop
;==================================================
; FIND CODE SCREEN
;==================================================
findCode:
	set_var Screen, FINDCODESCREEN
	set_var CDOn, 0 ; stop countdown
	set_var PotGood, 0
	clr temp
	out PORTC, temp
	out PORTG, temp
	
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001100 ; cursor off; not visible; not flashing
	do_lcd_data 'P'
	do_lcd_data 'o'
	do_lcd_data 's'
	do_lcd_data 'i'
	do_lcd_data 't'
	do_lcd_data 'i'
	do_lcd_data 'o'
	do_lcd_data 'n'
	do_lcd_data ' '
	do_lcd_data 'F'
	do_lcd_data 'o'
	do_lcd_data 'u'
	do_lcd_data 'n'
	do_lcd_data 'd'
	do_lcd_data '!'


	do_lcd_command 0b11000000		; pushes cursor to new line
	do_lcd_data 'S'
	do_lcd_data 'c'
	do_lcd_data 'a'
	do_lcd_data 'n'
	do_lcd_data ' '
	do_lcd_data 'f'
	do_lcd_data 'o'
	do_lcd_data 'r'
	do_lcd_data ' '
	do_lcd_data 'n'
	do_lcd_data 'u'
	do_lcd_data 'm'
	do_lcd_data 'b'
	do_lcd_data 'e'
	do_lcd_data 'r'

	set_var PreviousKey, 0xFF

	; set code here
	
	ldi XH, high(Round)
	ldi XL, low(Round)
	ld temp, X
	
	
	cpi temp, 1
	breq setCode1
	cpi temp, 2
	breq setCode2
	cpi temp, 3
	breq setCode3
	
	setCode1:
	set_code Code1
	rjmp formatCode
	setCode2:
	set_code Code2
	rjmp formatCode
	setCode3:
	set_code Code3
	rjmp formatCode


   ;do_lcd_data temp
	
	;out PORTC, temp
	formatCode:
	;set_var Code, 14
	ldi XH, high(Code)
	ldi XL, low(Code)
	ld temp, X
	cpi temp, 14
	brne HandleKey
	subi temp, 1
	st X, temp
	
	HandleKey:
		clr currKey
		;motor_off
		;rcall sleep_5ms
		;out PORTC, currKey
		scan_keypad	; looks what key is pressed 
		;out PORTC, currKey
		;jmp HandleKey
		; check if key is correct
		ldi YH, high(PreviousKey)
		ldi YL, low(PreviousKey)
		ld temp, Y
		cp currKey, temp ; same as previous key
		breq KeyCountDown; if not key is pressed than look again
			resetKeyTimer:
				set_var CDOn, 0
				set_var CDTime, 1
				clear_two TempCounter
				ldi YH, high(PreviousKey)
				ldi YL, low(PreviousKey)
				st Y, currKey
				motor_off
				jmp HandleKey

		KeyCountdown:
			ldi XH, high(code)
			ldi XL, low(code)
			ld temp2, X
			cp currKey, temp2
			brne resetKeyTimer
			motor_on
			set_var CDOn, 1
			ldi XH, high(CDTime)
			ldi XL, low(CDTime)
			ld temp2, X
			cpi temp2, 0
			breq roundEnd
			
			jmp HandleKey
/*
			clr currKey
			scan_keypad
			; look at previous key
			ldi YH, high(PreviousKey)
			ldi YL, low(PreviousKey)
			ld temp, Y
			
			;are keys still the same 
			cp currKey, temp
			;brne


			;set key to previous key

		jmp HandleKey
*/
	roundEnd:
		motor_off
		ldi YH, high(Round)
		ldi YL, low(Round)
		ld temp, Y
		inc temp ; next Round !
		st Y, temp
		cpi temp, 4
		brne nextRound
		jmp gameWinScreen
		nextRound:
		set_var NewRound, 1
		jmp startGame

;==================================================
; ENTER CODE SCREEN
;==================================================
enterCode:
	set_var Screen, ENTERCODESCREEN
	set_var CDOn, 0

	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001100 ; cursor off; not visible; not flashing

	do_lcd_data 'E'
	do_lcd_data 'n'
	do_lcd_data 't'
	do_lcd_data 'e'
	do_lcd_data 'r'
	do_lcd_data ' '
	do_lcd_data 'C'
	do_lcd_data 'o'
	do_lcd_data 'd'
	do_lcd_data 'e'
	
	do_lcd_command 0b11000000		; pushes cursor to new line

	set_var Round, 1

	InputCode:
		ldi XH, high(Round)
		ldi XL, low(Round)
		ld temp, X
	
		cpi temp, 1
		breq setCheckCode1
		cpi temp, 2
		breq setCheckCode2
		cpi temp, 3
		breq setCheckCode3
	
		setCheckCode1:
		set_code Code1
		rjmp CheckCode
		setCheckCode2:
		set_code Code2
		rjmp CheckCode
		setCheckCode3:
		set_code Code3		
		rjmp CheckCode

		CheckCode:
			ld temp2, X
			cpi temp, 4
			breq winJumper
			ldi YH, high(Code)
			ldi YL, low(Code)
			ld temp, Y
			clr currKey
			scan_keypad
			cp currKey, temp
			brne enterCode
			do_lcd_data '*'
			inc temp2
			st X, temp2
			jmp InputCode

	winJumper:
		jmp gameWinScreen

;==================================================
; TIMEOUT SCREEN
;==================================================
gameOverScreen:
	set_var Screen, TIMEOUTSCREEN
	set_var CDOn, 0

	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001100 ; cursor off; not visible; not flashing

	do_lcd_data 'G'
	do_lcd_data 'a'
	do_lcd_data 'm'
	do_lcd_data 'e'
	do_lcd_data ' '
	do_lcd_data 'o'
	do_lcd_data 'v'
	do_lcd_data 'e'
	do_lcd_data 'r'

	do_lcd_command 0b11000000		; pushes cursor to new line

	do_lcd_data 'Y'
	do_lcd_data 'o'
	do_lcd_data 'u'
	do_lcd_data ' '
	do_lcd_data 'L'
	do_lcd_data 'o'
	do_lcd_data 's'
	do_lcd_data 'e'
	do_lcd_data '!'
	
	rjmp main

;==================================================
; GAME COMPLETE SCREEN
;==================================================
gameWinScreen:
	set_var Screen, COMPLETESCREEN
	set_var CDOn, 0

	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001100 ; cursor off; not visible; not flashing

	do_lcd_data 'G'
	do_lcd_data 'a'
	do_lcd_data 'm'
	do_lcd_data 'e'
	do_lcd_data ' '
	do_lcd_data 'c'
	do_lcd_data 'o'
	do_lcd_data 'm'
	do_lcd_data 'p'
	do_lcd_data 'l'
	do_lcd_data 'e'
	do_lcd_data 't'
	do_lcd_data 'e'

	do_lcd_command 0b11000000		; pushes cursor to new line

	do_lcd_data 'Y'
	do_lcd_data 'o'
	do_lcd_data 'u'
	do_lcd_data ' '
	do_lcd_data 'W'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data '!'
	
	rjmp main



/*
ADCCOVF:
	push temp
	push temp2
	push ZH
	push ZL

	lds temp, ADCL
	lds temp2, ADCH
	
	ldi ZH, high(PotValue)
	ldi ZL, low(PotValue)
	st Z, temp

	out PORTC, temp

	pop ZL
	pop ZH
	pop temp2
	pop temp
	reti
*/
ADCCOVF:
	push r16
	in r16, SREG
	push r16
	push r17
	push ZH
	push ZL

	lds r16, ADCL
	lds r17, ADCH

	//ADC >> 4 = ADC/16; gives value to compare to where equality is +/- 16 of actual value
	lsr r17
	ror r16
	lsr r17
	ror r16
	lsr r17
	ror r16
	lsr r17
	ror r16

	ldi ZH, high(PotValue)
	ldi ZL, low(PotValue)
	st Z, r16

	pop ZL
	pop ZH
	pop r17
	pop r16
	out SREG, r16
	pop r16
	reti

;==================================================
; TIMER0
;==================================================
Timer0OVF:

	init:
		;push temp
		in temp, SREG				; store current state of status registers in temp
		push temp					; push conflict registers onto the stack
		push temp2
		push YH
		push YL
		push r25
		push r24


		lds r24, TempCounter
		lds r25, TempCounter+1
		adiw r25:r24, 1

		cpi r24, low(7812)			; 256*8/clock speed, where clockspeed is 16MHz then 1000000/128 = 7812, 1 second
		ldi temp, high(7812)		
		cpc r25, temp
		brne NotSecond				; if it is not a second yet skip to notSecond

		


		
		
	isSecond:
		clear_two TempCounter   		; reset the temporary counter.

	; Check if CDOn is enabled, if so, decrement the countdown time
	startCountdown:
		;clr temp2
		;st Y, temp2

		ldi YH, high(CDOn)
		ldi YL, low(CDOn)
		ld temp2, Y
		cpi temp2, 0
		breq EndIF
	
		decrementSeconds:
			set_var PrintNow, 1
			ldi YH, high(CDTime)
			ldi YL, low(CDTime)
			ld temp2, Y
			dec temp2
			st Y, temp2


		rjmp EndIF ; skip notsecond
			
	NotSecond:
		sts TempCounter, r24
		sts TempCounter+1, r25

	EndIF:
		pop r24          
        pop r25          ; restoring conflict registers
        pop YL
        pop YH
		pop temp2
		pop temp
        out SREG, temp
		;pop temp
        reti         		; return from interrupt

;==================================================
; TIMER2 (POT TIMER)
;==================================================
Timer2OVF:
		in temp, SREG				; store current state of status registers in temp
		push temp					; push conflict registers onto the stack
		push temp2
		push YH
		push YL
		push r25
		push r24


		lds r24, HalfCounter
		lds r25, HalfCounter+1
		adiw r25:r24, 1

		cpi r24, low(3906)			; 256*8/clock speed, where clockspeed is 16MHz then 1000000/128 = 7812, 1 second
		ldi temp, high(3906)		
		cpc r25, temp
		brne NotHalfSecond

	isHalfSecond:
		clear_two HalfCounter   		; reset the temporary counter.

	checkPotGood:
		ldi YH, high(POTGood)
		ldi YL, low(POTGood)
		ld temp2, Y
		cpi temp2, 0
		breq EndHalf

		decrementPotTime:
			ldi YH, high(PotCountdown)
			ldi YL, low(PotCountdown)
			ld temp, Y
			dec temp
			st Y, temp
			rjmp EndHalf

	NotHalfSecond:
		sts HalfCounter, r24
		sts HalfCounter+1, r25

	EndHalf:
		pop r24          
        pop r25          ; restoring conflict registers
        pop YL
        pop YH
		pop temp2
        pop temp
        out SREG, temp
        reti    




checkMilli0:
		lds r24, MilliCounter
		lds r25, MilliCounter+1
		adiw r25:r24, 1

		cpi r24, low(2725)
		ldi temp, high(2725)
		cpc r25, temp
		brne NotMilli0
		clr debounce
		clear_two MilliCounter
		ret

NotMilli0:
		sts MilliCounter, r24
		sts MilliCounter+1, r25

checkMilli1:
		lds r26, MilliCounter
		lds r27, MilliCounter+1
		adiw r27:r26, 1

		cpi r26, low(780) ;2725
		ldi temp, high(780)
		cpc r27, temp
		brne NotMilli1
		clr debounce
		clear_two MilliCounter
		ret

NotMilli1:
		sts MilliCounter, r26
		sts MilliCounter+1, r27

;==================================================
; PB0 BUTTON - QUIT
;==================================================
EXT_INT0:
	jmp RESET
	/*
    cpi debounce, 0
	breq isDebounced0
	rcall checkMilli0
	reti
	;rjmp EXT_INT0

	isDebounced0:
		inc debounce
		ldi cdown, 0
		ldi isReset,1
		;ser temp
		;out PORTC, temp
		;jmp RESET
		reti
	*/

;==================================================
; PB1 BUTTON - START GAME
;==================================================

EXT_INT1:
	cpi debounce, 0
	breq isDebounced1
	rcall checkMilli1
	;reti

	isDebounced1:
		inc debounce
		sei
		;add to stack
		in temp, SREG				; store current state of status registers in temp
		push temp					; push conflict registers onto the stack
		push temp2
		push YH
		push YL
		;CODE 1
		lds temp ,TCNT5L
		lsr temp
		lsr temp
		lsr temp
		lsr temp
		ldi YH, high(Code1)
		ldi YL, low(Code1)
		st Y, temp
		;CODE3
		lds temp ,TCNT5H
		lsr temp
		lsr temp
		lsr temp
		lsr temp
		ldi YH, high(Code3)
		ldi YL, low(Code3)
		st Y, temp
		;CODE2
		;maybe add delay
		lds temp ,TCNT2
		lsr temp
		lsr temp
		lsr temp
		lsr temp
		ldi YH, high(Code2)
		ldi YL, low(Code2)
		st Y, temp
		ldi YH, high(Screen)
		ldi YL, low(Screen)
		ld temp, Y
		cpi temp, STARTSCREEN
		breq jumpCD

		jmp RESET
		;reti
		
		jumpCD:
		jmp CountDownScreen
		;jmp 


;==================================================
; MAIN
;==================================================
main:

	/*


stop:
	cpi isReset, 1
	breq return
	rjmp stop
*/
return:
	rjmp return



;==================================================
; LCD FUNCTIONS
;==================================================

lcd_command:
	out PORTF, temp
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	ret

lcd_data:
	out PORTF, temp
	lcd_set LCD_RS
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	lcd_clr LCD_RS
	ret

lcd_wait:
	push temp
	clr temp
	out DDRF, temp
	out PORTF, temp
	lcd_set LCD_RW

lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	in temp, PINF
	lcd_clr LCD_E
	sbrc temp, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser temp
	out DDRF, temp
	pop temp
	ret

;==================================================
; SLEEP FUNCTIONS
;==================================================

.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4 ; 4 cycles per iteration - setup/call-return overhead

sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)

delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret

sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret

