.include "m2560def.inc"

; declarations
.def curr_pattern = r16      ; curr pattern
.def next_pattern = r17      ; next pattern
.def bits_entered = r18      ; number of bits entered
.def num_flashes  = r19      ; number of flashes (3)
.def temp = r20              ; temp (timer0)
.def debounce = r21          ; debounce
.def wasFlashOn = r22        ; boolean 
.def temp2 = r24

; macros
.macro clear
    ldi yl, low(@0)
    ldi yh, high(@0)
    clr temp
    st y+, temp
    st y, temp
.endmacro

; data segment
.dseg
SecondCounter:
    .byte 2

TempCounter:
    .byte 2


; code segment
.cseg
    jmp RESET

; interrupt table
.org INT0addr
    jmp EXT_INT0

.org INT1addr
    jmp EXT_INT1

.org OVF0addr
    jmp Timer0OVF



; reset/startup
RESET:
    ; setup the stack
    ldi temp, low(RAMEND)
    out SPL, temp
    ldi temp, high(RAMEND)
    out SPH, temp

	/*
    ; clear everything
    clr curr_pattern
    clr next_pattern
    clr bits_entered
    clr num_flashes
    clr wasFlashOn
	*/
	
	clr debounce

    ; setup DDRC for output
    ser temp                ; set temp to 0xFF
    out DDRC, temp          ; setup data direction to enable pinout

    ; enable interrupt for timer0
    ldi temp, 0b00000010
    out TCCR0B, temp
    ldi temp, 1<<TOIE0                  ; turns overflow interrupt bit on
    sts TIMSK0, temp

    ; enable interrupts for pbbns 
    ldi temp, (1<<ISC11) | (1<<ISC01)   ; setting int1 and int0 for falling edge trigger, each pair contains "10"
    sts EICRA, temp                     ; store temp back into EICRA

    in temp, EIMSK                      ; store current state of EIMSK in temp
    ori temp, (1<<INT0) | (1<<INT1)     ; enable INT0 and INT1 in temp
    out EIMSK, temp                     ; write it back to EIMSK


    sei                     ; enable global interrupt

    jmp main


; timer0 overflow
Timer0OVF:
    Timer_setup:
        in temp, SREG           ; setup stack
        push temp               ; push all conflicting regs into stack
        push YH             
        push YL
        push r25
        push r24            

        lds r24, TempCounter
        lds r25, TempCounter+1
        adiw r25:r24, 1         ; increment TempCounter

        cpi r24, low(2344)      ; check every ~300ms
        ldi temp, high(2344)    ; and clear debounce
        cpc r25, temp
        brne no_debounce
        clr debounce            

        no_debounce:
        cpi r24, low(7812)      ; Check if (r25:r24) = 7812
        ldi temp, high(7812)    ; (256*8)/16MHZ = 7812 (= 1 second)
        cpc r25, temp
        brne NotSecond

        ; clear bttn debounce
        clr debounce
/*
    Flash_ops:

        cpi wasFlashOn, 1
        brge Flash_off

    
        Flash_on:
        ; flash on x3
            out PORTC, curr_pattern
            inc wasFlashOn
            rjmp End_Flash_ops
    
        Flash_off:
        ; flash off x3
            push curr_pattern       ; because I cbb setting up a temp empty register
            clr curr_pattern
            out PORTC, curr_pattern
            inc num_flashes
            pop curr_pattern            
            clr wasFlashOn

        ; update output after 3 flashes
        ; if bits_entered == 8
        End_Flash_ops:
            cpi num_flashes, 3
            brlt Timer_fin

            clr curr_pattern
            
            cpi bits_entered, 8
            brlt Timer_fin

            mov curr_pattern, next_pattern
            clr next_pattern
            clr bits_entered
            clr num_flashes
            clr wasFlashOn
*/
	

    Timer_fin:
        clear TempCounter       ; Reset the temporary counter.
                                ; Load the value of the second counter.
        lds r24, SecondCounter
        lds r25, SecondCounter+1
        adiw r25:r24, 1         ; Increase the second counter by one.

        sts SecondCounter, r24
        sts SecondCounter+1, r25
        rjmp EndIF
        
    NotSecond:
        sts TempCounter, r24    ; incr counter stuff so it doesnt overflow
        sts TempCounter+1, r25
    EndIF:
        pop r24                 
        pop r25                 ; restore conflicting regs from stack
        pop YL
        pop YH
        pop temp
        out SREG, temp
   
    
    reti


; external interrupt 0 (enter 0)
EXT_INT0:
    ; if bttn is "pressed" again in the next 15ms, do nothing
        

    jmp RESET
	reti


; external interrupt 1 (enter 1)
EXT_INT1:
    cpi debounce, 0
    breq Enter_1
    reti

    Enter_1:
        rcall sleep_5ms
        rcall sleep_5ms
        rcall sleep_5ms

        ;ldi r23, 0b00001111
        ;out PORTC, r23

        inc debounce
        /*
        cpi bits_entered, 8
        breq End_Enter_1

        lsl next_pattern        ; lsl and inc next_pattern
        inc next_pattern

        inc bits_entered
		*/
		clear SecondCounter
		
		
    End_Enter_1:
	rcall count
    reti   


count:
	ldi r23, 0b00000001
	out PORTC, r23
	reti	

	/*
	lds temp2, SecondCounter
	cpi temp2, 1
	brge print1
	cpi temp2, 2
	brge print2
	cpi temp2, 3
	brge print3
	ldi r23, 4
	cpse temp2, r23
	rjmp count
	
	reti

	print1:
	ldi r23, 0b00000001
	out PORTC, r23
	rjmp count
	
	print2:
	ldi r23, 0b00000011
	out PORTC, r23
	rjmp count
	
	print3:
	ldi r23, 0b00000111
	out PORTC, r23
	rjmp count
	*/

; main
main:

    ldi r23, 0b11111111
    out PORTC, r23    

    clear SecondCounter     ; timer0 stuff
    clear TempCounter


end:
    rjmp end


; delay functions
; 4 cycles per iteration - setup/call-return overhead
.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
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
