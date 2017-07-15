# AVR-Safe-Cracker-Game
Safe Cracker game built on the AVR Instruction Set, run on a custom Arduino board

![avr_board](http://i.imgur.com/Acoabms.png)

Parts of the game include:
- Countdown timer on the LCD screen (ticks down every second; harder difficulties have less time per level).
- Smooth LCD backlight fading + use of speakers to create a countdown timer sound.
- Resetting the POT (potentiometer) button
- Turning the POT to a certain (random) position. The LEDs would light up when the position was getting closer.
- Finding the secret key on the keyboard. When found, the motor begins spinning. Must be held down for 1 second.
- Do this 3 times to find the secret combination. Enter the combination to win the game.