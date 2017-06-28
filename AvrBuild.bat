@ECHO OFF
"C:\Program Files (x86)\Atmel\AVR Tools\AvrAssembler2\avrasm2.exe" -S "C:\ass2\labels.tmp" -fI -W+ie -C V3 -o "C:\ass2\ass2.hex" -d "C:\ass2\ass2.obj" -e "C:\ass2\ass2.eep" -m "C:\ass2\ass2.map" "C:\ass2\ass2.asm"
