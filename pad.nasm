;*************************************************;
; Finish and pad
;*************************************************;

; Pad to 510 bytes (boot sector size minus 2) with 0s, and finish with the two-byte standard boot signature
times 512-($-$$)-2  db 0 
dw 0xAA55	        ; => 0x55 0xAA (little endian byte order)