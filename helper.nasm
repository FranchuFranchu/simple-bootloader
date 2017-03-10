;*************************************************;
; Print null-terminated string in 'si' register to screen
;*************************************************;
Print:
	mov ah, 0Eh     ; Specify 'int 10h' 'teletype output' function
	                ; [AL = Character, BH = Page Number, BL = Colour (in graphics mode)]
.printchar:
	lodsb           ; Load byte at address SI into AL, and increment SI
	cmp al, 0
	je .done        ; If the character is zero (NUL), stop writing the string
	int 10h         ; Otherwise, print the character via 'int 10h'
	jmp .printchar  ; Repeat for the next character
.done:
	ret

;*************************************************;
; Reset floppy disk
;
;INT 0x13/AH=0x0 - DISK : RESET DISK SYSTEM
;AH = 0x0
;DL = Drive to Reset
;
;Returns:
;AH = Status Code
;CF (Carry Flag) is clear if success, it is set if failure
;*************************************************;
reset:
	mov		ah, 0					; reset floppy disk function
	mov		dl, 0					; drive 0 is floppy drive
	int		0x13					; call BIOS
	jc		reset					; If Carry Flag (CF) is set, there was an error. Try resetting again
	ret

;************************************************;
; Reads a series of sectors
; CX=>Number of sectors to read
; AX=>Starting sector
; ES:BX=>Buffer to read to
;
; INT 0x13/AH=0x02 - DISK : READ SECTOR(S) INTO MEMORY
; AH = 0x02
; AL = Number of sectors to read
; CH = Low eight bits of cylinder number
; CL = Sector Number (Bits 0-5). Bits 6-7 are for hard disks only
; DH = Head Number
; DL = Drive Number (Bit 7 set for hard disks)
; ES:BX = Buffer to read sectors to
;
; Returns:
; AH = Status Code
; AL = Number of sectors read
; CF = set if failure, cleared is successfull
;************************************************;

ReadSectors:
    .MAIN
        mov     di, 0x0005                          ; five retries for error
    .SECTORLOOP
        push    ax
        push    bx
        push    cx
        call    LBACHS                              ; convert starting sector to CHS
        mov     ah, 0x02                            ; BIOS read sector
        mov     al, 0x01                            ; read one sector
        mov     ch, BYTE [absTrack]                 ; track
        mov     cl, BYTE [absSector]                ; sector
        mov     dh, BYTE [absHead]                  ; head
        mov     dl, BYTE [bsDriveNumber]            ; drive
        int     0x13                                ; invoke BIOS
        jnc     .SUCCESS                            ; test for read error
        xor     ax, ax                              ; BIOS reset disk
        int     0x13                                ; invoke BIOS
        dec     di                                  ; decrement error counter
        pop     cx
        pop     bx
        pop     ax
        jnz     .SECTORLOOP                         ; attempt to read again
        int     0x18
    .SUCCESS
        mov     si, msgProgress
        call    Print
        pop     cx
        pop     bx
        pop     ax
        add     bx, WORD [BytesPerSector]           ; queue next buffer
        inc     ax                                  ; queue next sector
        loop    .MAIN                               ; read next sector
        ret

;************************************************;
; Convert CHS to LBA
; AX=>Cluster start
; AX -> LBA = (cluster - 2) * sectors per cluster
;************************************************;
ClusterLBA:
    sub     ax, 0x0002                          ; zero base cluster number
    xor     cx, cx
    mov     cl, BYTE [SectorsPerCluster]		; convert byte to word
    mul     cx
    add     ax, WORD [datasector]               ; base data sector
    ret

;************************************************;
; Convert LBA to CHS
; AX=>LBA Address to convert
;
; absolute sector = (logical sector / sectors per track) + 1
; absolute head   = (logical sector / sectors per track) % number of heads
; absolute track  = logical sector / (sectors per track * number of heads)
;
;************************************************;
LBACHS:
	xor		dx, dx
	; absSector = (LBA % SectorsPerTrack) + 1
	;AX assumed to contain LBA
	div		WORD [SectorsPerTrack]		; ax = LBA / SectorsPerTrack
	inc		dl
	mov		BYTE [absSector], dl
	
	; absHead = (LBA / SectorsPerTrack) % Sides
	div		WORD [Sides]				; ax = (LBA / SectorsPerTrack) / Sides
	mov		BYTE [absHead], dl
	
	; absTrack = LBA / (SectorsPerTrack * Sides)
	mov		BYTE [absTrack], al
	ret