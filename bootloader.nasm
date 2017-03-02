;;bootloader.nasm
;**********************************************************************************;
; Begin sector 1 - loaded at 
;**********************************************************************************;

	bits		16
	org			0x7c00
	jmp short	_start	; Jump past disk description section


;*************************************************;
; OEM Parameter block / BIOS Parameter Block
;*************************************************;
OEMLabel	    	db "FogsKink "	; Disk label
BytesPerSector		dw 512		; Bytes per sector
SectorsPerCluster	db 1		; Sectors per cluster
ReservedForBoot		dw 1		; Reserved sectors for boot record
NumberOfFATs		db 2		; Number of copies of the FAT
RootDirEntries		dw 224		
LogicalSectors		dw 2880		; Number of logical sectors
MediumByte	    	db 0F0h		; Medium descriptor byte
SectorsPerFAT		dw 9		; Sectors per FAT
SectorsPerTrack		dw 18		; Sectors per track (36/cylinder)
Sides		    	dw 2		; Number of sides/heads
HiddenSectors		dd 0		; Number of hidden sectors
LargeSectors		dd 0		; Number of LBA sectors
DriveNo		    	db 0		; Drive No: 0
Unused				db 0		; Unused sectors 
Signature	    	db 29h		; Drive signature: 41 (0x29) for floppy
VolumeID	    	dd 12345678h	; Volume ID: any number
VolumeLabel	    	db "My First OS"; Volume Label: any 11 chars
FileSystem	    	db "FAT12   "	; File system type: don't change!

;*************************************************;
; Print null-terminated string in 'si' register to screen
;*************************************************;
print:
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
        mov     ch, BYTE [absoluteTrack]            ; track
        mov     cl, BYTE [absoluteSector]           ; sector
        mov     dh, BYTE [absoluteHead]             ; head
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
        add     bx, WORD [bpbBytesPerSector]        ; queue next buffer
        inc     ax                                  ; queue next sector
        loop    .MAIN                               ; read next sector
        ret

;************************************************;
; Convert CHS to LBA
; AX=>Cluster start
; LBA = (cluster - 2) * sectors per cluster
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

;*************************************************;
; Bootloader start
;*************************************************;

_start:

	cli		; Disable interrupts
	
	;----------------------------------------------------
	; Set up data segment
	;----------------------------------------------------
    mov 	ax, 0x07C0   ; Set 'ax' equal to the location of this bootloader divided by 16
    mov 	ds, ax
    mov 	es, ax
    mov 	fs, ax
    mov 	gs, ax
	
	;----------------------------------------------------
	; Set up stack
	;----------------------------------------------------
	mov     ax, 0x0000				; set the stack
    mov     ss, ax
    mov     sp, 0xFFFF
    sti								; restore interrupts
	
	;----------------------------------------------------
	; Print our message
	;----------------------------------------------------
	mov 	si, message 	; Put address of the null-terminated string to output into 'si'
	call	print			; Call our string-printing routine
	
	;----------------------------------------------------
	; Load root directory table
	; Read all sectors required for root directory table immediately following this boot sector in memory (0x200)
	;----------------------------------------------------

LOAD_ROOT:
    ; Compute the number of sectors used by root directory table, store in CX
    ; (sizeof entry * numEntries) / BytesPerSector = numSectors
    xor     cx, cx
	mov     ax, 0x0020      		; 32 byte directory entry
	mul     WORD [RootDirEntries]	; number of root entrys
	div     WORD [BytesPerSector]	; get sectors used by root directory
	xchng	ax, cx					; ax = 0, cx = sectors used by RDT
	
	; Compute starting sector for root directory table, store in AX
	; (numFATs * SectorsPerFAT) + ReservedSectors = starting sector for RDT
	mov     al, [NumberOfFATs]  	; Get number of FATs (Usually 2)
	mul     [SectorsPerFAT] 		; number of FATs * sectors per FAT; get number of sectors
	add     ax, [ReservedForBoot]	; add reserved sectors
	mov     WORD [datasector], ax   ; base of root directory
	add     WORD [datasector], cx
	
	; AX = starting sector
	; CX = sectors used by RDT
	mov     bx, 0x0200  			; load root directory to 7c00:0x0200
	call    ReadSectors
	
	;----------------------------------------------------
	; Find second stage binary
	; 
	;----------------------------------------------------
	
FIND_SECOND:
	mov     cx, [RootDirEntries]	; the number of entrys. If we reach 0, file doesnt exist
    mov     di, 0x0200				; Root directory was loaded here
.find_loop:
	push    cx
    mov     cx, 11          		; eleven character name
    mov     si, ImageName   		; compare the 11 bytes with the name of our file
    push    di
		repe	cmpsb					; while (cx > 0 && zf == 0) {zf = *si - *di; cx--;};
	pop     di
    je      LOAD_FAT        		; (zf == 0) they match, so begin loading FAT
    pop     cx
    add     di, 32          		; they dont match, so go to next entry (32 bytes)
    loop    .find_loop				; Decrement CX, jump not zero to top
    jmp     FAILURE         		; no more entrys left, file doesnt exist

	;----------------------------------------------------
	; Load FAT
	; di=>Stage 2 bootloader file entry in Root Directory Table
	;----------------------------------------------------

LOAD_FAT:
	mov 	ax, WORD [di + 0x001A]	; AX stores the starting cluster number

	xor 	ax, ax
	xor		cx, cx
	mov		
	
	xor		ax, ax
	mov		al, [NumberOfFATs]
	mul		[SectorsPerFAT]	; AX = number of sectors that used by FATs
	
	mov		cl, [ReservedForBoot]	; Start offset to reserved sectors
	
	;AH = 0x02
	;AL = Number of sectors to read
	;CH = Low eight bits of cylinder number
	;CL = Sector Number (Bits 0-5). Bits 6-7 are for hard disks only
	;DH = 1? -- Head Number
	;DL = 0 -- Drive Number (Bit 7 set for hard disks)
	;ES:BX = Buffer to read sectors to
	
	;; TODO This is broken
	
	mov		bx, 0x0200
	call	readsectors	; Read FAT immediately after this boot sector in memory
	
	
	call	reset		; Reset the floppy drive to sector 0
	call	load_root	; Load root directory table immediately after this boot sector in memory
	
	
data:
	message db 'Hello world from the first stage', 0
 

;*************************************************;
; Finish and pad
;*************************************************;

; Pad to 510 bytes (boot sector size minus 2) with 0s, and finish with the two-byte standard boot signature
times 510-($-$$) db 0 
dw 0xAA55	        ; => 0x55 0xAA (little endian byte order)


;**********************************************************************************;
; End of sector 1, beginning of sector 2
;**********************************************************************************;

org 0x1000			; This sector is loaded at 0x1000:0 by the bootsector

    ; Set up stack segment [Remember: Effective Address = Segment*16 + Offset]
    mov ax, 07C0h   ; Set 'ax' equal to the location of this bootloader divided by 16
    add ax, 20h     ; Skip over the size of the bootloader divided by 16
    mov ss, ax      ; Set 'ss' to this location (the beginning of our stack region)
    mov sp, 4096

	cli             ; Clear the Interrupt Flag (disable external interrupts)
	hlt             ; Halt the CPU (until the next external interrupt)