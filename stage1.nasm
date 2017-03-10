;**********************************************************************************;
; Define Constants and Assembler Directive
;**********************************************************************************;
%define STAGE1_ENTRY		0x7C00
%define MBR_SIZE            0x200
%define BLOCKBUFFER_SIZE	0x200
%define	FIRST_PART_OFFSET	0x01BE
;*************************************************;
%define BLOCKBUFFER		STAGE1_ENTRY + MBR_SIZE
%define FIRST_PARTITION		STAGE1_ENTRY + FIRST_PART_OFFSET
%define KERNEL_ADDR		STAGE1_ENTRY + MBR_SIZE + BLOCKBUFFER_SIZE
%define DFS_DATA_BEGIN		STAGE1_ENTRY + MBR_SIZE + 0x004
%define DFS_DATA_END		STAGE1_ENTRY + MBR_SIZE + 0x1FB + 0x01
%define DFS_NEXTBLOCK_0		STAGE1_ENTRY + MBR_SIZE + 0x1FC
%define DFS_NEXTBLOCK_1		STAGE1_ENTRY + MBR_SIZE + 0x1FE

org         STAGE1_ENTRY
bits        16
;**********************************************************************************;

;**********************************************************************************;
; Begin Stage 1 
;**********************************************************************************;

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
VolumeLabel	    	db "MY_FIRST_OS"; Volume Label: any 11 chars
FileSystem	    	db "FAT12   "	; File system type: don't change!

;*************************************************;
; Jump to true start
;*************************************************;
jmp         _start

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
	mov 	si, msgStage1 	; Put address of the null-terminated string to output into 'si'
	call	Print			; Call our string-printing routine
	
	;----------------------------------------------------
	; Load root directory table
	; Read all sectors required for root directory table immediately following this boot sector in memory (0x200)
	;----------------------------------------------------

LOAD_ROOT:
    ; Print "Loading root directory table" message
    mov		si, msgLoadRoot
    call	Print
    
    ; Compute the number of sectors used by root directory table, store in CX
    ; (sizeof entry * numEntries) / BytesPerSector = numSectors
    xor     cx, cx
	mov     ax, 0x0020      		; 32 byte directory entry
	mul     WORD [RootDirEntries]	; number of root entrys
	div     WORD [BytesPerSector]	; get sectors used by root directory
	xchg	ax, cx					; ax = 0, cx = sectors used by RDT
	
	; Compute starting sector for root directory table, store in AX
	; (numFATs * SectorsPerFAT) + ReservedSectors = starting sector for RDT
	mov     al, [NumberOfFATs]  	; Get number of FATs (Usually 2)
	mul     WORD [SectorsPerFAT]    ; number of FATs * sectors per FAT; get number of sectors
	add     ax, [ReservedForBoot]	; add reserved sectors
	mov     WORD [datasector], ax   ; base of root directory
	add     WORD [datasector], cx
	
	; AX = starting sector
	; CX = sectors used by RDT
	mov     bx, 0x0200  			; load root directory to 7c00:0x0200
	call    ReadSectors
	
	;----------------------------------------------------
	; Find second stage binary
	;----------------------------------------------------
	
FIND_SECOND:
	mov		si, msgFind2
	call	Print

	mov     cx, [RootDirEntries]	; the number of entrys. If we reach 0, file doesnt exist
    mov     di, 0x0200				; Root directory was loaded here
.find_loop:
	push    cx
    mov     cx, 11          		; eleven character name
    mov     si, ImageName   		; compare the 11 bytes with the name of our file
    push    di
		repe	cmpsb				; while (cx > 0 && zf == 0) {zf = *si - *di; cx--;};
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
	mov		si, msgLoadFAT
	call	Print

	mov 	ax, WORD [di + 0x001A]	; AX stores the starting cluster number
	mov		WORD [cluster], ax		; Store in cluster

	xor 	ax, ax
	xor		cx, cx
	
	mov		al, BYTE [NumberOfFATs]
	mul		WORD [SectorsPerFAT]	; AX = number of sectors that used by FATs
	mov		cx, ax					; CX = number of sectors used by FATs
	
	mov		al, [ReservedForBoot]	; Start offset to reserved sectors
	
	mov		bx, 0x0200
	call	ReadSectors				; Read FAT immediately after this boot sector in memory

	;----------------------------------------------------
	; Load Stage 2 image
	; Load to (0x0050:0x0000)
	;----------------------------------------------------
	mov     ax, 0x0050
	mov     es, ax                              ; destination for image
	mov     bx, 0x0000                          ; destination for image
	push    bx
	
	LOAD_IMAGE:
	mov     ax, WORD [cluster]                  ; cluster to read
    pop     bx                                  ; buffer to read into
    call    ClusterLBA                          ; convert cluster to LBA
    xor     cx, cx
    mov     cl, BYTE [SectorsPerCluster]		; sectors to read
    call    ReadSectors
    push    bx
    
    ; compute next cluster
     
    mov     ax, WORD [cluster]                  ; identify current cluster
    mov     cx, ax                              ; copy current cluster
    mov     dx, ax                              ; copy current cluster
    shr     dx, 0x0001                          ; divide by two (right shift 1)
    add     cx, dx                              ; sum for (3/2)
    mov     bx, 0x0200                          ; location of FAT in memory
    add     bx, cx                              ; index into FAT
    mov     dx, WORD [bx]                       ; read two bytes from FAT
    test    ax, 0x0001
    jnz     .ODD_CLUSTER
          
    .EVEN_CLUSTER:
     
    and     dx, 0000111111111111b               ; take low twelve bits
    jmp     .DONE
         
    .ODD_CLUSTER:
     
    shr     dx, 0x0004                          ; take high twelve bits
          
    .DONE:
     
    mov     WORD [cluster], dx                  ; store new cluster
    cmp     dx, 0x0FF0                          ; test for end of file
    jb      LOAD_IMAGE
          
    DONE:
     
    mov     si, msgSuccess
    call    Print
    push    WORD 0x0050
    push    WORD 0x0000
    retf
          
    FAILURE:
     
    mov     si, msgFailure
    call    Print
    mov     ah, 0x00
    int     0x16                                ; await keypress
    int     0x19                                ; warm boot computer
     
absSector  db 0x00
absHead    db 0x00
absTrack   db 0x00
bsDriveNumber   db 0x00

     
datasector  dw 0x0000
cluster     dw 0x0000

ImageName   db	"KRNLLODRSYS", 0	
msgStage1	db	"1Stage", 0x0D, 0x0A, 0
msgLoadRoot	db	"Loading RDT", 0x0D, 0x0A, 0
msgFind2	db	"Finding 2Stage", 0x0D, 0x0A, 0
msgLoadFAT	db	"Loading FAT", 0x0D, 0x0A, 0
msgSuccess	db	"Success", 0x0D, 0x0A, 0
msgFailure	db	"Error", 0x0D, 0x0A, 0
msgProgress db  ".", 0x00

;*************************************************;
; Finish and pad
;*************************************************;

; Pad to 510 bytes (boot sector size minus 2) with 0s, and finish with the two-byte standard boot signature
times 512-($-$$)-2  db 0 
dw 0xAA55	        ; => 0x55 0xAA (little endian byte order)