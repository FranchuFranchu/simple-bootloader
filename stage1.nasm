;**********************************************************************************;
; Setup
;**********************************************************************************;

	;----------------------------------------------------
    ; Constants
	;----------------------------------------------------
    %include    constants.nasm

	;----------------------------------------------------
    ; Assembler Directives
	;----------------------------------------------------
    org         STAGE1_ENTRY
    bits        16
    
;**********************************************************************************;

;**********************************************************************************;
; Begin Stage 1 
;**********************************************************************************;

	;----------------------------------------------------
    ; BIOS Parameter Block
    ;----------------------------------------------------
    %include    oemblock.nasm

;*************************************************;
; Jump to true start
;*************************************************;
jmp         _start

%include    helper.nasm

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

%include    pad.nasm