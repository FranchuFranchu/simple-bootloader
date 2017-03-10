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