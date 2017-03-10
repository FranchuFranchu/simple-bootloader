;**********************************************************************************;
; Define Constants
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
;**********************************************************************************;