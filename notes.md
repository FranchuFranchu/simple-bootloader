 * When a computer boots, EIP is hardcoded to 0xFFFFFFF0, where the chipset should memory map the BIOS
  * The BIOS copies itself to RAM (shadowing), then jumps from 0xFFFFFFF0 to this copy
 
 # https://osandamalith.com/2015/10/26/writing-a-bootloader/
 * On boot, the BIOS reads the first 512 byte sector of the bootable disk, the Master Boot Record (MBR), and simply it into memory at 0x7c00. The BIOS then jumps to that location to start execution
 * MBR breakdown
  * Code	440 bytes
  * Disk signature	4 bytes
  * Null	2 bytes
  * Partition tables	64 bytes
  * MBR signature	2 bytes
  * Total	512 bytes

 * A single stage bootloader consists of a single file that is loaded entirely by the BIOS. This image then performs the steps described above to start the kernel. However, on the x86 you are usually limited to 512 bytes for a first stage (An exception is no-emulation El-Torito), which is not much. 
 * Real mode:
  * Defaults to 16 bit operands
  * Less than 1 MB of RAM is available
  * Very low-level BIOS API access
  * The memory addressing modes provided are more restrictive than other CPU modes.
  * Accessing more than 64k requires the use of segment register that are difficult to work with
  * All of the 32-bit registers (EAX, ...) are still usable, by simply adding the "Operand Size Override Prefix" (0x66) to the beginning of any instruction. Your assembler is likely to do this for you, if you simply try to use a 32-bit register
  * The bootloader loads the kernel to physical address 0x100000, enabled protected mode then jumps to the kernel
 
 # https://osandamalith.com/2015/10/26/writing-a-bootloader/
 * Interrupts are similar to syscalls, except they interface between bootloader and BIOS rather than userspace and kernel
 * Printing the character 'O' using interrupts
  * mov ah, 0x0e    ; function number = 0Eh : Display Character
  * mov al, 'O'     ; AL = code of character to display
  * int 0x10        ; call INT 10h, BIOS video service
  * 

 # https://www.reinterpretcast.com/creating-a-bare-bones-bootloader  
 * When we set up a stack, we want to place it just after where the 512 byte MBR is loaded. 
  * Because in assembly segments refer to 64k chunks, we determine the actual start value of the stack with { Effective Address = Segment*16 + Offset } 
  * This becomes
   * mov ax, 07C0h   ; Set 'ax' equal to the location of this bootloader divided by 16
   * add ax, 20h     ; Skip over the size of the bootloader divided by 16
   * mov ss, ax      ; Set 'ss' to this location (the beginning of our stack region)
  * Because the stack grows upwards and the stack pointer is actually an offset of the Stack Segment (SS) register, we have to move it down
   * mov sp, 4096

 * The data segment is normally used to access more than 64k of memory, but since the bootloader itself is only 512b, we just set the data segment register to the start of the bootloader
  * mov ax, 07C0h   ; Set 'ax' equal to the location of this bootloader divided by 16
  * mov ds, ax      ; Set 'ds' to the this location

 * In NASM syntax, $ is used to refer to the current address and $$ is used to refer to the address of the start of current section
  * You can use inline Intel syntax with GCC by compiling with the -masm=intel flag
 
 # http://nairobi-embedded.org/qemu_monitor_console.html
  * To run the qemu emulator without SDL or ncurses and instead multiplex your own tty
   * qemu -nographic -serial mon:stdio -append 'console=ttyS0' binary.img
   * To switch tty: ctrl-A + C + ENTER
  * Running with curses actually works better
   * qemu-system-i386 -fda floppy.flp -curses
   * To switch: ESC 2 for console, ESC 1 for curses
 
 # http://www.brokenthorn.com/Resources/OSDev0.html
 * Start to finish OSDev resource
 * 
 
 * Make a floppy disk image
  * dd bs=512 count=2880 if=/dev/zero of=imagefile.img
 * Format to FAT12
  * mkfs.msdos imagefile.img
  * mkfs.msdos -f 2 -h 0 -i 1245678 -F 12 -n MY_FIRST_OS -R 1 -s 1 -r 32 image.bin
 * Mount
  * sudo mkdir /media/floppy1/
  * sudo mount -o loop imagefile.img /media/floppy1/
 * Copy SYS file
  * 
  
 # http://www.brokenthorn.com/Resources/OSDev6.html
 * Coming up on loading the second stage from the FAT filesystem
 * First we need to get to the root directory
  * The general strategy is to 
   * Seek past the File Allocation Tables (2) and the reserved sectors to reach the root directory,
    * First sector is the bootsector
    * Desired start sector = 1 + ReservedSectors + NumberofFATS*SectorsPerFat
   * Load the root directory table
    * Each entry is 32 bytes, 32 * NumRootEntries (in our case 224) = 7168b
    * Total bytes (7168b) / BytesPerSector = Number of sectors we need to load into memory (14)
   * Search through every file until finding the second stage bootloader
    * For every entry in the table, compare every 32 bytes to the known string
    * If it is found, bytes 26-27 offset from this entry's start address store the first cluster
    * bytes 28-32 store the file size
  * Use Logical Block Addressing (LBA) with the cluster start to convert to Cylinder/Head/Sector
   * LBA = (cluster - 2) * SectorsPerCluster
   * Number of sides = number of heads
   * Absolute sector = (LBA % SectorsPerTrack) + 1
   * Absolute head = (LBA / SectorsPerTrack) % Sides
   * Absolute track = LBA / (SectorsPerTrack * Sides)

 # http://x86.renejeschke.de/html/file_module_x86_id_279.html
 # https://www.aldeid.com/wiki/X86-assembly/Instructions/cmpsb
 * repe
  * Repeat while equal (ZF == 0)
 * cmpsb
  * Compare memory contents (byte) at DS:SI to DS:DI
  * If there is a mismatch, ZF is cleared
  * If they remain equal, ZF will not be 0
 
 # http://stackoverflow.com/questions/8021772/assembly-language-how-to-do-modulo
 * When dividing in x86, the operation is performed on AX (AX stores quotiant) and the remainder stored in DX
 
 # http://stackoverflow.com/questions/31301735/toy-os-filesystem
 * Goals for a toy OS and references for file system
 # http://wiki.osdev.org/VFAT
 * FAT Reference
 # http://www.win.tue.nl/~aeb/linux/fs/fat/fat-1.html
 * FAT Reference
 
 # http://forum.osdev.org/viewtopic.php?p=86997
 * The general strategy is to make a FAT disk, dd the boot sector and mount in order to transfer the second stage to root
   * # https://github.com/qihaiyan/fat12
   * Use this guy's FAT12 implementation
   * 