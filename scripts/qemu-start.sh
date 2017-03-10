# https://osandamalith.com/2015/10/26/writing-a-bootloader/
# qemu -nographic -serial mon:stdio -append 'console=ttyS0' binary.img
# qemu-system-i386 -nographic -serial mon:stdio -append 'console=ttyS0' -fda floppy.flp

# Press ESC 2 to reach qemu console
qemu-system-i386 -fda floppy.flp -curses