# https://osandamalith.com/2015/10/26/writing-a-bootloader/

nasm -f bin -o bootloader.bin bootloader.nasm
nasm -f bin -o KRNLLDR.SYS stage2.nasm