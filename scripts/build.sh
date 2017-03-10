# !/bin/bash

# Clean any files
./clean.sh

outfile=image.bin
# Build new FAT12 floppy image
dd bs=512 count=2880 if=/dev/zero of=$outfile

# Format to FAT12
mkfs.msdos $outfile

# Assemble Stage 1
nasm -f bin -o stage1.bin stage1.nasm

# Copy Stage 1 boot sector to image
dd bs=512 count=1 if=stage1.bin of=$outfile

# Assemble stage 2
nasm -f bin -o stage2.bin stage2.nasm 

# Copy stage 2 file to filesystem
./dos_cp $outfile stage2.bin a:KRNLLODR.SYS

# Move image to parent
mv $outfile ../

# Clean
./clean.sh