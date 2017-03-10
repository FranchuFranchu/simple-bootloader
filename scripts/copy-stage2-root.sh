#!/bin/bash

# Andrew Cabey 2017

# Uses https://github.com/qihaiyan/fat12 credit to qihaiyan

# Check if name supplied and file exists
if [ -z "$1" ] || [ -z "$2" ]
then
    echo "Usage: ./copy-stage2-root.sh <FileToCopy.xyz> <FAT12Image.bin>"
    exit 1
else
    # Input filename from arguments
    infile=$1
    outfile=$2
fi

if [ ! -f $infile ]
then
    echo "Error: file $infile not found"
    exit 1
fi

if [ ! ${#infile} -eq 11 ]
then
    echo "Error: FileToCopy must be exactly 11 characters"
    exit 1
fi

if [ ! -f $outfile ]
then
    echo "Error: file $outfile not found"
    exit 1
fi

./dos_cp $outfile $infile a:$infile
exit 0