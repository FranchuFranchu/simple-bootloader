#!/bin/bash

# Andrew Cabey 2017

# Check if name supplied and file exists
if [ -z "$1" ] || [ -z "$2" ]
then
    echo "Usage: ./copy-stage1.sh <FileToCopy.xyz> <FAT12Image.bin>"
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

if [ ! -f $outfile ]
then
    echo "Error: file $outfile not found"
    exit 1
fi

dd bs=512 count=1 if=$infile of=$outfile
exit 0