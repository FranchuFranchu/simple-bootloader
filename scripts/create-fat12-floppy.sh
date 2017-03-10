#!/bin/bash

# Andrew Cabey 2017
# Output filename from arguments
outfile=$1

# Check if name supplied
if [ -z "$1" ]
  then
    echo "Usage: ./create-fat12-floppy <Outfile.bin>"
  else
    dd bs=512 count=2880 if=/dev/zero of=$outfile
    mkfs.msdos $outfile
fi