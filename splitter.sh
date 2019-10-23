#!/bin/bash

#https://stackoverflow.com/questions/6951223/finding-unique-values-in-a-data-file

(($# == 2)) || { echo -e "\nUsage: $0 <file to split> <# columns in each split>\n\n"; exit; }

infile="$1"

inc=$2
ncol=$(awk 'NR==1{print NF}' "$infile")

((inc < ncol)) || { echo -e "\nSplit size >= number of columns per file\n\n"; exit; }

for((i=0, start=1, end=$inc; i < ncol/inc + 1; i++, start+=inc, end+=inc)); do
  cut -f$start-$end "$infile" > "${infile}.$i"
done

#take file with first column and find the unique IDs
cat "${infile}".0 | sort | uniq > unique_ids.txt
#mark each unique identifier with "u", a human mark what corresponds to chrom 1,2,3 or "u" (unassigned)
awk '{print $0", u"}' unique_ids.txt > marked_ids.txt
