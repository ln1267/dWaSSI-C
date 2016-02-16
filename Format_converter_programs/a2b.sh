#!/bin/bash
#directory="/group/y98/nliu/WaSSI_AU/INPUTS"
directory="/scratch/director1234/nliu/MJ/INPUTS"
LINE_YEARS=15
LINE_CELLS=47320
let LINE_ANNUAL=$LINE_YEARS*$LINE_CELLS
let LINE_MONTH=$LINE_YEARS*$LINE_CELLS*12
echo $directory $LINE_YEARS $LINE_CELLS $LINE_ANNUAL $LINE_MONTH

echo $directory

#./bin2ascii -i $directory/VEGINFO.DAT -o $directory/VEGINFO_1.TXT -m 6772800 -n 3
./ascii2bin -i $directory/CLIMATE.TXT -o $directory/CLIMATE.DAT -m $LINE_MONTH -n 5
./ascii2bin -i $directory/LANDLAI.TXT -o $directory/LANDLAI.DAT -m $LINE_MONTH -n 4
./ascii2bin -i $directory/SOILINFO.TXT -o $directory/SOILINFO.DAT -m $LINE_CELLS -n 12
./ascii2bin -i $directory/CELLINFO.TXT -o $directory/CELLINFO.DAT -m $LINE_CELLS -n 5
./ascii2bin -i $directory/VEGINFO.TXT -o $directory/VEGINFO.DAT -m 567840 -n 3
