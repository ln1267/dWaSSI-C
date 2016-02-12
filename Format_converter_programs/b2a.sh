#!/bin/bash
directory="/group/y98/nliu/WaSSI_AU/OUTPUTS"
LINE_YEARS=14
LINE_CELLS=564400
let LINE_ANNUAL=$LINE_YEARS*$LINE_CELLS
let LINE_MONTH=$LINE_YEARS*$LINE_CELLS*12
echo $directory $LINE_YEARS $LINE_CELLS $LINE_ANNUAL $LINE_MONTH

#./bin2ascii -i $directory/VEGINFO.DAT -o $directory/VEGINFO_1.TXT -m 6772800 -n 3
./ascii2bin -i $directory/HUCFLOW.DAT -o $directory/HUCFLOW.TXT -m $LINE_CELLS -n 9
./ascii2bin -i $directory/HUCCARBON.DAT -o $directory/HUCCARBON.TXT -m $LINE_CELLS -n 5
./ascii2bin -i $directory/ANNUALFLOW.DAT -o $directory/ANNUALFLOW.TXT -m $LINE_ANNUAL -n 12
./ascii2bin -i $directory/ANNUALCARBON.DAT -o $directory/ANNUALCARBON.TXT -m $LINE_ANNUAL -n 7
./ascii2bin -i $directory/MONTHFLOW.DAT -o $directory/MONTHFLOW.TXT -m $LINE_MONTH -n 13
./ascii2bin -i $directory/MONTHCARBON.DAT -o $directory/MONTHCARBON.TXT -m $LINE_MONTH -n 6
./ascii2bin -i $directory/SOILSTORAGE.DAT -o $directory/SOILSTORAGE.TXT -m $LINE_MONTH -n 8
./ascii2bin -i $directory/VALIDATION.DAT -o $directory/VALIDATION.TXT -m $LINE_MONTH -n 18
