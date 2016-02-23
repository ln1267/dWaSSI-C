#!/bin/bash
LINE_YEARS=15
#directory="/group/y98/nliu/WaSSI_AU/OUTPUTS"

directory="$MYGROUP/WaSSI/output"
LINE_CELLS=47320
let LINE_ANNUAL=$LINE_YEARS*$LINE_CELLS
let LINE_MONTH=$LINE_YEARS*$LINE_CELLS*12
echo $directory $LINE_YEARS $LINE_CELLS $LINE_ANNUAL $LINE_MONTH

#./bin2ascii -i $directory/VEGINFO.DAT -o $directory/VEGINFO_1.TXT -m 6772800 -n 3
./bin2ascii -i "$directory/HUCFLOW.DAT" -o "$directory/HUCFLOW.TXT" -m $LINE_CELLS -n 9
./bin2ascii -i "$directory/HUCCARBON.DAT" -o "$directory/HUCCARBON.TXT" -m $LINE_CELLS -n 5
./bin2ascii -i "$directory/ANNUALFLOW.DAT" -o "$directory/ANNUALFLOW.TXT" -m $LINE_ANNUAL -n 12
./bin2ascii -i "$directory/ANNUALCARBON.DAT" -o "$directory/ANNUALCARBON.TXT" -m $LINE_ANNUAL -n 5 
./bin2ascii -i "$directory/MONTHFLOW.DAT" -o "$directory/MONTHFLOW.TXT" -m $LINE_MONTH -n 13
./bin2ascii -i "$directory/MONTHCARBON.DAT" -o "$directory/MONTHCARBON.TXT" -m $LINE_MONTH -n 6
./bin2ascii -i "$directory/SOILSTORAGE.DAT" -o "$directory/SOILSTORAGE.TXT" -m $LINE_MONTH -n 8
./bin2ascii -i "$directory/VALIDATION.DAT" -o "$directory/VALIDATION.TXT" -m $LINE_MONTH -n 18
