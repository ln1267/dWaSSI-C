#!/bin/bash
director="/group/y98/nliu/WaSSI_AU/OUTPUTS"
echo $director

#./bin2ascii -i $director/VEGINFO.DAT -o $director/VEGINFO_1.TXT -m 6772800 -n 3
./bin2ascii -i $director/CLIMATE.TXT -o $director/CLIMATE.DAT -m 94819200 -n 5
./ascii2bin -i $director/LANDLAI.TXT -o $director/LANDLAI.DAT -m 94819200 -n 4
./ascii2bin -i $director/SOILINFO.TXT -o $director/SOILINFO.DAT -m 564400 -n 12
./ascii2bin -i $director/CELLINFO.TXT -o $director/CELLINFO.DAT -m 6772800 -n 5
./ascii2bin -i $director/VEGINFO.TXT -o $director/VEGINFO.DAT -m 6772800 -n 3
