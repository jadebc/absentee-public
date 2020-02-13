#!/bin/bash

# Copy utility run script into this folder for concision in call
cp ~/flu-absentee/runFileSaveLogs ~/flu-absentee/1-analysis/4-peakwk/

# Run folder scripts and produce output
cd ~/flu-absentee/1-analysis/4-peakwk/
./runFileSaveLogs -i "peakwk-GLM" 1p-absentee-mean-peakwk.R 2p-absentee-poscheck-peakwk.R 3ap-absentee-p1-glm-unadj-peakwk.R 3bp-absentee-p1-glm-adj-peakwk.R 4ap-absentee-p2-glm-unadj-peakwk.R 4bp-absentee-p2-glm-adj-peakwk.R

# Remove copied utility run script
rm runFileSaveLogs
