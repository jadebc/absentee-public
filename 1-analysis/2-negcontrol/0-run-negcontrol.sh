#!/bin/bash

# Copy utility run script into this folder for concision in call
cp ~/flu-absentee/runFileSaveLogs ~/flu-absentee/1-analysis/2-negcontrol/

# Run folder scripts and produce output
cd ~/flu-absentee/1-analysis/2-negcontrol/
./runFileSaveLogs -i "negcontrol-GLM" 1n-absentee-mean-negc.R 2n-absentee-poscheck.R 3an-absentee-p1-glm-unadj-negc.R 3bn-absentee-p1-glm-adj-negc.R 4an-absentee-p2-glm-unadj-negc.R 4bn-absentee-p2-glm-adj-negc.R 4cn-absentee-p2-glm-adj-negc-month.R

# Remove copied utility run script
rm runFileSaveLogs
