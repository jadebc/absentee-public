#!/bin/bash

# Copy utility run script into this folder for concision in call
cp ~/flu-absentee/runFileSaveLogs ~/flu-absentee/1-analysis/3-cdc-seas/

# Run folder scripts and produce output
cd ~/flu-absentee/1-analysis/3-cdc-seas/
./runFileSaveLogs -i "cdc-GLM" 1s-absentee-mean-cdc.R 2s-absentee-poscheck-cdc.R 3as-absentee-p1-glm-unadj-cdc.R 3bs-absentee-p1-glm-adj-cdc.R 4as-absentee-p2-glm-unadj-cdc.R 4bs-absentee-p2-glm-adj-cdc.R 

# Remove copied utility run script
rm runFileSaveLogs
