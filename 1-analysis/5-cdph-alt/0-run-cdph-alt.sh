#!/bin/bash

# Copy utility run script into this folder for concision in call
cp ~/flu-absentee/runFileSaveLogs ~/flu-absentee/1-analysis/5-cdph-alt/

# Run folder scripts and produce output
cd ~/flu-absentee/1-analysis/5-cdph-alt/
./runFileSaveLogs -i "cdph-GLM" 1a-absentee-mean-cdph2.R 1b-absentee-mean-cdph3.R 4a-absentee-p2-glm-unadj-cdph2.R 4a-absentee-p2-glm-unadj-cdph3.R 4b-absentee-p2-glm-adj-cdph2.R 4b-absentee-p2-glm-adj-cdph3.R  

# Remove copied utility run script
rm runFileSaveLogs