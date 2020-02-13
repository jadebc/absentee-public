#!/bin/bash

# Copy utility run script into this folder for concision in call
cp ~/flu-absentee/runFileSaveLogs ~/flu-absentee/1-analysis/1-primary/

# Run folder scripts and produce output
cd ~/flu-absentee/1-analysis/1-primary/
./runFileSaveLogs -i "primary-GLM" 1-absentee-mean.R 2-absentee-poscheck.R 3a-absentee-p1-glm-unadj.R 3b-absentee-p1-glm-adj.R 4a-absentee-p2-glm-unadj.R 4b-absentee-p2-glm-adj.R 4c-absentee-p2-glm-adj-grade.R 4d-absentee-p2-glm-adj-month.R 4e-absentee-p2-glm-adj-race.R  5-absentee-p3.R 6-total-absences.R

# Remove copied utility run script
rm runFileSaveLogs
