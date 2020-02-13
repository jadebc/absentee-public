#!/bin/bash

# Copy utility run script into this folder for concision in call
cp ~/flu-absentee/runFileSaveLogs ~/flu-absentee/4-figures

# Run folder scripts and produce output
cd ~/flu-absentee/4-figures
./runFileSaveLogs -i "Figures" fig-bias-analysis.R fig-bias-distributions.R fig-p2-glm-adj.R fig-p2-glm-adj-grade.R fig-p2-glm-adj-race.R fig-p2-glm-adj-month.R fig-p2-glm-all.R fig-p3.R fig-rates-daily.R fig-seas-cdph.R tab-absentee_results.R

# Remove copied utility run script
rm runFileSaveLogs