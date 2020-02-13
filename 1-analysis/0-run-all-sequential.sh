
#!/bin/bash

cd ~/flu-absentee/1-analysis

./1-primary/0-run-primary.sh
./2-negcontrol/0-run-negcontrol.sh
./3-cdc-seas/0-run-cdc.sh
./4-peakwk/0-run-peakwk.sh
./5-cdph-alt/0-run-cdph-alt.sh