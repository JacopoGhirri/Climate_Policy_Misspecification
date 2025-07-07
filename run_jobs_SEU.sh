#!/bin/bash

# Define the common parts of the command
base_command="bsub -q p_gams_long -M 50GB -P 0588 -n 9 -R span[ptile=9]"

discrate=(1 2 3 4 5) # discount rates for NPVs
dmids=(1 2 3) #IDS for damage functions
unids=(1 2 3 4) #IDS for uncertainty

for dr in "${discrate[@]}"; do
  for did in "${dmids[@]}"; do
    for uid in "${unids[@]}"; do
      log_file="logs_SEU_${dr}_${did}_${uid}.txt"
      $base_command -oo  "$log_file" Rscript SEU_RUN.R --args $dr $did $uid
    done
  done
done

