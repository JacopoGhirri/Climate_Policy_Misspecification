# Define the common parts of the command
base_command="bsub -q p_gams_long -M 50GB -P 0588 -n 9 -R span[ptile=9]"

# Define the lists of values for the two arguments
discrate=(1 2 3 4 5) #discount rates for NPV
dmids=(1 2 3) #IDS for damage functions
unids=(1 2 3 4) #IDS for uncertainty
# Iterate over the combinations of the lists and execute the commands
for dr in "${discrate[@]}"; do
  for did in "${dmids[@]}"; do
    for uni in "${unids[@]}"; do
      log_file="logs_miss_${dr}_${did}.txt"
      $base_command -oo  "$log_file" Rscript run_maxmin_momi.R --args $dr $did $uni
    done
  done
done


