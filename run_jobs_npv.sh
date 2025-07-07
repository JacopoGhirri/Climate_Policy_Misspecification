
# Define the common parts of the command
base_command="bsub -q p_gams_long -M 100GB -P 0588 -n 36 -R span[ptile=36]"

# discount rates
discrate=(1 2 3 4 5)

for arg1 in "${discrate[@]}"; do
    log_file="logs_npv_${arg1}.txt"
    $base_command -oo  "$log_file" Rscript run_NPVs.R --args $arg1
done

