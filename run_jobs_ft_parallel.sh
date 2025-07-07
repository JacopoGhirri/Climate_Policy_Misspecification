base_command="bsub -q p_gams_long -P 0588 -n 2 -M 27GB -R '{span[ptile=2]}'"

# Define the lists of values for the two arguments
from_list=(750  790  820  850  880  910  940  970 1000 1030 1060 1090 1120 1150 1180 1210 1240 1270 1300 1330 1360 1390 1420 1450 1480 1510 1540 1570 1600 1630 1660 1690 1720 1750 1780 1810 1840 1870 1900 1930 1960 1990)
to_list=(780  810  840  870  900  930  960  990 1020 1050 1080 1110 1140 1170 1200 1230 1260 1290 1320 1350 1380 1410 1440 1470 1500 1530 1560 1590 1620 1650 1680 1710 1740 1770 1800 1830 1860 1890 1920 1950 1980 2000)

# Iterate over the combinations of the two lists and execute the commands
for i in "${!from_list[@]}"; do
  arg1=${from_list[$i]}
  arg2=${to_list[$i]}
  log_file="logs_${arg1}_${arg2}.txt"
  $base_command -oo "$log_file" Rscript run_ft.R --args "$arg1" "$arg2"
done
