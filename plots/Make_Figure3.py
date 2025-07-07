import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Load the data from the CSV file
data = pd.read_csv('figure_os_ALL_dr_3.csv')

color_se =  '#F6AA1C'
color_mm =  '#C46D5E'
color_0 =   '#A3BBAD'
color_1 =   '#357266'
color_2 =   '#0E3B43'

bestcb = data.iloc[0, :]
data = data.iloc[1:, :]

cb_grid = data['cb.grid']
bestos_mm = data['bestos.mm']
bestos_0 = data['bestos.0']
bestos_1 = data['bestos.1']
bestos_2 = data['bestos.2']
bestos_se = data['bestos.se']

bestcb_mm = bestcb[1]
bestcb_0 = bestcb[2]
bestcb_1 = bestcb[3]
bestcb_2 = bestcb[4]
bestcb_se = bestcb[5]

os_mm = bestos_mm[cb_grid == bestcb_mm].values[0]
os_0 = bestos_0[cb_grid == bestcb_0].values[0]
os_1 = bestos_1[cb_grid == bestcb_1].values[0]
os_2 = bestos_2[cb_grid == bestcb_2].values[0]
os_se = bestos_se[cb_grid == bestcb_se].values[0]

def budget_to_temp(emi, quantile="expected"):
    if quantile == "expected":
        return emi * 1.8 * 1e-3 / 3.6 + 0.988 + 0.2
    elif quantile == "high":
        return emi * 2.7 * 1e-3 / 3.6 + 0.988 + 0.2

def overshoot_to_temp(emi, quantile="expected"):
    if quantile == "expected":
        return emi * 1.8 * 1e-3 / 3.6
    elif quantile == "high":
        return emi * 2.7 * 1e-3 / 3.6

def cb_to_temp(cb):
    return budget_to_temp(cb, quantile="expected")

def cb_to_temp_high(cb):
    return budget_to_temp(cb, quantile="high")

def os_to_temp(os):
    return overshoot_to_temp(os, quantile="expected")

def os_to_temp_high(os):
    return overshoot_to_temp(os, quantile="high")

fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(16, 12))
plt.rcParams.update({'font.size': 14})

ax1.plot(cb_grid, bestos_mm, label='MaxminEU', color=color_mm, linewidth=2)
ax1.plot(cb_grid, bestos_0, label='Low MoMi', color=color_0, linewidth=2)
ax1.plot(cb_grid, bestos_1, label='Moderate MoMi', color=color_1, linewidth=2)
ax1.plot(cb_grid, bestos_2, label='High MoMi', color=color_2, linewidth=2)
ax1.plot(cb_grid, bestos_se, label='SEU', color=color_se, linewidth=2)

ax1.scatter(bestcb_mm, os_mm, color='black', zorder=5, s=50, label='Optimal CB')
ax1.scatter(bestcb_mm, os_mm, color=color_mm, zorder=5, s=50)
ax1.scatter(bestcb_0, os_0, color=color_0, zorder=5, s=50)
ax1.scatter(bestcb_1, os_1, color=color_1, zorder=5, s=50)
ax1.scatter(bestcb_2, os_2, color=color_2, zorder=5, s=50)
ax1.scatter(bestcb_se, os_se, color=color_se, zorder=5, s=50)

ax1.set_xlim(750, 1550)
ax1.set_ylim(-5, 200)
ax1.set_xlabel('Carbon Budget - Gt CO2')
ax1.set_ylabel('Emission Overshoot - Gt CO2')
ax1.legend(loc='upper right', title='Legend')
ax1.grid()
ax1.text(740, 205, 'a)', fontsize=14, fontweight='bold')

# Adding secondary axes
# Top axes for temperature impacts
secax_x_top_expected = ax1.secondary_xaxis('top', functions=(cb_to_temp, lambda t: (t - 0.988+0.2) / 1.8*1e-3/3.6))
secax_x_top_expected.set_xlabel('50th Percentile Temperature Increase (°C)')
secax_x_top_high = ax1.secondary_xaxis('top', functions=(cb_to_temp_high, lambda t: (t - 0.988+0.2) / 2.7*1e-3/3.6))
secax_x_top_high.set_xlabel('95th Percentile Temperature Increase (°C)')

# Offset the high quantile label to avoid overlap
secax_x_top_high.spines['top'].set_position(('outward', 40))  # Offset by 40 points

# Right axes for overshoot impacts
secax_y_right_expected = ax1.secondary_yaxis('right', functions=(os_to_temp, lambda t: t / 1.8*1e-3/3.6))
secax_y_right_expected.set_ylabel('50th Percentile Temperature Overshoot (°C)')
secax_y_right_high = ax1.secondary_yaxis('right', functions=(os_to_temp_high, lambda t: t / 2.7*1e-3/3.6))
secax_y_right_high.set_ylabel('95th Percentile Temperature Overshoot (°C)')

# Offset the high quantile label to avoid overlap
secax_y_right_high.spines['right'].set_position(('outward', 60))  # Offset by 60 points

color_eo = '#CF5C36'
color_cb = '#141115'
color_nz = '#BBDEF0'
base_alpha = 0.4

generic_fear = np.arange(-1, 6)
optimal_CB = [1090, 1510, 1510, 1420, 1270, 820, 760]
optimal_EO = [10, 0, 0, 0, 0, 40, 50]

ax2.set_ylabel('Optimal Carbon Budget (GtCO2)', color=color_cb)
ax2.plot(generic_fear, optimal_CB, label='Optimal CB', color=color_cb, marker='o', linestyle='-')
ax2.tick_params(axis='y', labelcolor=color_cb)
ax2.spines['bottom'].set_visible(False)
ax2.spines['top'].set_visible(False)
ax2.set_xlim(-1.25, 5.1)

ax2_twin = ax2.twinx()
ax2_twin.set_ylabel('Optimal Emission Overshoot (GtCO2)', color=color_eo)
ax2_twin.plot(generic_fear, optimal_EO, label='Optimal EO', color=color_eo, marker='o', linestyle='-')
ax2_twin.tick_params(axis='y', labelcolor=color_eo)
ax2_twin.spines['bottom'].set_visible(False)
ax2_twin.spines['top'].set_visible(False)

ax2.set_xticks([])
ax2.text(-1.5, 1580, 'b)', fontsize=14, fontweight='bold')

ax2.axvspan(-1.5, -0.5, color=color_nz, alpha=base_alpha*(60/70))
ax2.text(-0.9, 1570, 'Net Zero by 2060', ha='center', va='center', fontsize=10)
ax2.axvspan(-0.5, 2.5, color=color_nz, alpha=base_alpha*(40/80))
ax2.text(1, 1570, 'Net Zero by 2080', ha='center', va='center', fontsize=10)
ax2.axvspan(2.5, 3.5, color=color_nz, alpha=base_alpha*(60/80))
ax2.text(3, 1570, 'Net Zero by 2075', ha='center', va='center', fontsize=10)
ax2.axvspan(3.5, 6, color=color_nz, alpha=base_alpha*(70/70))
ax2.text(4.5, 1570, 'Net Zero by 2050', ha='center', va='center', fontsize=10)

# Add a double-headed arrow for the x-axis (adjust position)
ax2.annotate('', xy=(4/12, -0.007), xytext=(1, -0.007),
             xycoords='axes fraction', textcoords='axes fraction',
             arrowprops=dict(arrowstyle='<->', lw=2, color='black'))

# Add labels for the x-axis

ax2.text(1, 700, 'Low Misspecification Fear', ha='left', va='center', fontsize=12)
ax2.text(5, 700, 'High Misspecification Fear', ha='right', va='center', fontsize=12)
ax2.text(0, 700, 'MaxMinEU', ha='center', va='center', fontsize=12)
ax2.text(-1, 700, 'SEU', ha='center', va='center', fontsize=12)


plt.tight_layout()


plt.savefig('DecModMis_3.pdf', dpi=300, bbox_inches='tight')
plt.show()
