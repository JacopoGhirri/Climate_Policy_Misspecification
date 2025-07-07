import pandas as pd
import matplotlib.pyplot as plt

# File names for the five different datasets
file_names = [
    'figure_os_NOAIM_ALL_dr_1.csv',
    'figure_os_NOAIM_ALL_dr_2.csv',
    'figure_os_NOAIM_ALL_dr_3.csv',
    'figure_os_NOAIM_ALL_dr_4.csv',
    'figure_os_NOAIM_ALL_dr_5.csv'
]

discount_rates = ["1% DR", "2% DR", "3% DR", "4% DR", "5% DR"]

# Define colors
color_se =  '#F6AA1C'
color_mm =  '#C46D5E'
color_0 =   '#A3BBAD'
color_1 =   '#357266'
color_2 =   '#0E3B43'

fig, axes = plt.subplots(3, 2, figsize=(16, 18))
plt.rcParams.update({'font.size': 14})
axes = axes.flatten()

for i, (file, dr_label) in enumerate(zip(file_names, discount_rates)):
    data = pd.read_csv(file)
    bestcb = data.iloc[0, :]
    cb_grid = data['cb.grid']

    bestos_mm = data['bestos.mm']
    bestos_0 = data['bestos.0']
    bestos_1 = data['bestos.1']
    bestos_2 = data['bestos.2']
    bestos_se = data['bestos.se']
    bestos_15 = data['bestos.15']

    bestcb_mm = bestcb[1]
    bestcb_0 = bestcb[2]
    bestcb_1 = bestcb[3]
    bestcb_15 = bestcb[4]
    bestcb_2 = bestcb[5]
    bestcb_se = bestcb[6]

    os_mm = bestos_mm[cb_grid == bestcb_mm].values[0]
    os_0 = bestos_0[cb_grid == bestcb_0].values[0]
    os_1 = bestos_1[cb_grid == bestcb_1].values[0]
    os_2 = bestos_2[cb_grid == bestcb_2].values[0]
    os_se = bestos_se[cb_grid == bestcb_se].values[0]
    os_15 = bestos_15[cb_grid == bestcb_15].values[0]

    ax = axes[i]
    ax.plot(cb_grid, bestos_mm, label='MaxminEU', color=color_mm, linewidth=2)
    ax.plot(cb_grid, bestos_0, label='Low MoMi', color=color_0, linewidth=2)
    ax.plot(cb_grid, bestos_1, label='Moderate MoMi', color=color_1, linewidth=2)
    ax.plot(cb_grid, bestos_2, label='High MoMi', color=color_2, linewidth=2)
    ax.plot(cb_grid, bestos_se, label='SEU', color=color_se, linewidth=2)

    ax.scatter(bestcb_mm, os_mm, color='black', zorder=5, s=50, label='Optimal CB')
    ax.scatter(bestcb_mm, os_mm, color=color_mm, zorder=5, s=50)
    ax.scatter(bestcb_0, os_0, color=color_0, zorder=5, s=50)
    ax.scatter(bestcb_1, os_1, color=color_1, zorder=5, s=50)
    ax.scatter(bestcb_2, os_2, color=color_2, zorder=5, s=50)
    ax.scatter(bestcb_se, os_se, color=color_se, zorder=5, s=50)

    ax.set_xlim(750, 1550)
    ax.set_ylim(-5, 200)
    ax.set_xlabel('Carbon Budget - Gt CO2')
    ax.set_ylabel('Emission Overshoot - Gt CO2')
    ax.grid()
    ax.text(740, 205, f'{chr(97 + i)})   {dr_label}', fontsize=14, fontweight='bold')

# Use the last subplot for the legend
axes[-1].axis('off')  # Hide the empty subplot
handles, labels = axes[0].get_legend_handles_labels()
axes[-1].legend(handles, labels, loc='center', title='Legend', fontsize=14)

plt.tight_layout()

plt.savefig('DecModMis_6.pdf', dpi=300, bbox_inches='tight')
plt.show()
