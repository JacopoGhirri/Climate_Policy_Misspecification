import pandas as pd
import matplotlib.pyplot as plt

# Define file names and titles
csv_files = ["figure_os_ALL_dr_3.csv", "figure_os_NOCLIM_dr_3.csv", "figure_os_NOMOD_dr_3.csv", "figure_os_NODMG_dr_3.csv"]
titles = ["All Uncertainties", "Without Climate Uncertainty", "Without IAMs Uncertainty", "Without Damage Uncertainty"]
tags = ['a)', 'b)', 'c)', 'd)']

# Define colors
color_se =  '#F6AA1C'
color_mm =  '#C46D5E'
color_0 =   '#A3BBAD'
color_1 =   '#357266'
color_2 =   '#0E3B43'

fig, axes = plt.subplots(nrows=2, ncols=2, figsize=(18, 12), sharex=True, sharey=True)

for i, (csv_file, title) in enumerate(zip(csv_files, titles)):
    if(i < 2):
      ax = axes[0][i]
    else:
      ax = axes[1][i-2]

    # Load data
    data = pd.read_csv(csv_file)
    bestcb = data.iloc[0, :]
    data = data.iloc[1:, :]

    # Extract variables
    cb_grid = data['cb.grid']
    bestos_mm = data['bestos.mm']
    bestos_0 = data['bestos.0']
    bestos_1 = data['bestos.1']
    bestos_2 = data['bestos.2']
    bestos_se = data['bestos.se']

    # Best CB values
    bestcb_mm, bestcb_0, bestcb_1, bestcb_2, bestcb_se = bestcb[1:6]

    # Find y-values for bestcb points
    os_mm = bestos_mm[cb_grid == bestcb_mm].values[0]
    os_0 = bestos_0[cb_grid == bestcb_0].values[0]
    os_1 = bestos_1[cb_grid == bestcb_1].values[0]
    os_2 = bestos_2[cb_grid == bestcb_2].values[0]
    os_se = bestos_se[cb_grid == bestcb_se].values[0]

    # Plot main curves
    ax.plot(cb_grid, bestos_mm, label='MaxminEU', color=color_mm, linewidth=2)
    ax.plot(cb_grid, bestos_0, label='Low MoMi', color=color_0, linewidth=2)
    ax.plot(cb_grid, bestos_1, label='Moderate MoMi', color=color_1, linewidth=2)
    ax.plot(cb_grid, bestos_2, label='High MoMi', color=color_2, linewidth=2)
    ax.plot(cb_grid, bestos_se, label='SEU', color=color_se, linewidth=2)

    # Scatter points
    ax.scatter(bestcb_mm, os_mm, color='black', zorder=5, s=50, label='Optimal CB')
    ax.scatter(bestcb_mm, os_mm, color=color_mm, zorder=5, s=50)
    ax.scatter(bestcb_0, os_0, color=color_0, zorder=5, s=50)
    ax.scatter(bestcb_1, os_1, color=color_1, zorder=5, s=50)
    ax.scatter(bestcb_2, os_2, color=color_2, zorder=5, s=50)
    ax.scatter(bestcb_se, os_se, color=color_se, zorder=5, s=50)

    # Titles
    ax.set_title(title, fontsize=14)

    # Grid
    ax.grid()
    ax.set_xlim(750, 1510)
    ax.set_ylim(-2, 200)

    ax.text(750, 205, tags[i], fontsize=14, fontweight='bold')

# Shared labels
axes[1][0].set_xlabel("Carbon Budget - Gt CO2", fontsize=12)
axes[1][1].set_xlabel("Carbon Budget - Gt CO2", fontsize=12)
axes[0][0].set_ylabel("Emission Overshoot - Gt CO2", fontsize=12)
axes[1][0].set_ylabel("Emission Overshoot - Gt CO2", fontsize=12)

# Shared legend on the side
handles, labels = ax.get_legend_handles_labels()
fig.legend(handles, labels, loc='center left', bbox_to_anchor=(1, 0.5), title="Legend", prop={'size': 12})


plt.tight_layout()
plt.savefig('DecModMis_4.pdf', dpi=300, bbox_inches='tight')

plt.show()
