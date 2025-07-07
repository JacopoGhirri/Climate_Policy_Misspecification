import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
from matplotlib.colors import to_rgba

# Customization at the top
model_colors_A_B = ['red', 'blue', 'green', 'purple', 'orange']  # For Panels A and B
model_colors_C_D = ['blue', 'orange']  # For Panels C and D
SMs_A_B = ['AIM', 'GEM', 'MESSAGE', 'REMIND', 'WITCH']  # Models for A and B
SMs_C_D = ['BHM', 'HSTOT']  # Models for C and D

# Create figure and axes
fig, axes = plt.subplots(2, 2, figsize=(18, 12), constrained_layout=True)
axes = axes.flatten()

### Panel A: Impact of Carbon Budget on Mitigation Costs
cb_grid = np.arange(750, 2010, 10)
plotter_A = pd.read_csv('figure2plotter_A.csv')

#data_A: for scatterplot of IAM scenarios
data_A = pd.read_csv('data_A.csv')
dsm_A = data_A['dsm']#model
dcb_A = data_A['dcb']#carbon budget
dmc_A = data_A['dmc']#mitigation costs NPV

axes[0].plot(cb_grid, plotter_A['AIM_mean'], color='black', linewidth=2)
for i, model in enumerate(SMs_A_B):
    axes[0].fill_between(cb_grid, plotter_A[f'{model}_low'], plotter_A[f'{model}_high'],
                         color=to_rgba(model_colors_A_B[i], alpha=0.25))
    axes[0].plot(cb_grid, plotter_A[f'{model}_mean'], color=model_colors_A_B[i], linewidth=2)
    indices = np.where(dsm_A == model)[0]
    axes[0].scatter(dcb_A.iloc[indices], dmc_A.iloc[indices], color=model_colors_A_B[i])

axes[0].set_title('Costs of Mitigation by Carbon Budget')
axes[0].set_xlabel('Gt CO2')
axes[0].set_ylabel('% of baseline')

legend_A = [
    Line2D([0], [0], color='black', linewidth=2, label='Mean'),
    Patch(facecolor='gray', alpha=0.5, label='90% CI'),
    Line2D([0], [0], marker='o', color='w', markerfacecolor='black', markersize=8, label='IAM Scenario'),
]
for i, model in enumerate(SMs_A_B):
    legend_A.append(Line2D([0], [0], color=model_colors_A_B[i], linewidth=2, label=model))
axes[0].legend(handles=legend_A, loc='upper right', frameon=False)

### Panel B: Impact of Emission Overshoot on Mitigation Costs
os_grid = np.arange(0, 210, 10)
plotter_B = pd.read_csv('figure2bplotter_ratio_v3.csv')

axes[1].plot(os_grid, plotter_B['AIM_mean'], color='black', linewidth=2)
for i, model in enumerate(SMs_A_B):
    axes[1].fill_between(os_grid, plotter_B[f'{model}_low'], plotter_B[f'{model}_high'],
                         color=to_rgba(model_colors_A_B[i], alpha=0.25))
    axes[1].plot(os_grid, plotter_B[f'{model}_mean'], color=model_colors_A_B[i], linewidth=2)

axes[1].set_title('Costs of Mitigation by Emission Overshoot')
axes[1].set_xlabel('Gt CO2')
axes[1].set_ylabel('Costs at 2080 over Costs at 2040')

legend_B = [
    Line2D([0], [0], color='black', linewidth=2, label='Mean'),
    Patch(facecolor='gray', alpha=0.5, label='90% CI'),
]
for i, model in enumerate(SMs_A_B):
    legend_B.append(Line2D([0], [0], color=model_colors_A_B[i], linewidth=2, label=model))
axes[1].legend(handles=legend_B, loc='upper left', frameon=False)

### Panel C: Impact of Carbon Budget on Climate Damages
plotter_C = pd.read_csv('figure2plotter_C_eocdamage_0.csv')

axes[2].plot(cb_grid, plotter_C['BHM_mean'], color='black', linewidth=2)
for i, model in enumerate(SMs_C_D):
    axes[2].fill_between(cb_grid, plotter_C[f'{model}_low'], plotter_C[f'{model}_high'],
                         color=to_rgba(model_colors_C_D[i], alpha=0.5))
    axes[2].plot(cb_grid, plotter_C[f'{model}_mean'], color=model_colors_C_D[i], linewidth=2)

axes[2].set_title('Climate Damages at 2100 by Carbon Budget')
axes[2].set_xlabel('Gt CO2')
axes[2].set_ylabel('% of baseline')

legend_C = [
    Line2D([0], [0], color='black', linewidth=2, label='Mean'),
    Patch(facecolor='gray', alpha=0.5, label='90% CI'),
]
for i, model in enumerate(['Growth', 'Level']):#enumerate(SMs_C_D):
    legend_C.append(Line2D([0], [0], color=model_colors_C_D[i], linewidth=2, label=model))
axes[2].legend(handles=legend_C, loc='upper left', frameon=False)

### Panel D: New Panel
file_path_D = "figure2d_NZ_EOC.csv"
df_D = pd.read_csv(file_path_D)
df_melted_D = df_D.melt(id_vars=["dmg", "scen"], var_name="Year", value_name="Damage")
df_melted_D["Year"] = df_melted_D["Year"].astype(int)

summary_stats_D = (
    df_melted_D.groupby(["dmg", "scen", "Year"])
    .agg(
        mean_damage=("Damage", "mean"),
        lower_ci=("Damage", lambda x: np.percentile(x, 5)),
        upper_ci=("Damage", lambda x: np.percentile(x, 95))
    )
    .reset_index()
)

color_palettes = {
    "BHM": sns.color_palette(model_colors_C_D[0].capitalize() + 's', 2),  # Shades for BHM (Scenario 1 and Scenario 2)
    "HSTOT": sns.color_palette(model_colors_C_D[1].capitalize() + 's', 2),  # Shades for HSTOT (Scenario 1 and Scenario 2)
}

for dmg_type in summary_stats_D["dmg"].unique():
    subset = summary_stats_D[summary_stats_D["dmg"] == dmg_type]
    palette = color_palettes[dmg_type]

    sns.lineplot(data=subset, x="Year", y="mean_damage", hue="scen", palette=palette, linewidth=2, ax=axes[3])
    for scen in subset["scen"].unique():
        scen_data = subset[subset["scen"] == scen]
        axes[3].fill_between(
            scen_data["Year"], scen_data["lower_ci"], scen_data["upper_ci"],
            alpha=0.2, color=palette[0] if scen == subset["scen"].unique()[0] else palette[1]
        )

axes[3].set_title('Climate Damages by Emission Overshoot scenarios')
axes[3].set_xlabel('Year')
axes[3].set_ylabel('% of Baseline')
axes[3].grid(True)
axes[3].set_yticklabels([f'{int(tick * 100)}' for tick in axes[3].get_yticks()])

custom_legend = [
    # Mean line
    Line2D([0], [0], color='black', linewidth=2, label="Mean"),
    # Confidence interval
    Patch(facecolor='gray', alpha=0.2, label="90% CI"),
    # Damage-specific colors with scenarios
    Patch(facecolor=color_palettes["BHM"][0], alpha=0.8, label="Growth - High EO"),
    Patch(facecolor=color_palettes["BHM"][1], alpha=0.8, label="Growth - Low EO"),
    Patch(facecolor=color_palettes["HSTOT"][0], alpha=0.8, label="Level - High EO"),
    Patch(facecolor=color_palettes["HSTOT"][1], alpha=0.8, label="Level - Low EO"),
]
axes[3].legend(handles=custom_legend, loc='upper left', frameon=False)

# Add panel labels
panel_labels = ['(a)', '(b)', '(c)', '(d)']
for ax, label in zip(axes, panel_labels):
    ax.annotate(label, xy=(0, 1), xytext=(-20, 10),
                textcoords='offset points', fontsize=14, fontweight='bold',
                ha='right', va='bottom', xycoords='axes fraction')
plt.savefig('DecModMis_2.pdf', dpi=300, bbox_inches='tight')
plt.show()
