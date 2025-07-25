# Climate Policy under Fear of Model Misspecication
This project is covered under the MIT License.

This is the code used for the analyses and figures of Ghirri J., Marinacci M., & Tavoni M. _Climate Policy under Fear of Model Misspecication_ DOI: https://doi.org/10.21203/rs.3.rs-6813235/v1

## Requirements
This repository can run on a standard computer, with R > 4.3.0 installed, and with enough memory and RAM to support the required operations. For a full replication of the analysis, at least 1.5 Terabytes of memory are required. Such requirement can be lowered by decreasing the number of Monte Carlo samples, or restricting the scope of the analysis by exploring a smaller range, or a more coarse grid, of Carbon Budgets.

All the scripts are set up to use parallelization where possible. On a high-performance computing (HPC) system with 72+ CPUs @ 2.4 GHz, the full simulation takes approximately 1 week.

## Disclaimer
The scripts present in this repository have been executed on an HPC Cluster. The Shell scripts reflect the original configuration. For local execution one can either adapt the shell scripts accordingly, or directly call the R scripts with the appropriate arguments.

Due to memory constraints, only post-processed results are shared in full. These are sufficient to replicate the figures in the paper.
By launching first the _run_ft.R_ script, passing as two arguments the Carbon Budget range in GigaTons (750 2000 is the one explored in the analysis), followed by _run_NPVs.R_, passing as argument the desired discount rate (1-5% explored in the analysis), it is possible to fully replicate all the simulations used in the study.

In case one were to be interested in just a small scale proof of concept, and not a full replication, we recommend to edit _run_ft.R_, reducing the number of Monte Carlo samples in line 15, being the parameter _n_ passed to function _save.steps(***)_, easing the memory requirements and the computation time needed.

## Structure
- **data**: containing original IAMs scenarios, temperature, population, and gdp data, together with *BDD2018* and *BurkeHsiangMiguel2015_Replication*, used to extract bootstrap samples of the damage function parameters, and postprocessing scripts.
- **R**: containing the scripts used for the analysis.
- **results**: used for storing the Monte Carlo sampled GDP curves, net of costs of mitigation and climate damages.
- **NPVS**: used for storing the NPV distribution processing of the samples in *results*.
- **final_results**: storing the post-processed final evaluations of the decision making criteria.
- **final_results_NOAIM**: as in *final_results*, but without the evaluation of the AIM based simulations, for sensitivity in Figure 6.
- **plots**: containing the R scripts used to process the final results for the different visualizations, the respective csvs, and the python scripts generating the pdf figures.
- _run_ft.R_: receiving as arguments two Carbon Budgets, computes the Monte Carlo samples of GDP curves in the selected range.
- _run_NPVs.R_: receiving as argument a discount rate (as percentage value), computes the Net Present Value of the GDP curves.
- _SEU_RUN.R_: receiving as argument a discount rate, and ID for the desired damage configuration, and an ID for the uncertainty configuration, evaluates the climate target space according to the Subjective Expected Utility criterion.
- _run_maxmin_momi.R_: receiving as argument a discount rate, and ID for the desired damage configuration, and an ID for the uncertainty configuration, evaluates the climate target space according to MaxMin Expected Utility criterion, and different levels of Model Misspecification fear.
