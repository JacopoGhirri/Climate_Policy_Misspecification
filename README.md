# Climate Policy under Fear of Model Misspecication

This is the code used for the analyses and figures of Ghirri J., Marinacci M., & Tavoni M. _Climate Policy under Fear of Model Misspecication_ DOI: https://doi.org/10.21203/rs.3.rs-6813235/v1

## DISCLAIMERS
The scripts present in this repository have been executed on an HPC Cluster. The Shell scripts reflect the original configuration, for local execution one can either adapt them to work on the local machine, or directly plug in the appropriate arguments on the core R scripts.

Due to the high requirements (>1Tb of memory) of the full set of Monte Carlo samples, only the post processed data are reported in full, as it suffice to easily replicate the figures in the paper. 
By launching first the _run_ft.R_ script, passing as arguments the Carbon Budget range (750-2000 is the one explored in the analysis), followed by _run_NPVs.R_, passing as argument the desired discount rate (1-5% explored in the analysis), it is possible to fully replicate the study.

In case one were to be interested in just a proof of concept, and not a full replication, we recommend to edit _run_ft.R_, reducing the number of Monte Carlo samples in line 15, being the parameter _n_ passed to function _save.steps(***)_, easing the memory requirements and the computation time needed.

## STRUCTURE
- **data**: containing original IAMs scenarios, temperature, population, and gdp data, together with *BDD2018* and *BurkeHsiangMiguel2015_Replication*, used to extract bootstrap samples of the damage function parameters, and postprocessing scripts.
- **R**: containing the scripts used for the analysis.
- **results**: used for storing the Monte Carlo samples of GDP curves, used in the analysis.
- **NPVS**: used for storing the NPV distribution processing of the samples in *results*.
- **final_results**: storing the post-processed final evaluations of the decision making criteria.
- **final_results_NOAIM**: as in *final_results*, but without the evaluation of the AIM IAM, for sensitivity in Figure 6.
- **plots**: containing the R scripts used to process the final results for the different visualizations, the respective csvs, and the python scripts generating the pdf figures.
- _run_ft.R_: receiving as arguments two Carbon Budgets, computes the Monte Carlo samples of GDP curves in the selected range.
- _run_NPVs.R_: receiving as argument a discount rate (as percentage value), computes the Net Present Value of the GDP curves.
- _SEU_RUN.R_: receiving as argument a discount rate, and ID for the desired damage configuration, and an ID for the uncertainty configuration, evaluates the climate target space according to the Subjective Expected Utility criterion.
- _run_maxmin_momi.R: receiving as argument a discount rate, and ID for the desired damage configuration, and an ID for the uncertainty configuration, evaluates the climate target space according to MaxMin Expected Utility criterion, and different levels of Model Misspecification fear.
