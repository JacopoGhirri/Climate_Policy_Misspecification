#Data for figure2 panel D
source("R/AAA_packages.R")
source("R/AAA_functions.R")

n <- 50000

set.seed(17021890)
datablock <- get.data()
formulas <- read.formulas()
models <- fit.models(data = datablock, formula.miticost = formulas$mitcost, formula.emi = formulas$emi, plot = F)
par.BHM <- get.BHM.par()

baseline <- read.csv('data/baseline_world.csv')[,2]
#low overshoot scenario
cbl <- predict.cumemi.MC(models$model.cumemi$WITCH, 750, 750, n)

#high overshoot scenario
cbh <- predict.cumemi.MC(models$model.cumemi$WITCH, 750, 950, n)

tel <- get_temp(cbl, 1.9)
teh <- get_temp(cbh, 1.9)

colnames(tel) <- models$y.int
colnames(teh) <- models$y.int

dmb_l <- compute.climate.impact(tel, 'BHM', models$y.int, baseline, par.BHM, mean.par = F, seed = 17021890)
dmb_h <- compute.climate.impact(teh, 'BHM', models$y.int, baseline, par.BHM, mean.par = F, seed = 17021890)
dmh_l <- compute.climate.impact(tel, 'HSTOT', models$y.int, baseline, par.BHM, mean.par = F, seed = 17021890)
dmh_h <- compute.climate.impact(teh, 'HSTOT', models$y.int, baseline, par.BHM, mean.par = F, seed = 17021890)

dmb_l <- cbind(dmg='BHM', scen='NZ',dmb_l)
dmb_h <- cbind(dmg='BHM', scen='EOC',dmb_h)
dmh_l <- cbind(dmg='HSTOT', scen='NZ',dmh_l)
dmh_h <- cbind(dmg='HSTOT', scen='EOC',dmh_h)

plotter <- rbind(dmb_l, dmb_h, dmh_l, dmh_h)

write.csv(plotter, "figure2plotter_D.csv", row.names = F)

