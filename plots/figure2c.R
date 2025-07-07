#Data for figure2 panel C
source("R/AAA_packages.R")
source("R/AAA_functions.R")

set.seed(17021890)
datablock <- get.data()
formulas <- read.formulas()
models <- fit.models(data = datablock, formula.miticost = formulas$mitcost, formula.emi = formulas$emi, plot = F)
par.BHM <- get.BHM.par()
dmgs <- c("BHM", "HSTOT")

baseline <- read.csv('data/baseline_world.csv')[,2]

cb.grid <- seq(750, 2000, 10)
os.grid <- 0#look at carbon budget impact, with no overshoot
n.MC <- 15000
confidence <- 90/100
plotter <- matrix(nrow = length(cb.grid), ncol = 4*length(dmgs))
colnames(plotter) <- c(paste(dmgs, "low", sep = '_'), paste(dmgs, "mean", sep = '_'), paste(dmgs, "median", sep = '_'), paste(dmgs, "high", sep = '_'))
rownames(plotter) <- cb.grid

#this NPV configuration extracts end of century damages, if one wants to experiment with different metrics it's possible to edit the discount factor appropriately
dfac <- 0*((2020:2100)-2010)
dfac[length(dfac)] <- 1

for(i in 1:length(cb.grid)){
  cb <- cb.grid[i]
  for(d in dmgs){
    row <- numeric(n.MC*length(os.grid))
    idx <- 0
    for(os in os.grid){
      cumemi <- predict.cumemi.clean(mod = models$model.cumemi$AIM, newcb = cb, newpos.cb = cb+os)
      cumemi <- t(rbind(replicate(n.MC,cumemi)))
      tcurves <- get_temp(cum.emission.curve = cumemi, TCRE = 1.9)
      climp <- compute.climate.impact(temp = tcurves, dmg.fun = d, y.int = models$y.int, baseline = baseline, par.BHM = par.BHM, mean.par = F)
      for(l in 1:n.MC){
        temp <- climp[l,]
        temp <- (baseline*approx(seq(2010, 2100, 5), temp, 2010:2100)$y)[-(1:10)]
        idx <- idx+1
        row[idx] <- sum(temp*dfac)
      }
    }
    plotter[i, which(colnames(plotter)==paste(d,'low',sep='_'))] <- quantile(row, probs = (1-confidence)/2)
    plotter[i, which(colnames(plotter)==paste(d,'mean',sep='_'))] <- mean(row)
    plotter[i, which(colnames(plotter)==paste(d,'median',sep='_'))] <- median(row)
    plotter[i, which(colnames(plotter)==paste(d,'high',sep='_'))] <- quantile(row, probs = (1+confidence)/2)
  }
  print(cb)
}

base_NPV <- sum(baseline[-(1:10)]*dfac)
plotter <- plotter/base_NPV
plotter <- plotter*100

write.csv(plotter, "figure2plotter_C.csv", row.names = F)
