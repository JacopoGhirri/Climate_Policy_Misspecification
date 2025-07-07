#Data for figure2 panel B
source("R/AAA_packages.R")
source("R/AAA_functions.R")

set.seed(17021890)
datablock <- get.data()
formulas <- read.formulas()
models <- fit.models(data = datablock, formula.miticost = formulas$mitcost, formula.emi = formulas$emi, plot = F)
SMs <- c("AIM","GEM", "MESSAGE", "REMIND", "WITCH")
baseline <- read.csv('data/baseline_world.csv')[,2]

#compute ratio of costs at y_end over y_beg
y_end = 2080
y_beg = 2040

cb.grid <- seq(750, 800, 10) #focus on low carbon budgets, where overshoot is most impactful
os.grid <- seq(0, 200, 10)
n.MC <- 1000
dr <- 3/100
confidence <- 90/100
plotter <- matrix(nrow = length(os.grid), ncol = 4*length(SMs))
colnames(plotter) <- c(paste(SMs, "low", sep = '_'), paste(SMs, "mean", sep = '_'), paste(SMs, "median", sep = '_'), paste(SMs, "high", sep = '_'))
rownames(plotter) <- os.grid

dfac <- 1/(1+dr)^((2020:2100)-2010)

for(i in 1:length(os.grid)){
  os <- os.grid[i]
  for(s in SMs){
    cores=detectCores()
    cl <- makeCluster(cores-2)
    registerDoParallel(cl)
    row <- foreach(cb = cb.grid, .combine = 'c', .export = c("mvrnorm")) %dopar% {
      mitcost <- predict.mitcost.MC(mod = models$model.micost[[s]], newcb = cb, newpos.cb = cb+os, n.MC = n.MC)
      row <- numeric(n.MC)
      for(l in 1:n.MC){
        temp <- mitcost[l,]
        temp <- (baseline*approx(seq(2010, 2100, 5), temp, 2010:2100)$y)[-(1:10)]
        row[l] <- temp[which(2020:2100 == y_end)]/temp[which(2020:2100 == y_beg)]
      }
      return(row)
    }
    stopCluster(cl)
    
    plotter[i, which(colnames(plotter)==paste(s, 'low',sep='_'))] <- quantile(row, probs = (1-confidence)/2)
    plotter[i, which(colnames(plotter)==paste(s,'mean',sep='_'))] <- mean(row)
    plotter[i, which(colnames(plotter)==paste(s,'median',sep='_'))] <- median(row)
    plotter[i, which(colnames(plotter)==paste(s,'high',sep='_'))] <- quantile(row, probs = (1+confidence)/2)
  }
  print(os)
}
write.csv(plotter, "figure2plotter_B.csv")
