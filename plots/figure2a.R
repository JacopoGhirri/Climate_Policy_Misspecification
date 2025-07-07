#Data for figure2 panel A
source("R/AAA_packages.R")
source("R/AAA_functions.R")

set.seed(17021890)
datablock <- get.data()
formulas <- read.formulas()
models <- fit.models(data = datablock, formula.miticost = formulas$mitcost, formula.emi = formulas$emi, plot = F)
SMs <- c("AIM","GEM", "MESSAGE", "REMIND", "WITCH")
baseline <- read.csv('data/baseline_world.csv')[,2]

cb.grid <- seq(750, 2000, 10)
os.grid <- seq(0, 200, 10)
n.MC <- 5000
dr <- 3/100 #3% discount rate
confidence <- 90/100
plotter <- matrix(nrow = length(cb.grid), ncol = 4*length(SMs))
colnames(plotter) <- c(paste(SMs, "low", sep = '_'), paste(SMs, "mean", sep = '_'), paste(SMs, "median", sep = '_'), paste(SMs, "high", sep = '_'))
rownames(plotter) <- cb.grid

dfac <- 1/(1+dr)^((2020:2100)-2010)

base_NPV <- sum(baseline[-(1:10)]*dfac)

set.seed(123)

for(i in 1:length(cb.grid)){
  cb <- cb.grid[i]
  for(s in SMs){
    row <- numeric(n.MC*length(os.grid))
    idx <- 0
    for(os in os.grid){
      mitcost <- predict.mitcost.MC(mod = models$model.micost[[s]], newcb = cb, newpos.cb = cb+os, n.MC = n.MC)
      for(l in 1:n.MC){
        temp <- mitcost[l,]
        temp <- (baseline*approx(seq(2010, 2100, 5), temp, 2010:2100)$y)[-(1:10)]
        idx <- idx+1
        row[idx] <- sum(temp*dfac)
      }
    }
    plotter[i, which(colnames(plotter)==paste(s,'low',sep='_'))] <- quantile(row, probs = (1-confidence)/2)
    plotter[i, which(colnames(plotter)==paste(s,'mean',sep='_'))] <- mean(row)
    plotter[i, which(colnames(plotter)==paste(s,'median',sep='_'))] <- median(row)
    plotter[i, which(colnames(plotter)==paste(s,'high',sep='_'))] <- quantile(row, probs = (1+confidence)/2)
  }
  print(cb)
}


dmc <- numeric(nrow(datablock$data$data$DT.miticost))
dsm <- replicate(nrow(datablock$data$data$DT.miticost), NA)
dcb <- get.cb(datablock$data$data$DT.e.co2)
dos <- get.pos.cb(datablock$data$data$DT.e.co2) - dcb
for(i in 1:nrow(datablock$data$data$DT.miticost)){
  temp <- datablock$data$data$DT.miticost[i,]
  temp <- (baseline*approx(seq(2010, 2100, 5), temp, 2010:2100)$y)[-(1:10)]
  dmc[i] <- sum(temp*dfac)
  dsm[i] <- as.character(datablock$data$meta$SM[i])
}



dmc <- dmc[which(dos <= 200)]
dsm <- dsm[which(dos <= 200)]
dcb <- dcb[which(dos <= 200)]
dos <- dos[which(dos <= 200)]
dmc <- dmc*100/base_NPV

data_A <- cbind('dmc' = dmc, 'dcb' = dcb, 'dsm' = dsm)
write.csv(data_A, "data_A.csv", row.names = F)

plotter <- plotter*100/base_NPV

write.csv(plotter, "figure2plotter_A.csv", row.names = F)
