args <- base::commandArgs(trailingOnly = T)
dr <- as.numeric(args[2])
dm.ID <- as.numeric(args[3])
un.ID <- as.numeric(args[4])
source("R/AAA_packages.R")
source("R/AAA_functions.R")

all_models <- T #set False for the "NOAIM" setting, Figure 6


projection.2300 = c('noproj')
cb.grid <- seq(750, 2000, 10)
os.grid <- seq(0, 200, 10)
if(all_models){
  SMs <- c("AIM", "GEM", "MESSAGE", "REMIND", "WITCH")
}else{
  SMs <- c("GEM", "MESSAGE", "REMIND", "WITCH")
}

if(dm.ID == 1){
  dmgs <- c("BHM")
}else if(dm.ID == 2){
  dmgs <- c("HSTOT")
}else if(dm.ID==3){
  dmgs <- c("BHM", 'HSTOT')
}
if(un.ID == 1){
  unce_tags <- c('ALL')
}else if(un.ID == 2){
  unce_tags <- c('NOMOD')
}else if(un.ID == 3){
  unce_tags <- c('NODMG')
}else if(un.ID == 4){
  unce_tags <- c('NOCLIM')
}

combs <- expand.grid(projection.2300,SMs,dmgs, unce_tags)
colnames(combs)<- c("p", "s", "d", "u")

lhs <- read_feather(paste("results/NPV_",combs$s[1],"_",combs$d[1],"_dr",dr,"_uncetag_", unce_tags,"_",combs$p[1],".feather", sep = ''))
lhs <- lhs[,c(1,2)]

SEU <- function(combs, dr){
  meaner <- function(u, na.rm){
    mean(u, na.rm = na.rm)  
  }
  cores=floor(detectCores()/3)
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  utils <- foreach(i = 1:nrow(combs), .combine = 'cbind')%dopar%{
    p <- combs[i,1]
    s <- combs[i,2]
    d <- combs[i,3]
    ut <- combs[i,4]
    npv <- arrow::read_feather(paste("results/NPV_",s,"_",d,"_dr",dr,"_uncetag_", ut,"_",p,".feather", sep = ''))
    as.numeric(apply(npv[,-c(1,2)], 1, meaner, na.rm = T))
  }
  stopCluster(cl)
  return(as.numeric(apply(utils,1,mean)))
}

run <- function(lhs, combs, dr){
  u <- SEU(combs, dr)
  tab <- as.data.frame(cbind(lhs, value = u))
  tab <- acast(tab, carbon_budget ~ overshoot)
  baseline <- as.numeric(read.csv('data/baseline_world.csv')[,2])
  baseline <- approx(seq(2020,2100,5),baseline[which(2010:2100 %in% seq(2020,2100,5))],2020:2100)$y#flatten links between each 5 years
  dfac = 1/(1+(dr/100))^((2020:2100)-2020)
  base_NPV = sum(sum(dfac*baseline))
  tab <- tab/base_NPV
  if(all_models){
    write.csv(tab, paste("final_results/SEU_",paste(dmgs, collapse=''),"_uncetag_", unce_tags,"_dr",dr,".csv", sep = ''))
  }else{
    write.csv(tab, paste("final_results_NOAIM/SEU_",paste(dmgs, collapse=''),"_uncetag_", unce_tags,"_dr",dr,".csv", sep = ''))
  }
}

run(lhs, combs, dr)

