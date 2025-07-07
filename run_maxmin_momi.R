update_files <- F #If true rewrites existing files

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
  save_directory <- "final_results/"
}else{
  SMs <- c("GEM", "MESSAGE", "REMIND", "WITCH")
  save_directory <- "final_results_NOAIM/"
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

get_lambda_exp <- function(combs, dr){
  cores=floor(detectCores()/3)
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  means <- foreach(i = 1:nrow(combs), .combine = 'c')%dopar%{
    p <- combs[i,1]
    s <- combs[i,2]
    d <- combs[i,3]
    u <- combs[i,4]
    npv <- arrow::read_feather(paste("results/NPV_",s,"_",d,"_dr",dr,"_uncetag_", u,"_",p,".feather", sep = ''))
    quantile(as.matrix(npv[,-c(1,2)]), na.rm = T, probs = 0.9)#NA presence ~ 1 out of 10^8 values
  }
  stopCluster(cl)
  return(ceiling(log10(mean(means))))
}

final.criterion <- function(combs, dr, direct, lambda){
  get.util.misspecification <- function(u, l){
    if(is.infinite(l)){
      return(mean(u, na.rm = T))
    }
    return(-l*log(mean(exp(-u/l), na.rm = T)))
  }
  cores=floor(detectCores()/3)
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  utils <- foreach(i = 1:nrow(combs), .combine = 'cbind')%dopar%{
    p <- combs[i,1]
    s <- combs[i,2]
    d <- combs[i,3]
    u <- combs[i,4]
    npv <-  arrow::read_feather(paste("results/NPV_",s,"_",d,"_dr",dr,"_uncetag_", u,"_",p,".feather", sep = ''))
    as.numeric(apply(npv[,-c(1,2)], 1, get.util.misspecification, l = lambda))
  }
  stopCluster(cl)
  return(as.numeric(apply(utils,1,min)))
}

run <- function(lhs, combs, dr, lambda.base.exp, llevel){
  if(llevel=='None'){
    lexp = Inf}
  else{
    lexp = lambda.base.exp-llevel}
  lambda <- 10^lexp
  u <- final.criterion(combs, dr, direct, lambda)
  tab <- as.data.frame(cbind(lhs, value = u))
  tab <- acast(tab, carbon_budget ~ overshoot)
  baseline <- as.numeric(read.csv('data/baseline_world.csv')[,2])
  baseline <- approx(seq(2010,2100,5),baseline[which(2010:2100 %in% seq(2010,2100,5))],2010:2100)$y#flatten links between each 5 years
  dfac = 1/(1+(dr/100))^((2010:2100)-2010)
  base_NPV = sum(sum(dfac*baseline))
  tab <- tab/base_NPV
  write.csv(tab, paste(save_directory,"value_",paste(dmgs,collapse=''),"_uncetag_", unce_tags,"_dr",dr,"_mislevel_",llevel,".csv", sep = ''))
}

lambda.base.exp <- get_lambda_exp(combs = combs, dr = dr)

already_computed <- list.files(save_directory)

for(mis_lev in seq(from=0, to = 1.75, by = 0.05)){
  if(!update_files & (paste(save_directory,"value_",paste(dmgs,collapse=''),"_uncetag_", unce_tags,"_dr",dr,"_mislevel_",mis_lev,".csv", sep = '') %in% already_computed)){next}
  run(lhs, combs, dr, lambda.base.exp, mis_lev)
}

run(lhs, combs, dr, lambda.base.exp, 'None')
