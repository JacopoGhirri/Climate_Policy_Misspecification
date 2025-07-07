#unified script for the data needed to generate figures 3-6

#Figure 3 needs:
#dmgs <- "BHMHSTOT"
#unce_tags <- "ALL"
#directory <- "final_results"
#dr <- 3

#Figure 4 needs:
#dmgs <- "BHMHSTOT"
#unce_tags <- "ALL", "NODMG", "NOCLIM", "NOMOD" SEPARATELY
#directory <- "final_results"
#dr <- 3

#Figure 5 needs:
#dmgs <- "BHMHSTOT"
#unce_tags <- "ALL"
#directory <- "final_results"
#dr <- 1, 2, 3, 4, 5 SEPARATELY

#Figure 6 needs:
#dmgs <- "BHMHSTOT"
#unce_tags <- "ALL"
#directory <- "final_results_NOAIM"
#dr <- 1, 2, 3, 4, 5 SEPARATELY


dmgs <- "BHMHSTOT" #"BHMHSTOT", "BHM", or "HSTOT"
unce_tags <- "ALL" #"ALL", "NODMG", "NOCLIM", or "NOMOD"
directory <- "final_results"# "final_results", or "final_results_NOAIM"
dr <- 3 #1, 2, 3, 4, or 5

#representative levels of misspecification fear
mislevels <- c(0.65, 1.05, 1.45)


#MC estimation leaves significant noise. Due to the high computation load, an exponential increase in the number of samples is not feasible
#we exploit the problem continuity, performing two levels of smoothing to plot the underlying denoised signal
smooth_map <- T
smooth_curve <- T
smoothing <- "no smoothing"
if(smooth_map){
  smoothing <- "map smoothing"
  if(smooth_curve){
    smoothing <- "full smoothing"
    bandwidth <- 200
  }
}

library(lattice)
library(SpatialPack)

source("R/AAA_packages.R")

rowwise_max <- function(m){
  args <- numeric(nrow(m))
  for(i in 1:nrow(m)){
    args[i] <- which(m[i,] == max(m[i,]))[1]
  }
  return(colnames(m)[args])
}

projection.2300 = c('noproj')
cb.grid <- seq(750, 2000, 10)
os.grid <- seq(0, 200, 10)

tab_seu_1 <- read.csv(paste(directory,"/SEU_",dmgs,"_uncetag_", unce_tags,"_dr",dr,".csv", sep = ''), row.names = 1)

if(smooth_map){tab_seu_1 <- denoise(as.matrix(tab_seu_1), type = 'median')}
colnames(tab_seu_1)<-os.grid

bestos.se <- rowwise_max(tab_seu_1)
bestcb.se <- which(tab_seu_1==max(tab_seu_1),arr.ind = T)[,1]
bestcb.se <- cb.grid[bestcb.se[length(bestcb.se)]]

if(smooth_curve){bestos.se <- ksmooth(x=cb.grid,y=as.numeric(bestos.se), kernel = "normal", bandwidth = bandwidth)$y}

tab_maxmin_1 <- read.csv(paste(directory,"/value_",dmgs,"_uncetag_", unce_tags,"_dr",dr,"_mislevel_",'None',".csv", sep = ''), row.names = 1)

if(smooth_map){tab_maxmin_1 <- denoise(as.matrix(tab_maxmin_1), type = 'median')}
colnames(tab_maxmin_1)<-os.grid

bestos.mm <- rowwise_max(tab_maxmin_1)
bestcb.mm <- which(tab_maxmin_1==max(tab_maxmin_1),arr.ind = T)[,1]
bestcb.mm <- cb.grid[bestcb.mm[length(bestcb.mm)]]
if(smooth_curve){bestos.mm <- ksmooth(x=cb.grid,y=as.numeric(bestos.mm), kernel = "normal", bandwidth = bandwidth)$y}

misl <- mislevels[1]
tab_val <- read.csv(paste(directory,"/value_",dmgs,"_uncetag_", unce_tags,"_dr",dr,"_mislevel_",misl,".csv", sep = ''), row.names = 1)
if(smooth_map){tab_val <- denoise(as.matrix(tab_val), type = 'median')}
colnames(tab_val)<-os.grid
bestos.0 <- rowwise_max(tab_val)
bestcb.0 <- which(tab_val==max(tab_val),arr.ind = T)[,1]
bestcb.0 <- cb.grid[bestcb.0[length(bestcb.0)]]
if(smooth_curve){bestos.0 <- ksmooth(x=cb.grid,y=as.numeric(bestos.0), kernel = "normal", bandwidth = bandwidth)$y}
misl <- mislevels[2]
tab_val <- read.csv(paste(directory,"/value_",dmgs,"_uncetag_", unce_tags,"_dr",dr,"_mislevel_",misl,".csv", sep = ''), row.names = 1)
if(smooth_map){tab_val <- denoise(as.matrix(tab_val), type = 'median')}
colnames(tab_val)<-os.grid
bestos.1 <- rowwise_max(tab_val)
bestcb.1 <- which(tab_val==max(tab_val),arr.ind = T)[,1]
bestcb.1 <- cb.grid[bestcb.1[length(bestcb.1)]]
if(smooth_curve){bestos.1 <- ksmooth(x=cb.grid,y=as.numeric(bestos.1), kernel = "normal", bandwidth = bandwidth)$y}
misl <- mislevels[3]
tab_val <- read.csv(paste(directory,"/value_",dmgs,"_uncetag_", unce_tags,"_dr",dr,"_mislevel_",misl,".csv", sep = ''), row.names = 1)
if(smooth_map){tab_val <- denoise(as.matrix(tab_val), type = 'median')}
colnames(tab_val)<-os.grid
bestos.2 <- rowwise_max(tab_val)
bestcb.2 <- which(tab_val==max(tab_val),arr.ind = T)[,1]
bestcb.2 <- cb.grid[bestcb.2[length(bestcb.2)]]
if(smooth_curve){bestos.2 <- ksmooth(x=cb.grid,y=as.numeric(bestos.2), kernel = "normal", bandwidth = bandwidth)$y}

plotter <- cbind(cb.grid=cb.grid,
                 bestos.mm=bestos.mm,
                 bestos.0=bestos.0,
                 bestos.1=bestos.1,
                 bestos.2=bestos.2,
                 bestos.se=bestos.se)

#first row is global optimal Carbon budget
plotter <- rbind(c(cb.grid=0, 
                   bestos.mm=bestcb.mm,
                   bestos.0=bestcb.0,
                   bestos.1=bestcb.1,
                   bestos.2=bestcb.2,
                   bestos.se=bestcb.se),plotter)

if(directory=="final_results"){
  write.csv(plotter, paste("plots/figures_os_",unce_tags,"_dr_", dr,".csv", sep = ''),row.names = F)
}else{
  write.csv(plotter, paste("plots/figures_os_NOAIM_",unce_tags,"_dr_", dr,".csv", sep = ''),row.names = F)
}
