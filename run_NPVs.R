args <- base::commandArgs(trailingOnly = T)

source("R/AAA_packages.R")
source("R/AAA_functions.R")

projection.2300 = c('constant', 'linear', 'noproj')
cb.grid <- seq(750, 2000, 10)
os.grid <- seq(0, 200, 10)
SMs <- c("AIM", "GEM", "MESSAGE", "REMIND", "WITCH")
dmgs <- c("BHM", "HSTOT")
unce <- c('ALL', 'NOMOD', 'NODMG', 'NOCLIM')
if(is.na(as.numeric(args[2]))){
  stop("NA discount rate")
}

compute.utility.lines(discount.rate = as.numeric(args[2]), cb.grid = cb.grid, os.grid = os.grid, SMs = SMs, dmgs = dmgs, projection.2300 = 'noproj', unce = unce)
