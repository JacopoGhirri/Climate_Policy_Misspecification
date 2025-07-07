run <- function(cfrom, cto){
  source("R/AAA_packages.R")
  source("R/AAA_functions.R")
  set.seed(17021890)
  datablock <- get.data()
  
  formulas <- read.formulas()
  
  models <- fit.models(data = datablock, formula.miticost = formulas$mitcost, formula.emi = formulas$emi, plot = F)
  print(paste(cfrom,cto))
  cb.grid <- seq(from = cfrom, to = cto, by = 10)
  os.grid <- seq(from = 0, to = 200, by = 10)
  
  set.seed(17021890)
  save.steps(models, cb.grid, os.grid, n = 15000, seed = 17021890, overwrite = F)
  
}

args <- base::commandArgs(trailingOnly = T)
run(cfrom = as.numeric(args[2]), cto = as.numeric(args[3]))
