#GDP curve net of costs and damages
compute.net.gdp <- function(miticost, climpacts, baseline){
  if(dim(miticost)[1]!=dim(climpacts)[1] || dim(miticost)[2]!=dim(climpacts)[2]){
    stop(paste("miticost (", dim(miticost)[1],",",dim(miticost)[2], ") and climpacts (", dim(climpacts)[1],",",dim(climpacts)[2], ") do not have same shape", sep = ''))
  }
  
  new.gdp <- t(matrix(baseline, nrow = dim(miticost)[2], ncol = dim(miticost)[1]))
  new.gdp <- new.gdp*(1-miticost)*(1-climpacts)
  colnames(new.gdp) <- seq(from=2010, to = 2100, by = 5)
  
  return(new.gdp)
}