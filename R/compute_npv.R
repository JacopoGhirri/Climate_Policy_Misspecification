#projection in the deep future is arbitrary and unstable, only no_proj is used
utility.gdp <- function(gdp,year, projection.2300) {
  utility <- approx(year,gdp,2020:2100)$y # GDP/year
  if(projection.2300 == 'constant'){
    utility <- c(utility, replicate(200, utility[length(utility)]))
  }else if(projection.2300 == 'linear'){
    projection <- utility[length(utility)] + (0:199)*(utility[length(utility)] - utility[length(utility)-5])/5
    utility <- c(utility, projection)
  }else if(projection.2300 == 'noproj'){
     utility <- c(utility, replicate(200, 0))
  }
  return(utility)
}

compute_single_lines <- function(ngdp, y.int, dr, projection.2300){
  n <- nrow(ngdp)
  
  npv <- numeric(n)
  r = dr/100 # discount rate
  dfac = 1/(1+r)^((2020:2300)-2010) # discount factor
  
  for(i in 1:n){
    temp <- utility.gdp(gdp = as.numeric(ngdp[i,]), year = y.int, projection.2300 = projection.2300)
    npv[i] <- sum(dfac*temp)
  }
  return(npv)
}

compute.utility.lines <- function(discount.rate, cb.grid, os.grid, SMs, dmgs, projection.2300, unce, directory = 'results', write.dir = 'results'){
  if(!(projection.2300 %in% c('constant', 'linear', 'noproj'))){
    stop("non admissible extrapolations")
  }
  
  combs <- expand.grid(cb.grid, os.grid, SMs, dmgs, unce)
  
  colnames(combs)<- c("cb", "os", "s", "d", "u")
  
  computed <- list.files(paste(directory,'/', sep = ''))
  computed_npvs <- list.files(paste(write.dir,'/', sep = ''))
  
  file_names_to_check <- with(combs, paste("ngdp_", s, "_", d, "_uncetag_", u, "_cb", cb, "_poscb", cb + os, ".csv", sep = ''))
  
  # Check for the presence of each file in the computed vector
  missing_files_indices <- which(!(file_names_to_check %in% computed))
  
  # Extract the rows where files are missing
  if(length(missing_files_indices)>0){
    print(paste("The following combinations do not have matching file in /", directory,":", sep = ''))
    print(combs[missing_files_indices,])
    stop("No further computations allowed")
  }
  
  y.int<-seq(2010,2100,by=5)
  
  n.MC = nrow(read.csv(paste(directory,"/ngdp_",combs$s[1],"_",combs$d[1], "_uncetag_", combs$u[1],"_cb",combs$cb[1], "_poscb", combs$cb[1]+combs$os[1], ".csv", sep = ''), header = T))
  
  num_cores <- detectCores()
  cl <- makeCluster(floor(num_cores/2))
  registerDoParallel(cl)
  
  for(d in dmgs){
    for(s in SMs){
      for(u in unce){
        if(paste("NPV_",s,"_",d,"_dr",discount.rate, "_uncetag_", u,"_",projection.2300,".feather", sep = '') %in% computed_npvs){
          next
        }
        combs_sd <- combs[which(combs$s==s & combs$d==d & combs$u==u),]
        
        npv <- foreach(i = 1:nrow(combs_sd), .combine = 'rbind', .export = c("compute_single_lines", "utility.gdp")) %dopar% {
          cb <- combs_sd$cb[i]
          poscb <- cb + combs_sd$os[i]
          
          gdp.lines <- read.csv(paste(directory, "/ngdp_",s,"_",d, "_uncetag_", combs_sd$u[i],"_cb",cb, "_poscb", poscb, ".csv", sep = ''), header = T)
          compute_single_lines(ngdp=gdp.lines, y.int=y.int, dr = discount.rate, projection.2300=projection.2300)
        }
        ls <- combs_sd[, 1:2]
        
        colnames(ls) <- c("carbon_budget", "overshoot")
        colnames(npv) <- paste("npv_", 1:n.MC, sep = '')
        tab <- cbind(ls, npv)
        
        write_feather(tab, paste(write.dir,"/NPV_",s,"_",d,"_dr",discount.rate,"_uncetag_", u,"_",projection.2300,".feather", sep = ''))
        }
    }
  }
  stopCluster(cl)
}
