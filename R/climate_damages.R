downscale_temp <- function(temp, downscaling.coeff){
  years <- names(temp)
  countries <- downscaling.coeff$iso3
  ret <- cbind(expand.grid(countries, years), NaN)
  colnames(ret) <- c("iso3", "year", "value")
  for(c in countries){
    ret$value[which(ret$iso3 == c)] <- downscaling.coeff$alpha_temp_pop[which(downscaling.coeff$iso3 == c)] + temp*downscaling.coeff$beta_temp_pop[which(downscaling.coeff$iso3 == c)]
  }
  return(ret)
}

warming_effect <- function(temp, temp_baseline, param){
  g_pool <- function(par, temp) { return(par[1] * temp + par[2] * temp^2) }
  return(g_pool(param,temp) - g_pool(param,temp_baseline))
}

project_bhm <- function(tas, gdpr, gdpc_2010, tas_base, param){
  stopifnot(length(tas) == 19) # 2010-2100 [5-year]
  stopifnot(length(gdpr) == 91) # 2010-2100
  
  .gdpcap <- rep(gdpc_2010,91)
  idx_tas <- ceiling((1:91) / 5)
  idx_tas1 <- pmin(19,ceiling((1:91) / 5) + 1)
  for (i in 2:91) {
    tas_i <- tas[idx_tas[i]] + ((i - 1) %% 5) * (tas[idx_tas1[i]] - tas[idx_tas[i]]) / 5
    .delta <- warming_effect(tas_i - tas[1] + tas_base, tas_base, param)
    .gdpcap[i] <- .gdpcap[i - 1] * (1 + gdpr[i] + .delta)
  }
  
  names(.gdpcap) <- 2010:2100
  y.int <- 2010 + 5*0:18
  
  return(gdpcap_cc = .gdpcap)
}


damage_BHM_SR <- function(temp, ssp_gdpcap, baseline_temp, downscaling.coeff, y.int, baseline, par.BHM, mean.par){
  par <- par.BHM[sample(nrow(par.BHM))[1],]
  if(mean.par){
    par <- c(0.01272, -4.871*1e-4)
  }
  
  dt <- downscale_temp(temp, downscaling.coeff)
  
  full_gdp <- numeric(length(91))
  
  for(c in unique(ssp_gdpcap$iso3)){
    tas <- dt[which(dt$iso3 == c),-1]
    tas <- tas$value[order(tas$year)]
    gdp <- project_bhm(tas, 
                       ssp_gdpcap$gdpr[which(ssp_gdpcap$iso3 == c)], 
                       ssp_gdpcap$gdpcap_nocc[which(ssp_gdpcap$iso3 == c & ssp_gdpcap$year == 2010)], 
                       baseline_temp$tas[which(baseline_temp$iso3 == c)],
                       par)
    gdp <- gdp*ssp_gdpcap$pop[which(ssp_gdpcap$iso3==c)]
    full_gdp <- full_gdp + gdp
  }
  full_gdp <- full_gdp*1e-3
  
  climpacts <- -(full_gdp/baseline - 1)
  return(climpacts[which(seq(2010,2100) %in% y.int)])
}

damage_HSTOT <- function(temp, baseline, mean){
  nro <- nrow(temp)
  if(mean){
    coef <- -(0.59504 * 1.25 + 0.260 + 0.113 * 1.25) * 1e-2
    climpacts <- coef*temp^2
  }else{
    t2 <- rnorm(n=nro, mean = 0.59504, sd = 0.190)
    mktt2 <- rnorm(n=nro, mean = 0.260, sd = 0.267)
    prodt2 <- rnorm(n=nro, mean = 0.113, sd = 0.125)
    
    coefs <- t2*1.25 + mktt2 + prodt2*1.25
    while(any(coefs<0)){
      n <- sum(coefs<0)
      coefs[coefs<0] <- rnorm(n=n, mean = 0.59504, sd = 0.190)*1.25+rnorm(n=n, mean = 0.260, sd = 0.267)+rnorm(n=n, mean = 0.113, sd = 0.125)*1.25
    }
    climpacts <- -coefs*1e-2*temp^2
  }
  return(climpacts)
}

compute.climate.impact <- function(temp, dmg.fun, y.int, baseline, par.BHM = par.BHM, mean.par = FALSE, seed = NULL){
  colnames(temp)<-y.int
  
  if(dmg.fun == 'BHM'){
    baseline_temp <- read_feather("data/baseline_temp.feather")
    downscaling.coeff <- read_parquet('data/cmip6_downscaling_coef.parquet')
    ssp_gdpcap <- read.csv('data/ssp_gdpcap.csv')
    climp <- matrix(NaN, nrow = nrow(temp), ncol = 19)
    cores=detectCores()
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    climp <- foreach(i = 1:nrow(temp), .combine = 'rbind', .export = c("damage_BHM_SR", "project_bhm","warming_effect", "downscale_temp")) %dopar% {
      damage_BHM_SR(temp[i,], ssp_gdpcap, baseline_temp, downscaling.coeff, y.int, baseline, par.BHM, mean.par)
    }
    stopCluster(cl)
    
    colnames(climp) <- y.int
  }
  if(dmg.fun == 'HSTOT'){
    climp<- -(damage_HSTOT(temp, baseline, mean.par))
  }
  
  return(climp)
}
