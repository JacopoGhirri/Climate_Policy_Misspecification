sample.TCRE <- function(nsamples, respectCI = T){
  if(respectCI){#find parameters based on CI
    q.05.TCRE <- 1.0
    q.95.TCRE <- 2.7
    
    l.s <- (log(q.95.TCRE) - log(q.05.TCRE))/(qnorm(p=0.95) - qnorm(p=0.05))
    l.m <- log(q.05.TCRE) - qnorm(p=0.05)*l.s
    
  } else{#find parameters based on mean and median
    mean.TCRE <- 1.9
    median.TCRE <- 1.8
    
    l.m <- log(median.TCRE)
    l.s <- sqrt(2*(log(mean.TCRE) - l.m))
  }
  
  ret <- rlnorm(n= nsamples, meanlog = l.m, sdlog = l.s)
  return(ret)
}

get_temp <- function(cum.emission.curve, TCRE, baseline = 0.988){#baseline in 2010 from SSP scenarios@iiasa
  baseline = baseline +0.2 #non co2 wsrming
  return(cum.emission.curve*TCRE*1e-3/3.6 + baseline)#TCRE is K/Eg, CO2 data is in Gt
}

get_baseline_temp <- function(){
  ssp_iso3 <- read.csv('data/ssp_gdpcap.csv')
  ssp_iso3 <- unique(ssp_iso3$iso3)
  
  clim_hist<-fread("data/hist_temp.csv")
  clim_hist <- clim_hist[, .(year,iso3,tas = tas_pop_wgt_mean_hist)]
  
  bhm_par <- read_param_bhm()
  bhm_dta <- bhm_par[['BHM DATASET']]
  bhm_baseline <- bhm_dta[year >= 2000, .(tas = mean(temp)), by = "iso3"]
  
  miss <- clim_hist[iso3 %in% ssp_iso3[!ssp_iso3 %in% unique(bhm_dta$iso3)] &
                      year >= 2000,
                    .(tas = mean(tas)),
                    by = "iso3"]
  bhm_baseline <- rbind(bhm_baseline,miss)
  bhm_baseline <- bhm_baseline[bhm_baseline$iso3 %in% ssp_iso3,]
  return(bhm_baseline)
}
