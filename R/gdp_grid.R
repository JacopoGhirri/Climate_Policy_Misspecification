compute.and.save <- function(models, newdata, SM, damage, par.BHM, uncertainty_configuration, n = 10000){
  newcb <- newdata$newcb
  newpos.cb <- newdata$newpos.cb
  
  if(uncertainty_configuration['mean.models']){
    mitcost <- predict.mitcost.clean(mod = models$model.micost[[SM]], newcb = newcb, newpos.cb = newpos.cb)
    mitcost <- t(rbind(replicate(n,mitcost)))
    cumemi <- predict.cumemi.clean(mod = models$model.cumemi[[SM]], newcb = newcb, newpos.cb = newpos.cb)
    cumemi <- t(rbind(replicate(n,cumemi)))
  }else{
    mitcost <- predict.mitcost.MC(mod = models$model.micost[[SM]], newcb = newcb, newpos.cb = newpos.cb, n.MC = n)
    cumemi <- predict.cumemi.MC(mod = models$model.cumemi[[SM]], newcb = newcb, newpos.cb = newpos.cb, n.MC = n)
  }
  
  
  if(uncertainty_configuration['mean.TCRE']){
    tcurves <- get_temp(cum.emission.curve = cumemi, TCRE = 1.8)
  }else{
    TCREs <- sample.TCRE(nsamples = n, respectCI = T)
    tcurves <- get_temp(cum.emission.curve = cumemi, TCRE = TCREs)
  }
  
  baseline <- read.csv('data/baseline_world.csv')[,2]
  
  climp <- compute.climate.impact(temp = tcurves, dmg.fun = damage, y.int = models$y.int, baseline = baseline, par.BHM = par.BHM, mean.par = uncertainty_configuration['mean.par'])
  
  colnames(mitcost)<-models$y.int
  colnames(climp)<-models$y.int
  
  dgdp <- compute.net.gdp(miticost = mitcost, climpacts = climp, baseline = baseline[which(seq(2010,2100) %in% models$y.int)])
  
  write.csv(dgdp, paste("results/ngdp_",SM,"_",damage,"_uncetag_",uncertainty_configuration['tag'],"_cb",newcb,"_poscb",newpos.cb,".csv", sep = ''), row.names = F)
}

visualize.damages <- function(models, SM, damage, par.BHM, n = 10000){
  
  cumemi <- c(40294.323, 40656.969, 34541.602, 26305.612, 16330.601, 9247.853, 2010.280, -4045.767, -6495.753, -8357.277)*1e-3
  cumemi <- cumsum(approx(seq(2010,2100,10), cumemi, 2010:2100)$y)[which(2010:2100 %in% seq(2010,2100,5))]
  cumemi <- cumemi - cumemi[1] + models$model.cumemi[[SM]]$v.2010
  cumemi <- t(rbind(replicate(n,cumemi)))
  colnames(cumemi) <- seq(2010, 2100, 5)
  
  TCREs <- sample.TCRE(nsamples = n, respectCI = T)
  tcurves <- get_temp(cum.emission.curve = cumemi, TCRE = TCREs)

  baseline <- read.csv('data/baseline_world.csv')[,2]
  
  climp <- compute.climate.impact(temp = tcurves, dmg.fun = damage, y.int = models$y.int, baseline = baseline, par.BHM = par.BHM, mean.par = F)
  
  colnames(climp)<-models$y.int
  
  
  write.csv(climp, paste("damage_visualizer/climp_",SM,"_",damage,".csv", sep = ''), row.names = F)
}

get_unce_conf <- function(){
  uncertainty_configurations <- list()
  
  uncertainty_configurations = append(uncertainty_configurations, list('ALL'= c()))
  uncertainty_configurations[['ALL']]['mean.models'] <- FALSE
  uncertainty_configurations[['ALL']]['mean.TCRE'] <- FALSE
  uncertainty_configurations[['ALL']]['mean.par'] <- FALSE
  uncertainty_configurations[['ALL']]['tag'] = 'ALL'
  
  uncertainty_configurations = append(uncertainty_configurations, list('NODMG'= c()))
  uncertainty_configurations[['NODMG']]['mean.models'] <- FALSE
  uncertainty_configurations[['NODMG']]['mean.TCRE'] <- FALSE
  uncertainty_configurations[['NODMG']]['mean.par'] <- TRUE
  uncertainty_configurations[['NODMG']]['tag'] = 'NODMG'
  
  uncertainty_configurations = append(uncertainty_configurations, list('NOCLIM'= c()))
  uncertainty_configurations[['NOCLIM']]['mean.models'] <- FALSE
  uncertainty_configurations[['NOCLIM']]['mean.TCRE'] <- TRUE
  uncertainty_configurations[['NOCLIM']]['mean.par'] <- FALSE
  uncertainty_configurations[['NOCLIM']]['tag'] = 'NOCLIM'
  
  uncertainty_configurations = append(uncertainty_configurations, list('NOMOD'= c()))
  uncertainty_configurations[['NOMOD']]['mean.models'] <- TRUE
  uncertainty_configurations[['NOMOD']]['mean.TCRE'] <- FALSE
  uncertainty_configurations[['NOMOD']]['mean.par'] <- FALSE
  uncertainty_configurations[['NOMOD']]['tag'] = 'NOMOD'
  
  return(uncertainty_configurations)
}

save.steps <- function(models, cb.grid, os.grid, n = 10000, seed = 17021890, overwrite = F){
  set.seed(seed)
  SMs <- c("AIM","GEM", "MESSAGE", "REMIND", "WITCH")#core IAMs defining SMs
  dmgs <- c('BHM', 'HSTOT')
  unce_tags <- c('ALL', 'NODMG', 'NOMOD', 'NOCLIM')
  
  uncertainty_configurations <- get_unce_conf()
  
  
  par.BHM <- get.BHM.par()
  combs <- expand.grid(cb.grid, os.grid, SMs, dmgs, unce_tags)
  colnames(combs)<- c("cb", "os", "s", "d", "t")
  
  if(!overwrite){
    already_computed <- list.files('results/')
    rmv <- c()
    for(i in 1:nrow(combs)){
      if(paste("ngdp_",combs$s[i],"_",combs$d[i],"_uncetag_",combs$t[i],"_cb",combs$cb[i], "_poscb", combs$cb[i]+combs$os[i], ".csv", sep = '') %in% already_computed){
        rmv <- c(rmv, i)
      }
    }
    if(length(rmv)>0){
      print(paste(100*length(rmv)/nrow(combs),"% of simulations have already been computed"))
      combs <- combs[-rmv,]
    }
  }
  
  pb <- progress_bar$new(total = nrow(combs))
  pb$tick(0)
  for(i in 1:nrow(combs)){
    newcb <- combs$cb[i]
    newpos.cb <- newcb + combs$os[i]
    s <- combs$s[i]
    d <- combs$d[i]
    t <- combs$t[i]
    
    uncertainty_configuration = uncertainty_configurations[[as.character(t)]]
    
    #unique configuration based seed
    newseed <- (floor(log(seed))^(which(dmgs==d)+1))*newcb + floor(sqrt(seed)*log(newpos.cb - newcb + 1))*which(SMs==s) + floor(sqrt(log(seed)*log(newpos.cb)))*which(unce_tags==t)
    set.seed(newseed)
    
    compute.and.save(models, newdata = list(newcb = newcb, newpos.cb = newpos.cb), SM = s, damage = d, par.BHM = par.BHM, uncertainty_configuration=uncertainty_configuration, n = n)
    pb$tick()
  }
}
