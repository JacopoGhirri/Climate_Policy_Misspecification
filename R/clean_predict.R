#get mitigation costs and cumulative emissions central estimates

predict.mitcost.clean <- function(mod, newcb, newpos.cb){
  coefrow <- model.matrix(as.formula(mod$formula), data = data.frame(cb=newcb, pos.cb = newpos.cb))
  
  fit.mitcost.dirty <- predict.fosr(mod$m, coefrow)
  fit.MC <- (0.3+2*mod$tolerance)/(1+exp(-fit.mitcost.dirty))-mod$tolerance
  
  return(fit.MC)
}

predict.cumemi.clean <- function(mod, newcb, newpos.cb){
  coefrow <- model.matrix(as.formula(mod$formula), data = data.frame(cb=newcb, pos.cb = newpos.cb))
  
  fit.cumemi.dirty <- c(0,predict.fosr(mod$m, coefrow))
  fit.cumemi <- respect.cb(fit = fit.cumemi.dirty, cb = newcb, pos.cb = newpos.cb, v.2010 = mod$v.2010)
  
  return(fit.cumemi)
}
