predict.mitcost.MC <- function(mod, newcb, newpos.cb, n.MC = 10000){
  coefrow <- model.matrix(as.formula(mod$formula), data = data.frame(cb=newcb, pos.cb = newpos.cb))
  
  fit.mitcost.dirty <- predict.fosr(mod$m, coefrow)
  res.params <- fit.residual.distribution(mod = mod)
  
  res.MC <- sample.from.residuals(n = n.MC, params = res.params)
  fit.MC.dirty <- t(t(res.MC) + fit.mitcost.dirty)
  
  fit.MC <- (0.3+2*mod$tolerance)/(1+exp(-fit.MC.dirty))-mod$tolerance
  
  return(fit.MC)
}

predict.cumemi.MC <- function(mod, newcb, newpos.cb, n.MC = 10000){
  coefrow <- model.matrix(as.formula(mod$formula), data = data.frame(cb=newcb, pos.cb = newpos.cb))
  
  fit.cumemi.dirty <- c(0,predict.fosr(mod$m, coefrow))
  res.params <- fit.residual.distribution.CONSTR(mod = mod)
  
  res.MC <- sample.from.residuals(n = n.MC, params = res.params)
  res.MC <- cbind(0, res.MC, 0)
  fit.MC <- t(t(res.MC) + fit.cumemi.dirty)
  fit.MC <- respect.cb(fit.MC, newcb, newpos.cb, mod$v.2010)
  
  return(fit.MC)
}
