gdpl.fitmodel <- function(Y, X, y.int, row.sel = NULL, plot = F, scale = F){
  if((is.null(row.sel) && nrow(X)!=nrow(Y)) || (!is.null(row.sel) && sum(row.sel) != nrow(X))){
    stop("error in number of rows in model.matrix, possibly due to NaNs")
  }
  
  if(!is.null(row.sel)){
    Y <- Y[row.sel,]
  }
  
  mod <- try(refund::fosr(Y=Y, X=X, method = "OLS", nbasis=30, argvals = y.int), silent = T)
  if("try-error" %in% class(mod)) {
    print("withfda")
    
    basis <- fda::create.polygonal.basis(rangeval=c(range(y.int)), argvals = y.int)
    
    Y.fd <- fda::Data2fd(y = t(Y),argvals = y.int,basisobj = basis)
    mod <- refund::fosr(fdobj=Y.fd, X=X, method = "OLS", argvals = y.int, nbasis = 40, norder = 1, pen.order = 1)
    if(plot){
      x11()
      par(mfrow = c(2,2))
      matplot(y = t(Y), x = y.int, type = 'l', main = 'Data', ylim = range(Y))
      plot(Y.fd, main = 'Smoothed', ylim = range(Y))
      plot(mod$yhat, main = 'Fitted', ylim = range(Y))
      plot(mod$resid, main = 'Residuals')
    }
    return(mod)
  }else{
    if(plot){
      x11()
      par(mfrow = c(1,3))
      matplot(y = t(Y), x = y.int, type = 'l', main = 'Data', ylim = range(Y))
      matplot(y = t(mod$yhat), x = y.int, type = 'l', main = 'Fitted', ylim = range(Y))
      matplot(y = t(mod$resid), x = y.int, type = 'l', main = 'Residuals')
      
    }
    return(mod)
  }
}

SM.gdpl.fit_single <- function(data, cb, pos.cb, y.int, plot = F, formula = NULL, flag = NULL, tolerance = 0){
  
  cov.data <- data.frame(cb = cb, pos.cb = pos.cb)
  
  X <- model.matrix(as.formula(formula), data = cov.data)
  Y <- as.data.frame(data)
  
  #transform data
  Y <- (Y+tolerance)/(0.3+2*tolerance)
  Y <- log(Y) - log(1-Y)
  
  m <- gdpl.fitmodel(Y=Y, X = X, row.sel = NULL, plot = F, y.int = y.int)
  
  if(m$resp.type=="fd"){
    fit <- t(fda::eval.fd(m$yhat, evalarg = m$argvals))
    res <- t(fda::eval.fd(m$resid, evalarg = m$argvals))
  }else{
    fit <- m$yhat
    res <- m$resid
  }
  
  if(plot){
    x11()
    par(mfrow = c(1,3))
    matplot(y = t(Y), x = y.int, type = 'l', main = 'Data', ylim = range(Y))
    matplot(y = t(fit), x = y.int, type = 'l', main = 'Fitted', ylim = range(Y))
    matplot(y = t(res), x = y.int, type = 'l', main = 'Residuals')
    if(!is.null(flag)){title(main = flag, outer = T, line = -1)}
  }
  ret <- list(m = m, cov.data = cov.data, res = res, formula = formula, tolerance = tolerance, name = flag)
  return(ret)
}

SM.gdpl.fit <- function(data, meta, y.int, formulas, tolerance, plot = F){
  
  cb <- get.cb(data$DT.e.co2)
  pos.cb <- get.pos.cb(data$DT.e.co2)
  
  SM <- unique(meta$SM)
  ret <- list()
  for(sm in SM){
    ids <- which(meta$SM == sm)
    temp <- SM.gdpl.fit_single(data = data$DT.miticost[ids,], cb = cb[ids], pos.cb = pos.cb[ids], y.int = y.int, plot = plot, formula = formulas[sm], flag = sm, tolerance = tolerance)
    ret <- append(ret, list(temp))
    names(ret)[length(ret)]<-sm
  }
  
  return(ret)
}
