#obtain cumulative emissions from emission pathways
get_cumulative <- function(Y){
  Y <- as.matrix(Y)
  ret <- apply(Y, 1, cumsum)
  return(t(ret))
}

cumulative.emissions.fitmodel <- function(Y, X, y.int, row.sel = NULL, plot = F, scale = F){
  if((is.null(row.sel) && nrow(X)!=nrow(Y)) || (!is.null(row.sel) && sum(row.sel) != nrow(X))){
    stop("error in number of rows in model.matrix, possibly due to NaNs")
  }
  
  if(ncol(Y)!= length(y.int)){
    y.int <- y.int[-1]
  }
  if(!is.null(row.sel)){
    Y <- Y[row.sel,]
  }
  
  mod <- try(refund::fosr(Y=Y, X=X, method = "OLS", nbasis=30, argvals = y.int), silent = T)
  if("try-error" %in% class(mod)) {
    print("withfda")
    basis <- fda::create.bspline.basis(rangeval=c(range(y.int)), nbasis=30, norder=4)
    Y.fd <- fda::Data2fd(y = t(Y),argvals = y.int,basisobj = basis)
    mod <- refund::fosr(fdobj=Y.fd, X=X, method = "OLS", argvals = y.int)
    
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

SM.cum.emi.fit_single <- function(data, y.int, formula, flag, plot = F){
  cb <- get.cb(data)
  pos.cb <- get.pos.cb(data)
  
  cov.data <- data.frame(cb = cb, pos.cb = pos.cb)
  
  X <- model.matrix(as.formula(formula), data = cov.data)
  
  v.2010 <- mean(unlist(data[,1]))
  
  Y <- get_cumulative(Y = as.data.frame(data))-v.2010
  m <- cumulative.emissions.fitmodel(Y=Y[,-1], X = X, row.sel = NULL, plot = F, y.int = y.int)
  
  if(m$resp.type=="fd"){
    fit <- cbind(0,t(fda::eval.fd(m$yhat, evalarg = m$argvals)))
  }else{
    fit <- cbind(0,m$yhat)
  }
  
  Y <- Y+v.2010
  
  newfit <- matrix(nrow = nrow(fit), ncol = ncol(fit))
  newres <- matrix(nrow = nrow(fit), ncol = ncol(fit))
  for(i in 1:nrow(newfit)){
    newfit[i,] <- respect.cb(fit[i,], cb[i], pos.cb[i], v.2010)
    newres[i,] <- Y[i,] - newfit[i,]
  }
  
  if(plot){
    x11()
    par(mfrow = c(1,3))
    matplot(y = t(Y), x = y.int, type = 'l', main = 'Data', ylim = range(Y))
    matplot(y = t(newfit), x = y.int, type = 'l', main = 'Fitted', ylim = range(Y))
    matplot(y = t(newres), x = y.int, type = 'l', main = 'Residuals')
    if(!is.null(flag)){title(main = flag, outer = T, line = -1)}
  }
  ret <- list(m = m, cov.data = cov.data, res = newres, formula = formula, v.2010 = v.2010)
  return(ret)
}

SM.cum.emi.fit <- function(data, meta, y.int, formulas, plot = F){
  SM <- unique(meta$SM)
  ret <- list()
  for(sm in SM){
    ids <- which(meta$SM == sm)
    temp <- SM.cum.emi.fit_single(data = data$DT.e.co2[ids,], y.int = y.int, plot = plot, formula = formulas[sm], flag = sm)
    ret <- append(ret, list(temp))
    names(ret)[length(ret)]<-sm
  }
  
  return(ret)
}
