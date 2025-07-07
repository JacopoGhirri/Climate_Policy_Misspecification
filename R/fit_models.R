get.CV_error <- function(choose, data, meta, y.int, formula, tolerance, sm){
  ids <- which(meta$SM == sm)
  rmses <- numeric(length(ids))
  
  cb <- get.cb(data$DT.e.co2[ids,])
  pos.cb <- get.pos.cb(data$DT.e.co2[ids,])
  cores <- detectCores()
  cl <- makeCluster(cores[1]-1)
  registerDoParallel(cl)
  
  if(choose == 'emi'){
    rmses <- foreach(i = 1:length(ids), .combine = c, .export = c("SM.cum.emi.fit_single", "predict.cumemi.clean", "respect.cb", "cumulative.emissions.fitmodel", "gdpl.fitmodel", "predict.fosr", "get.cb", "get.pos.cb", "get_cumulative"))%dopar%{
      temp <- SM.cum.emi.fit_single(data = data$DT.e.co2[ids[-i],], y.int = y.int, plot = F, formula = formula, flag = sm)
      pred <- predict.cumemi.clean(temp, cb[i], pos.cb[i])
      res <- (cumsum(as.numeric(data$DT.e.co2[ids[i],]))-pred)^2
      sqrt(mean(as.numeric(res)))
    }
  }
  if(choose == 'mc'){
    rmses <- foreach(i = 1:length(ids), .combine = c, .export = c("SM.gdpl.fit_single", "predict.mitcost.clean", "gdpl.fitmodel", "predict.fosr"), .packages = "KernSmooth")%dopar%{
      temp <- SM.gdpl.fit_single(data = data$DT.miticost[ids[-i],], cb = cb[-i], pos.cb = pos.cb[-i], y.int = y.int, plot = F, formula = formula, flag = sm, tolerance = tolerance)
      pred <- predict.mitcost.clean(temp, cb[i], pos.cb[i])
      res <- (data$DT.miticost[ids[i],]-pred)^2
      sqrt(mean(as.numeric(res)))
    }
  }
  stopCluster(cl)
  ret <- mean(rmses)
  return(ret)
}

CV.model <- function(datablock, choose, formulas){
  data <- datablock$data
  y.int <- datablock$y.int
  tol <- datablock$tol
  ms <- unique(data$meta$SM)[order(unique(data$meta$SM))]
  
  m <- matrix(nrow = length(formulas), ncol = length(ms))
  rownames(m) <- formulas
  colnames(m) <- ms
  
  for(f in 1:length(formulas)){
    for(i in 1:length(ms)){
      invisible(capture.output(m[f, i] <- get.CV_error(choose = choose, data = data$data, meta = data$meta, y.int = y.int, formula = formulas[f], tolerance = tol, sm = ms[i]) ))
    }
  }
  return(m)
}

find.formulas.CV <- function(datablock){
  formulas <- c("~ I(cb*1e-2) + I((pos.cb - cb)*1e-2)",
                "~ I(cb*1e-2) + I(((pos.cb - cb)*1e-2)^2) + I(exp(pos.cb/cb))",
                "~ I(cb*1e-2) + I(sqrt((pos.cb - cb)*1e-2))",
                "~ I(cb*1e-2) + I(pos.cb*1e-2) + I(sqrt((pos.cb - cb)*1e-2))",
                "~ I(cb*1e-2) + I((pos.cb*1e-2)^2) + I(sqrt((pos.cb - cb)*1e-2))",
                "~ I(cb*1e-2) + I((pos.cb*1e-2)^2) + I(sqrt((pos.cb - cb)*1e-2)) + I(exp(pos.cb/cb))",
                "~ I(cb*1e-2) + I((pos.cb*1e-2)^2) + I((pos.cb - cb)*1e-2) + I(exp(pos.cb/cb))",
                "~ I(cb*1e-2) + I((pos.cb*1e-2)^2) + I((pos.cb - cb)*1e-2)",
                "~ I(cb*1e-2) + I((pos.cb*1e-2)^2) + I(((pos.cb - cb)*1e-2)^2)",
                "~ I(cb*1e-2) + I((pos.cb*1e-2)^2) + I(sqrt((pos.cb - cb)*1e-2)) + I(pos.cb/cb)",
                "~ I(cb*1e-2) + I((cb*1e-2)^2) + I((pos.cb - cb)*1e-2)",
                "~ I(cb*1e-2) + I(((pos.cb - cb)*1e-2)^2) + I((pos.cb - cb)*1e-2)")
  
  m <- CV.model(datablock, 'mc', formulas)
  write.table(m, file = 'data/CV_miticost_results.csv')
  formulas <- c("~ I(cb*1e-2) + I((pos.cb - cb)*1e-2)",
                "~ I(cb*1e-2) + I((pos.cb - cb)*1e-2) + I(1/(1+exp(-cb*1e-2)))",
                "~ I(cb*1e-2) + I((pos.cb - cb)*1e-2) + I(1/(1+exp(-cb*1e-3)))",
                "~ I(cb*1e-2) + I((pos.cb - cb)*1e-2) + I(log(cb*1e-2))",
                "~ I(cb*1e-2) + I((pos.cb - cb)*1e-2) + I((cb*1e-2)^2)",
                "~ I(cb*1e-2) + I((pos.cb - cb)*1e-2) + I((pos.cb*1e-2)^2)",
                "~ I(cb*1e-2) + I((pos.cb - cb)*1e-2) + I(((pos.cb - cb)*1e-2)^2)",
                "~ I(cb*1e-2) + I((pos.cb - cb)*1e-2) + I(1/(1+exp(-cb*1e-2))) + I(log(cb*1e-2))")
  
  m <- CV.model(datablock, 'emi', formulas)
  write.table(m, file = 'data/CV_cumemi_results.csv')
}

read.formulas <- function(){
  mit.CV <- read.table('data/CV_miticost_results.csv')
  
  formula.miticost <- character(length=ncol(mit.CV))
  names(formula.miticost) <- colnames(mit.CV)
  for(mi in 1:ncol(mit.CV)){
    formula.miticost[mi] <- rownames(mit.CV)[which(mit.CV[,mi] == min(mit.CV[,mi]))]
  }
  
  emi.CV <- read.table('data/CV_cumemi_results.csv')
  
  formula.emi <- character(length=ncol(emi.CV))
  names(formula.emi) <- colnames(emi.CV)
  for(mi in 1:ncol(emi.CV)){
    formula.emi[mi] <- rownames(emi.CV)[which(emi.CV[,mi] == min(emi.CV[,mi], na.rm = T))]
  }
  
  #Similar performance but greater interpretability
  formula.emi['GEM']<-"~ I(cb*1e-2) + I((pos.cb - cb)*1e-2) + I(log(cb*1e-2))"
  formula.emi['REMIND']<-"~ I(cb*1e-2) + I((pos.cb - cb)*1e-2) + I(log(cb*1e-2))"
  formula.emi['WITCH']<-"~ I(cb*1e-2) + I((pos.cb - cb)*1e-2) + I(log(cb*1e-2))"
  
  return(list(mitcost = formula.miticost, emi = formula.emi))
}

fit.models <- function(datablock, formula.miticost = NULL, formula.emi = NULL, plot = F){
  if(is.null(formula.miticost)){
    formula.miticost <- read.formulas()$mitcost
  }
  if(is.null(formula.emi)){
    formula.miticost <- read.formulas()$emi
  }
  data <- datablock$data
  y.int <- datablock$y.int
  tol <- datablock$tol
  model.micost <- SM.gdpl.fit(data = data$data, meta = data$meta, y.int = y.int, plot = plot, formulas = formula.miticost, tolerance = tol)
  model.cumemi <- SM.cum.emi.fit(data = data$data, meta = data$meta, y.int = y.int, plot = plot, formulas = formula.emi)
  return(list(model.micost = model.micost, model.cumemi = model.cumemi, y.int = y.int))
}
