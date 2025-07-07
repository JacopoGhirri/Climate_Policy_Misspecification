#fit and sample from Gaussian Process

fit.residual.distribution <- function(mod){
  y <- mod$res
  p <- dim(y)[2]
  n <- dim(y)[1]
  
  #priors
  mu0 <- replicate(p,0)
  l0 <- 1
  n0 <- p+2
  P <- cov(y) + diag(p)*1e-3
  
  #utils
  ym <- colMeans(y)
  S <- (t(y) - ym)%*%t((t(y) - ym))
  
  #posteriors
  ln <- l0 + n
  nn <- n0 + n
  mn <- (l0*mu0 + n*ym)/ln
  Pn <- P + S + (l0*n/ln)*(ym%*%t(ym))
  
  return(list(m = mn, l = ln, n = nn, P = Pn))
}

fit.residual.distribution.CONSTR <- function(mod){
  y <- mod$res
  p <- dim(y)[2]-2
  n <- dim(y)[1]
  
  y <- y[,-c(1,dim(y)[2])]
  #priors
  mu0 <- replicate(p,0)
  l0 <- 1
  n0 <- p+2
  P <- cov(y) + diag(p)*1e-3
  
  #utils
  ym <- colMeans(y)
  S <- (t(y) - ym)%*%t((t(y) - ym))
  
  #posteriors
  ln <- l0 + n
  nn <- n0 + n
  mn <- (l0*mu0 + n*ym)/ln
  Pn <- P + S + (l0*n/ln)*(ym%*%t(ym))
  
  return(list(m = mn, l = ln, n = nn, P = Pn))
}

sample.from.residuals <- function(params, n=100, seed = NULL){
  p <- length(params$m)
  if(!(is.null(seed))){
    set.seed(seed = seed)
  }
  Ps <- matrixsampling::rinvwishart(n = n, nu = params$n, Omega = params$P)
  ys <- matrix(nrow = n, ncol = p)
  for(i in 1:n){
    ms <- mvrnorm(mu = params$m, Sigma = Ps[,,i]/params$l)
    ys[i,] <- mvrnorm(mu = ms, Sigma = Ps[,,i])
  }
  return(ys)
}
