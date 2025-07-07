predict.fosr <- function(m, coefrow){
  components <- m$est.func
  for(j in 1:ncol(components)){
    components[,j] <- components[,j]*coefrow[j]
  }
  ret <- colSums(t(components))
  return(ret)
}

#hard constraining of cumulative emission curves to climate targets
respect.cb <- function(fit, cb, pos.cb, v.2010){
  cb <- cb - v.2010
  pos.cb <- pos.cb - v.2010
  
  if(is.null(nrow(fit))){
    if(pos.cb < cb+0.5){
      fit <- fit*pos.cb/max(fit)
      fit[(which(fit==max(fit))[1]):length(fit)] <- pos.cb
    }else{
      if((which(fit == max(fit))[1]) == length(fit)){
        #during error sampling exceptional conditions may cause overshoot bump to disappear, since it is a constraint of the problem we artificially add one
        fit <- fit -(pos.cb-fit[9])*((0:18)^2)/81 + (pos.cb-fit[9])*(2/9)*(0:18)
        if((which(fit == max(fit))[1]) == length(fit)){
            fit[(which(diff(fit)<0)[1]+1):length(fit)][(which(diff(fit[(which(diff(fit)<0)[1]+1):length(fit)])>0)[1] +1):length(fit[(which(diff(fit)<0)[1]+1):length(fit)])] <- min(fit[(which(diff(fit)<0)[1] + 1):length(fit)])
            fit <- fit -(pos.cb-fit[9])*((0:18)^2)/81 + (pos.cb-fit[9])*(2/9)*(0:18)
          }
      }
      ts <- (which(fit == max(fit))[1])-1
      tl <- length(fit)-1
      b <- fit[length(fit)]
      a <- max(fit)
      
      y <- (pos.cb - cb*a/b)/(ts - tl*a/b)
      x <- (cb - y*tl)/b
      
      fit <- fit*x + y*(0:(length(fit)-1))
      while(max(fit)-pos.cb> 2.5){
        ts <- (which(fit == max(fit))[1])-1
        tl <- length(fit)-1
        b <- fit[length(fit)]
        a <- max(fit)
        y <- (pos.cb - cb*a/b)/(ts - tl*a/b)
        x <- (cb - y*tl)/b
       fit <- fit*x + y*(0:(length(fit)-1))
      }
      fit[which(fit>pos.cb)] <- pos.cb
      fit[(which(fit>=pos.cb-0.5)[1]):length(fit)][fit[(which(fit>=pos.cb-0.5)[1]):length(fit)]<cb]<- cb
    }
    return(fit+v.2010)
  }else{
    if(length(cb)==1){
      cb <- replicate(nrow(fit), cb)
      pos.cb <- replicate(nrow(fit), pos.cb)
    }
    for(rr in 1:nrow(fit)){
      if(pos.cb[rr] < cb[rr]+0.5){
        fit[rr,] <- fit[rr,]*pos.cb[rr]/max(fit[rr,])
        fit[rr,][(which(fit[rr,]==max(fit[rr,]))[1]):length(fit[rr,])] <- pos.cb[rr]
      }else{
        if((which(fit[rr,] == max(fit[rr,]))[1]) == length(fit[rr,])){
          #during error sampling exceptional conditions may cause overshoot bump to disappear, since it is a constraint of the problem we artificially add one
          fit[rr,] <- fit[rr,] -(pos.cb[rr]-fit[rr,9])*((0:18)^2)/81 + (pos.cb[rr]-fit[rr,9])*(2/9)*(0:18)
          if((which(fit[rr,] == max(fit[rr,]))[1]) == length(fit[rr,]) & any(diff(fit[rr,])<0)){
            fit[rr,(which(diff(fit[rr,])<0)[1]+1):length(fit[rr,])][(which(diff(fit[rr,(which(diff(fit[rr,])<0)[1]+1):length(fit[rr,])])>0)[1] +1):length(fit[rr,(which(diff(fit[rr,])<0)[1]+1):length(fit[rr,])])] <- min(fit[rr,(which(diff(fit[rr,])<0)[1] + 1):length(fit[rr,])])
            fit[rr,] <- fit[rr,] -(pos.cb[rr]-fit[rr,9])*((0:18)^2)/81 + (pos.cb[rr]-fit[rr,9])*(2/9)*(0:18)
          }else  if((which(fit[rr,] == max(fit[rr,]))[1]) == length(fit[rr,])){
            fit[rr,] <- fit[rr,] -(pos.cb[rr]-fit[rr,9])*((0:18)^2)/81 + (pos.cb[rr]-fit[rr,9])*(2/9)*(0:18)
            if((which(fit[rr,] == max(fit[rr,]))[1]) == length(fit[rr,]) & any(diff(fit[rr,])<0)){
              fit[rr,(which(diff(fit[rr,])<0)[1]+1):length(fit[rr,])][(which(diff(fit[rr,(which(diff(fit[rr,])<0)[1]+1):length(fit[rr,])])>0)[1] +1):length(fit[rr,(which(diff(fit[rr,])<0)[1]+1):length(fit[rr,])])] <- min(fit[rr,(which(diff(fit[rr,])<0)[1] + 1):length(fit[rr,])])
              fit[rr,] <- fit[rr,] -(pos.cb[rr]-fit[rr,9])*((0:18)^2)/81 + (pos.cb[rr]-fit[rr,9])*(2/9)*(0:18)
            }
          }
        }
        ts <- (which(fit[rr,] == max(fit[rr,]))[1])-1
        tl <- length(fit[rr,])-1
        b <- fit[rr,][length(fit[rr,])]
        a <- max(fit[rr,])
        
        y <- (pos.cb[rr] - cb[rr]*a/b)/(ts - tl*a/b)
        x <- (cb[rr] - y*tl)/b
        
        
        fit[rr,] <- fit[rr,]*x + y*(0:(length(fit[rr,])-1))
        while(max(fit[rr,])-pos.cb[rr]> 2.5){
          ts <- (which(fit[rr,] == max(fit[rr,]))[1])-1
          tl <- length(fit[rr,])-1
          b <- fit[rr,][length(fit[rr,])]
          a <- max(fit[rr,])
          y <- (pos.cb[rr] - cb[rr]*a/b)/(ts - tl*a/b)
          x <- (cb[rr] - y*tl)/b
          fit[rr,] <- fit[rr,]*x + y*(0:(length(fit[rr,])-1))
        }
        fit[rr,which(fit[rr,]>pos.cb[rr])] <- pos.cb[rr]
        fit[rr,(which(fit[rr,]>=pos.cb[rr]-0.5)[1]):length(fit[rr,])][fit[rr,(which(fit[rr,]>=pos.cb[rr]-0.5)[1]):length(fit[rr,])]<cb[rr]]<- cb[rr]
      }
    }
    return(fit+v.2010)
  }
}
