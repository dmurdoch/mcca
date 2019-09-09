ests <- function (y, d, acc="hum",level=0.95,method = "multinom",B=250,balance=FALSE, ...) {

  series=numeric()
  k=length(unique(y))

  if (acc=="hum"){

    if (balance==FALSE){
    for (b in 1:B){
      nn <- length(y)
      id <- sample(1:nn,nn,replace = T)
      #id <- unique(id)
      while (length(unique(y[id]))<k){
        id <- sample(1:nn,nn,replace = T)
        #id <- unique(id)
      }
      while (min(table(y[id]))<2){
        id <- sample(1:nn,nn,replace = T)
        #id <- unique(id)
      }
      if (class(d)=="numeric"){
      series[b] <- hum(y=y[id],d=d[id],method=method,...)$measure
      }else {
        series[b] <- hum(y=y[id],d=d[id,],method=method,...)$measure
      }
    }
    }
    if (balance==TRUE){
      for (b in 1:B){
        id <- unlist(caret::createResample(y, times = 1))
        if (class(d)=="numeric"){
          series[b] <- hum(y=y[id],d=d[id],method=method,...)$measure
        }else {
          series[b] <- hum(y=y[id],d=d[id,],method=method,...)$measure
        }
      }
    }

    series.sort <- sort(series)
    return(list(value=hum(y=y,d=d,method=method,...)$measure,
                se=sd(series),
                interval=c(series.sort[ifelse(B*(0.5-level/2)<1,1,B*(0.5-level/2))],series.sort[B*(0.5+level/2)])))
  }
  if (acc=="pdi"){
    if (balance==FALSE){
    for (b in 1:B){
      nn <- length(y)
      id <- sample(1:nn,nn,replace = T)
      #id <- unique(id)
      while (length(unique(y[id]))<k){
        id <- sample(1:nn,nn,replace = T)
        #id <- unique(id)
      }
      while (min(table(y[id]))<2){
        id <- sample(1:nn,nn,replace = T)
        #id <- unique(id)
      }
      if (class(d)=="numeric"){
        series[b] <- pdi(y=y[id],d=d[id],method=method,...)$measure
      }else {
        series[b] <- pdi(y=y[id],d=d[id,],method=method,...)$measure
      }
    }
    }
    if (balance==TRUE){
      for (b in 1:B){
        id <- unlist(caret::createResample(y, times = 1))
        if (class(d)=="numeric"){
          series[b] <- pdi(y=y[id],d=d[id],method=method,...)$measure
        }else {
          series[b] <- pdi(y=y[id],d=d[id,],method=method,...)$measure
        }
      }
    }
    series.sort <- sort(series)
    return(list(value=pdi(y=y,d=d,method=method,...)$measure,
                se=sd(series),
                interval=c(series.sort[ifelse(B*(0.5-level/2)<1,1,B*(0.5-level/2))],series.sort[B*(0.5+level/2)])))
  }
  if (acc=="ccp"){
    if (balance==FALSE){
    for (b in 1:B){
      nn <- length(y)
      id <- sample(1:nn,nn,replace = T)
      #id <- unique(id)
      while (length(unique(y[id]))<k){
        id <- sample(1:nn,nn,replace = T)
        #id <- unique(id)
      }
      while (min(table(y[id]))<2){
        id <- sample(1:nn,nn,replace = T)
        #id <- unique(id)
      }
      if (class(d)=="numeric"){
        series[b] <- ccp(y=y[id],d=d[id],method=method,...)$measure
      }else {
        series[b] <- ccp(y=y[id],d=d[id,],method=method,...)$measure
      }
    }
  }
  if (balance==TRUE){
    for (b in 1:B){
      id <- unlist(caret::createResample(y, times = 1))
      if (class(d)=="numeric"){
        series[b] <- ccp(y=y[id],d=d[id],method=method,...)$measure
      }else {
        series[b] <- ccp(y=y[id],d=d[id,],method=method,...)$measure
      }
    }
  }
    series.sort <- sort(series)
    return(list(value=ccp(y=y,d=d,method=method,...)$measure,
                se=sd(series),
                interval=c(series.sort[ifelse(B*(0.5-level/2)<1,1,B*(0.5-level/2))],series.sort[B*(0.5+level/2)])))
  }

  if (acc=="rsq"){
    if (balance==FALSE){
    for (b in 1:B){
      nn <- length(y)
      id <- sample(1:nn,nn,replace = T)
      #id <- unique(id)
      while (length(unique(y[id]))<k){
        id <- sample(1:nn,nn,replace = T)
        #id <- unique(id)
      }
      while (min(table(y[id]))<2){
        id <- sample(1:nn,nn,replace = T)
        #id <- unique(id)
      }
      if (class(d)=="numeric"){
        series[b] <- rsq(y=y[id],d=d[id],method=method,...)$measure
      }else {
        series[b] <- rsq(y=y[id],d=d[id,],method=method,...)$measure
      }
    }
  }
if (balance==TRUE){
  for (b in 1:B){
    id <- unlist(caret::createResample(y, times = 1))
    if (class(d)=="numeric"){
      series[b] <- rsq(y=y[id],d=d[id],method=method,...)$measure
    }else {
      series[b] <- rsq(y=y[id],d=d[id,],method=method,...)$measure
    }
  }
}
    series.sort <- sort(series)
    return(list(value=rsq(y=y,d=d,method=method,...)$measure,
                se=sd(series),
                interval=c(series.sort[ifelse(B*(0.5-level/2)<1,1,B*(0.5-level/2))],series.sort[B*(0.5+level/2)])))
  }

}
