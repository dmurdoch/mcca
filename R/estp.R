estp <- function (y, m1, m2, acc = "idi", level = 0.95, method = "multinom",B=250,balance=FALSE, ...) {

  k=length(unique(y))
  series=numeric()

  if (acc=="idi"){
    if (balance==FALSE){
    for (b in 1:B){
      nn <- length(y)
      id <- sample(1:nn,nn,replace = T)
      #id <- unique(id)
      while (length(unique(y[id]))<k){
        id <- sample(1:nn,nn,replace = T)
        #id <- unique(id)
      }
      if (class(m1)=="numeric" & class(m2)=="numeric"){
        series[b] <- idi(y=y[id],m1=m1[id],m2=m2[id],method=method,...)
      }else if (class(m1)=="numeric"){
        series[b] <- idi(y=y[id],m1=m1[id],m2=m2[id,],method=method,...)
      }else if (class(m2)=="numeric"){
        series[b] <- idi(y=y[id],m1=m1[id,],m2=m2[id],method=method,...)
      }else {
        series[b] <- idi(y=y[id],m1=m1[id,],m2=m2[id,],method=method,...)
      }

    }
    }
    if (balance==TRUE){
      for (b in 1:B){
        id <- unlist(caret::createResample(y, times = 1))
        if (class(m1)=="numeric" & class(m2)=="numeric"){
          series[b] <- idi(y=y[id],m1=m1[id],m2=m2[id],method=method,...)
        }else if (class(m1)=="numeric"){
          series[b] <- idi(y=y[id],m1=m1[id],m2=m2[id,],method=method,...)
        }else if (class(m2)=="numeric"){
          series[b] <- idi(y=y[id],m1=m1[id,],m2=m2[id],method=method,...)
        }else {
          series[b] <- idi(y=y[id],m1=m1[id,],m2=m2[id,],method=method,...)
        }
      }
    }
    series.sort <- sort(series)
    return(list(value=idi(y=y,m1=m1,m2=m2,method=method,...),
                se=sd(series),
                interval=c(series.sort[ifelse(B*(0.5-level/2)<1,1,B*(0.5-level/2))],series.sort[B*(0.5+level/2)])))
  }
  if (acc=="nri"){
    if (balance==FALSE){
    for (b in 1:B){
      nn <- length(y)
      id <- sample(1:nn,nn,replace = T)
      #id <- unique(id)
      while (length(unique(y[id]))<k){
        id <- sample(1:nn,nn,replace = T)
        #id <- unique(id)
      }
      if (class(m1)=="numeric" & class(m2)=="numeric"){
        series[b] <- nri(y=y[id],m1=m1[id],m2=m2[id],method=method,...)
      }else if (class(m1)=="numeric"){
        series[b] <- nri(y=y[id],m1=m1[id],m2=m2[id,],method=method,...)
      }else if (class(m2)=="numeric"){
        series[b] <- nri(y=y[id],m1=m1[id,],m2=m2[id],method=method,...)
      }else {
        series[b] <- nri(y=y[id],m1=m1[id,],m2=m2[id,],method=method,...)
      }
    }
    }
    if (balance==TRUE){
      for (b in 1:B){
        id <- unlist(caret::createResample(y, times = 1))
        if (class(m1)=="numeric" & class(m2)=="numeric"){
          series[b] <- nri(y=y[id],m1=m1[id],m2=m2[id],method=method,...)
        }else if (class(m1)=="numeric"){
          series[b] <- nri(y=y[id],m1=m1[id],m2=m2[id,],method=method,...)
        }else if (class(m2)=="numeric"){
          series[b] <- nri(y=y[id],m1=m1[id,],m2=m2[id],method=method,...)
        }else {
          series[b] <- nri(y=y[id],m1=m1[id,],m2=m2[id,],method=method,...)
        }
      }
    }
    series.sort <- sort(series)
    return(list(value=nri(y=y,m1=m1,m2=m2,method=method,...),
                se=sd(series),
                interval=c(series.sort[ifelse(B*(0.5-level/2)<1,1,B*(0.5-level/2))],series.sort[B*(0.5+level/2)])))
  }


}
