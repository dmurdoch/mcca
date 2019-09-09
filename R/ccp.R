ccp=function(y,d,method="multinom",...){

  # transform to numeric&matrix
  y=factor(y)
  y_levels=levels(y)
  y=as.numeric(y)
  d=data.matrix(d)

  #determine label vector pv from y, d and method
  if(method=="multinom"){
    #require(nnet)
    fit = nnet::multinom(y~d,...)
    pv=predict(fit,type='class')
  }else if(method=="tree"){
    #require(rpart)
    y = as.factor(y)
    fit = rpart::rpart(y~d,...)
    pv = predict(fit,type="class")
  }else if(method=="svm"){
    #require(e1071)
    y = as.factor(y)
    fit = e1071::svm(y~d,...)
    pv = predict(fit)
  }else if(method=="lda"){
    #require(MASS)
    fit = MASS::lda(y~d,...)
    predict.test.fit = predict(fit)
    pv = predict.test.fit$class

  }else if(method=="label"){
    temp = sum(d %in% 1:length(unique(y)))
    if (temp!=length(d)){
      cat("ERROR: The input value \"d\" should be a label vector encoded same as y.")
      return(NULL)
    }
    pv=d

  }else if(method=="prob"){
    l=max.col(d)
    for(i in 1:nrow(d)){
      if (length(which(max(d[i,])==d[i,]))>1){
        cat("WARNING: there exists two same max probability in one sample.\n")
        break
      }
    }
    pv=l
  }


  k=length(unique(y))
  ns=sapply(1:k, function(i) sum(y==i) )
  nn=sum(ns)
  ros=ns/nn
  ccps=sapply(1:k, function(i) sum(y==i & pv==i)/ns[i] )
  ccp=sum(ccps*ros)

  df=data.frame(CATEGORIES=sapply(1:k, function(i) y_levels[i]),VALUES=ccps,PREVALENCE=ros)
  result=list(call=match.call(),measure=ccp,table=df)

  class(result)="mcca.ccp"

  return(result)
}
