nri=function(y,m1,m2,method="multinom",...){
  if (method!="prob"&method!="label"){
    ccp1=ccp(y=y,d=m1,method=method,...)$measure
    ccp2=ccp(y=y,d=cbind(m1,m2),method=method,...)$measure
  }else{
    ccp1=ccp(y=y,d=m1,method=method,...)$measure
    ccp2=ccp(y=y,d=m2,method=method,...)$measure
  }
  return(ccp2-ccp1)

}

