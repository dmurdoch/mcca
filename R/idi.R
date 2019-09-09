idi=function(y,m1,m2,method="multinom",...){
  if (method!="prob"){
    rsq1=rsq(y=y,d=m1,method=method,...)$measure
    rsq2=rsq(y=y,d=cbind(m1,m2),method=method,...)$measure
  }else{
    rsq1=rsq(y=y,d=m1,method=method,...)$measure
    rsq2=rsq(y=y,d=m2,method=method,...)$measure
  }
  return(rsq2-rsq1)

}

