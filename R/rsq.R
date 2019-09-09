rsq=function(y,d,method="multinom",...){

  y=factor(y)
  y_levels=levels(y)
  y=as.numeric(y)
  d=data.matrix(d)
  num=length(unique(y))

  pp=pm(y=y,d=d,method=method,...)

  #########################################
  if(num==3){

    n1=sum(y==1)
    n2=sum(y==2)
    n3=sum(y==3)
    nn=n1+n2+n3
    ro1=n1/nn
    ro2=n2/nn
    ro3=n3/nn

    rsq=(
      stats::var(pp[,1])/(ro1*(1-ro1))+
        stats::var(pp[,2])/(ro2*(1-ro2))+
        stats::var(pp[,3])/(ro3*(1-ro3))
    )/3/nn*(nn-1)

    rsqs=c(stats::var(pp[,1])/(ro1*(1-ro1)),stats::var(pp[,2])/(ro2*(1-ro2)),stats::var(pp[,3])/(ro3*(1-ro3)))/nn*(nn-1)

    df=data.frame(CATEGORIES=sapply(1:num, function(i) y_levels[i]),VALUES=rsqs)
    result=list(call=match.call(),measure=rsq,table=df)
    class(result)="mcca.rsq"
    return(result)


  }else if(num==4){

    n1=sum(y==1)
    n2=sum(y==2)
    n3=sum(y==3)
    n4=sum(y==4)
    nn=n1+n2+n3+n4
    ro1=n1/nn
    ro2=n2/nn
    ro3=n3/nn
    ro4=n4/nn

    rsq=(stats::var(pp[,1])/(ro1*(1-ro1))+stats::var(pp[,2])/(ro2*(1-ro2))+stats::var(pp[,3])/(ro3*(1-ro3))+stats::var(pp[,4])/(ro4*(1-ro4)))/4/nn*(nn-1)

    rsqs=c(stats::var(pp[,1])/(ro1*(1-ro1)),stats::var(pp[,2])/(ro2*(1-ro2)),stats::var(pp[,3])/(ro3*(1-ro3)),stats::var(pp[,4])/(ro4*(1-ro4)))/nn*(nn-1)

    df=data.frame(CATEGORIES=sapply(1:num, function(i) y_levels[i]),VALUES=rsqs)
    result=list(call=match.call(),measure=rsq,table=df)
    class(result)="mcca.rsq"
    return(result)



  }else if(num==2){
    n1=sum(y==1)
    n2=sum(y==2)

    nn=n1+n2
    ro1=n1/nn
    ro2=n2/nn


    rsq=(stats::var(pp[,1])/(ro1*(1-ro1))+stats::var(pp[,2])/(ro2*(1-ro2)))/2/nn*(nn-1)
    rsqs=c(stats::var(pp[,1])/(ro1*(1-ro1)),stats::var(pp[,2])/(ro2*(1-ro2)))/nn*(nn-1)

    df=data.frame(CATEGORIES=sapply(1:num, function(i) y_levels[i]),VALUES=rsqs)
    result=list(call=match.call(),measure=rsq,table=df)
    class(result)="mcca.rsq"
    return(result)  }
}
