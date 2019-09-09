pdi=function(y,d,method="multinom",...){

  y=factor(y)
  y_levels=levels(y)
  y=as.numeric(y)
  d=data.matrix(d)
  num=length(unique(y))

  pp=pm(y=y,d=d,method=method,...)

  #########################################
  if(num==3){
    n1=which(y==1) #return the label
    n2=which(y==2)
    n3=which(y==3)
    pp1=pp[n1,]
    pp2=pp[n2,]
    pp3=pp[n3,]
    pdi1=0
    pdi2=0
    pdi3=0
    for(i in 1:length(n1)){
      pdi1=pdi1+sum(pp1[i,1]>pp2[,1])*sum(pp1[i,1]>pp3[,1])
    }
    for(i in 1:length(n2)){
      pdi2=pdi2+sum(pp2[i,2]>pp1[,2])*sum(pp2[i,2]>pp3[,2])
    }
    for(i in 1:length(n3)){
      pdi3=pdi3+sum(pp3[i,3]>pp1[,3])*sum(pp3[i,3]>pp2[,3])
    }
    pdi=(pdi1+pdi2+pdi3)/(3*length(n1)*length(n2)*length(n3))

    pdis=c(pdi1,pdi2,pdi3)/(length(n1)*length(n2)*length(n3))
    df=data.frame(CATEGORIES=sapply(1:num, function(i) y_levels[i]),VALUES=pdis)
    result=list(call=match.call(),measure=pdi,table=df)
    class(result)="mcca.pdi"
    return(result)

  #########################################
  }else if(num==4){
    n1=which(y==1) #return the label
    n2=which(y==2)
    n3=which(y==3)
    n4=which(y==4)
    pp1=pp[n1,]
    pp2=pp[n2,]
    pp3=pp[n3,]
    pp4=pp[n4,]
    pdi1=0
    pdi2=0
    pdi3=0
    pdi4=0
    for(i in 1:length(n1)){
      pdi1=pdi1+sum(pp1[i,1]>pp2[,1])*sum(pp1[i,1]>pp3[,1])*sum(pp1[i,1]>pp4[,1])
    }
    for(i in 1:length(n2)){
      pdi2=pdi2+sum(pp2[i,2]>pp1[,2])*sum(pp2[i,2]>pp3[,2])*sum(pp2[i,2]>pp4[,2])
    }
    for(i in 1:length(n3)){
      pdi3=pdi3+sum(pp3[i,3]>pp1[,3])*sum(pp3[i,3]>pp2[,3])*sum(pp3[i,3]>pp4[,3])
    }
    for(i in 1:length(n4)){
      pdi4=pdi4+sum(pp4[i,4]>pp1[,4])*sum(pp4[i,4]>pp2[,4])*sum(pp4[i,4]>pp3[,4])
    }
    pdi=(pdi1+pdi2+pdi3+pdi4)/(4*length(n1)*length(n2)*length(n3)*length(n4))

    pdis=c(pdi1,pdi2,pdi3,pdi4)/(length(n1)*length(n2)*length(n3)*length(n4))

    df=data.frame(CATEGORIES=sapply(1:num, function(i) y_levels[i]),VALUES=pdis)
    result=list(call=match.call(),measure=pdi,table=df)
    class(result)="mcca.pdi"
    return(result)

    #########################################

  }else if(num==2){
    n1=which(y==1) #return the label
    n2=which(y==2)
    pp1=pp[n1,]
    pp2=pp[n2,]

    pdi1=0
    pdi2=0

    for(i in 1:length(n1)){
      pdi1=pdi1+sum(pp1[i,1]>pp2[,1])+sum(pp1[i,1]==pp2[,1])
    }
    for(i in 1:length(n2)){
      pdi2=pdi2+sum(pp2[i,2]>pp1[,2])
    }

    pdi=(pdi1+pdi2)/(2*length(n1)*length(n2))

    pdis=c(pdi1,pdi2)/(length(n1)*length(n2))
    df=data.frame(CATEGORIES=sapply(1:num, function(i) y_levels[i]),VALUES=pdis)
    result=list(call=match.call(),measure=pdi,table=df)
    class(result)="mcca.pdi"
    return(result)
  }
}

