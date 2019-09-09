hum=function(y,d,method="multinom",...){

  y_raw=y
  y=factor(y)
  y_levels=levels(y)
  y=as.numeric(y)
  d=data.matrix(d)
  num=length(unique(y))

  pp=pm(y=y,d=d,method=method,...)

  if(num==3){
  x1=which(y==1) #return the label
  x2=which(y==2)
  x3=which(y==3)
  n=length(y)

  #n is the sample size
  a=matrix(0,n,3);
  one1=a;
  one1[,1]=1;
  one2=a;
  one2[,2]=1;
  one3=a;
  one3[,3]=1;

  #get cr value
  dd1=pp-one1;
  dd2=pp-one2;
  dd3=pp-one3;

  jd1=sqrt(dd1[,1]^2+dd1[,2]^2+dd1[,3]^2);
  jd2=sqrt(dd2[,1]^2+dd2[,2]^2+dd2[,3]^2);
  jd3=sqrt(dd3[,1]^2+dd3[,2]^2+dd3[,3]^2);
  jd1=exp(jd1);
  jd2=exp(jd2);
  jd3=exp(jd3);

  mt1=kronecker(jd1[x1]%*%t(jd2[x2]),jd3[x3]);
  mt2=kronecker(jd1[x1]%*%t(jd3[x2]),jd2[x3]);
  mt3=kronecker(jd2[x1]%*%t(jd1[x2]),jd3[x3]);
  mt4=kronecker(jd2[x1]%*%t(jd3[x2]),jd1[x3]);
  mt5=kronecker(jd3[x1]%*%t(jd2[x2]),jd1[x3]);
  mt6=kronecker(jd3[x1]%*%t(jd1[x2]),jd2[x3]);

  cr=sum(mt1==pmin(pmin(pmin(pmin(pmin(mt1, mt2), mt3), mt4), mt5), mt6)); #min of every number of mt1-mt6 is mt1

  #hypervolume under ROC manifold value
  hum=cr/(length(x1)*length(x2)*length(x3));

  result=list(call=match.call(),measure=hum,pm=pp,y=y_raw)
  class(result)="mcca.hum"

  return(result)

  }else if(num==4){

    n=length(y)
    #n is the sample size
    a=matrix(0,n,4);
    one1=a;
    one1[,1]=1;
    one2=a;
    one2[,2]=1;
    one3=a;
    one3[,3]=1;
    one4=a;
    one4[,4]=1;

    x1=which(y==1);
    x2=which(y==2);
    x3=which(y==3);
    x4=which(y==4);
    n1=sum(y==1)
    n2=sum(y==2)
    n3=sum(y==3)
    n4=sum(y==4)

    dd1=pp-one1;
    dd2=pp-one2;
    dd3=pp-one3;
    dd4=pp-one4;

    jd1=sqrt(dd1[,1]^2+dd1[,2]^2+dd1[,3]^2+dd1[,4]^2);
    jd2=sqrt(dd2[,1]^2+dd2[,2]^2+dd2[,3]^2+dd2[,4]^2);
    jd3=sqrt(dd3[,1]^2+dd3[,2]^2+dd3[,3]^2+dd3[,4]^2);
    jd4=sqrt(dd4[,1]^2+dd4[,2]^2+dd4[,3]^2+dd4[,4]^2);
    jd1=exp(jd1);
    jd2=exp(jd2);
    jd3=exp(jd3);
    jd4=exp(jd4);

    mt1=kronecker(kronecker(jd1[x1]%*%t(jd2[x2]),jd3[x3]),jd4[x4]);
    mt7=kronecker(kronecker(jd1[x1]%*%t(jd2[x2]),jd4[x3]),jd3[x4]);
    mt2=kronecker(kronecker(jd1[x1]%*%t(jd3[x2]),jd2[x3]),jd4[x4]);
    mt8=kronecker(kronecker(jd1[x1]%*%t(jd3[x2]),jd4[x3]),jd2[x4]);
    mt9=kronecker(kronecker(jd1[x1]%*%t(jd4[x2]),jd2[x3]),jd3[x4]);
    mt10=kronecker(kronecker(jd1[x1]%*%t(jd4[x2]),jd3[x3]),jd2[x4]);
    mt3=kronecker(kronecker(jd2[x1]%*%t(jd1[x2]),jd3[x3]),jd4[x4]);
    mt11=kronecker(kronecker(jd2[x1]%*%t(jd1[x2]),jd4[x3]),jd3[x4]);
    mt4=kronecker(kronecker(jd2[x1]%*%t(jd3[x2]),jd1[x3]),jd4[x4]);
    mt12=kronecker(kronecker(jd2[x1]%*%t(jd3[x2]),jd4[x3]),jd1[x4]);
    mt13=kronecker(kronecker(jd2[x1]%*%t(jd4[x2]),jd3[x3]),jd1[x4]);
    mt14=kronecker(kronecker(jd2[x1]%*%t(jd4[x2]),jd1[x3]),jd3[x4]);
    mt5=kronecker(kronecker(jd3[x1]%*%t(jd2[x2]),jd1[x3]),jd4[x4]);
    mt15=kronecker(kronecker(jd3[x1]%*%t(jd2[x2]),jd4[x3]),jd1[x4]);
    mt6=kronecker(kronecker(jd3[x1]%*%t(jd1[x2]),jd2[x3]),jd4[x4]);
    mt16=kronecker(kronecker(jd3[x1]%*%t(jd1[x2]),jd4[x3]),jd2[x4]);
    mt23=kronecker(kronecker(jd3[x1]%*%t(jd4[x2]),jd1[x3]),jd2[x4]);
    mt24=kronecker(kronecker(jd3[x1]%*%t(jd4[x2]),jd2[x3]),jd1[x4]);
    mt17=kronecker(kronecker(jd4[x1]%*%t(jd1[x2]),jd2[x3]),jd3[x4]);
    mt18=kronecker(kronecker(jd4[x1]%*%t(jd1[x2]),jd3[x3]),jd2[x4]);
    mt19=kronecker(kronecker(jd4[x1]%*%t(jd2[x2]),jd1[x3]),jd3[x4]);
    mt20=kronecker(kronecker(jd4[x1]%*%t(jd2[x2]),jd3[x3]),jd1[x4]);
    mt21=kronecker(kronecker(jd4[x1]%*%t(jd3[x2]),jd1[x3]),jd2[x4]);
    mt22=kronecker(kronecker(jd4[x1]%*%t(jd3[x2]),jd2[x3]),jd1[x4]);

    cr=sum(mt1==pmin(mt7,pmin(mt8,pmin(mt9,pmin(mt10,pmin(mt11,pmin(mt12,pmin(mt13,pmin(mt14,pmin(mt15,pmin(mt16,pmin(mt17,pmin(mt18,pmin(mt19,pmin(mt20,pmin(mt21,pmin(mt22,pmin(mt23,pmin(mt24,pmin(pmin(pmin(pmin(pmin(mt1, mt2), mt3), mt4), mt5), mt6))))))))))))))))))));


    #hypervolume under ROC manifold
    hum=cr/(n1*n2*n3*n4);

    result=list(call=match.call(),measure=hum,pm=pp,y=y_raw)
    class(result)="mcca.hum"

    return(result)

  }else if(num==2){

      x1=which(y==1) #return the label
      x2=which(y==2)
      n=length(y)

      #n is the sample size
      a=matrix(0,n,2);
      one1=a;
      one1[,1]=1;
      one2=a;
      one2[,2]=1;

      result=list(call=match.call(),measure=as.numeric(pROC::roc(y ~ pp[,1])$auc),pm=pp,y=y_raw)
      class(result)="mcca.hum"

      return(result)
      }

}
