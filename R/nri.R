nri=function(y,m1,m2,method="multinom",k=3,...){
  num=k
  option=method

  if(num==3){
  #y is the tri-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical

  y=as.numeric(y)
  m1=m1
  m2=m2
  m1=data.matrix(m1)
  m2=data.matrix(m2)

  n1=sum(y==1)
  n2=sum(y==2)
  n3=sum(y==3)
  nn=n1+n2+n3
  ro1=n1/nn
  ro2=n2/nn
  ro3=n3/nn

  #define the id
  if(option=="multinom"){
    #require(nnet)
    fit <- nnet::multinom(y~m1,...)
    pvold=predict(fit,type='class')
    fit <- nnet::multinom(y~m1+m2,...)
    pv=predict(fit,type='class')
  }else if(option=="tree"){
    #require(rpart)
    y <- as.factor(y)
    fit <- rpart::rpart(y~m1,...)
    pvold <- predict(fit,type="class")
    fit <- rpart::rpart(y~m1+m2,...)
    pv <- predict(fit,type="class")
  }else if(option=="svm"){
    #require(e1071)
    y <- as.factor(y)
    fit <- e1071::svm(y~m1,...)
    pvold <- predict(fit)
    fit <- e1071::svm(y~m1+m2,...)
    pv <- predict(fit)
  }else if(option=="lda"){
    #require(MASS)
    fit <- MASS::lda(y~m1,...)
    predict.test.fit <- predict(fit)
    pvold <- predict.test.fit$class
    fit <- MASS::lda(y~m1+m2,...)
    predict.test.fit <- predict(fit)
    pv <- predict.test.fit$class


  }else if(option=="label"){
    pvold=m1
    pv=m2
  }

  nri=(
    (sum(pv==1 & y==1 & pvold!=1)-sum(pv!=1 & y==1 & pvold==1))/n1+
      (sum(pv==2 & y==2 & pvold!=2)-sum(pv!=2 & y==2 & pvold==2))/n2+
      (sum(pv==3 & y==3 & pvold!=3)-sum(pv!=3 & y==3 & pvold==3))/n3
  )/3

  return(nri)
  }else if(num==4){
    #y is the tri-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical

    y=as.numeric(y)
    m1=data.matrix(m1)
    m2=data.matrix(m2)

    n1=sum(y==1)
    n2=sum(y==2)
    n3=sum(y==3)
    n4=sum(y==4)
    nn=n1+n2+n3+n4
    ro1=n1/nn
    ro2=n2/nn
    ro3=n3/nn
    ro4=n4/nn

    #define the id
    if(option=="multinom"){
      #require(nnet)
      fit <- nnet::multinom(y~m1,...)
      pvold=predict(fit,type='class')
      fit <- nnet::multinom(y~m1+m2,...)
      pv=predict(fit,type='class')
    }else if(option=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~m1,...)
      pvold <- predict(fit,type="class")
      fit <- rpart::rpart(y~m1+m2,...)
      pv <- predict(fit,type="class")
    }else if(option=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~m1,...)
      pvold <- predict(fit)
      fit <- e1071::svm(y~m1+m2,...)
      pv <- predict(fit)
    }else if(option=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~m1,...)
      predict.test.fit <- predict(fit)
      pvold <- predict.test.fit$class
      fit <- MASS::lda(y~m1+m2,...)
      predict.test.fit <- predict(fit)
      pv <- predict.test.fit$class


    }else if(option=="label"){
      pvold=m1
      pv=m2
    }

    nri=(
      (sum(pv==1 & y==1 & pvold!=1)-sum(pv!=1 & y==1 & pvold==1))/n1+
        (sum(pv==2 & y==2 & pvold!=2)-sum(pv!=2 & y==2 & pvold==2))/n2+
        (sum(pv==3 & y==3 & pvold!=3)-sum(pv!=3 & y==3 & pvold==3))/n3+
        (sum(pv==4 & y==4 & pvold!=4)-sum(pv!=4 & y==4 & pvold==4))/n4
    )/4

    return(nri)

  }else if(num==2){

    #y is the tri-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical
    
    y=as.numeric(y)
    m1=m1
    m2=m2
    m1=data.matrix(m1)
    m2=data.matrix(m2)
    
    n1=sum(y==1)
    n2=sum(y==2)

    nn=n1+n2
    ro1=n1/nn
    ro2=n2/nn

    
    #define the id
    if(option=="multinom"){
      #require(nnet)
      fit <- nnet::multinom(y~m1,...)
      pvold=predict(fit,type='class')
      fit <- nnet::multinom(y~m1+m2,...)
      pv=predict(fit,type='class')
    }else if(option=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~m1,...)
      pvold <- predict(fit,type="class")
      fit <- rpart::rpart(y~m1+m2,...)
      pv <- predict(fit,type="class")
    }else if(option=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~m1,...)
      pvold <- predict(fit)
      fit <- e1071::svm(y~m1+m2,...)
      pv <- predict(fit)
    }else if(option=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~m1,...)
      predict.test.fit <- predict(fit)
      pvold <- predict.test.fit$class
      fit <- MASS::lda(y~m1+m2,...)
      predict.test.fit <- predict(fit)
      pv <- predict.test.fit$class
      
      
    }else if(option=="label"){
      pvold=m1
      pv=m2
    }
    
    nri=(
      (sum(pv==1 & y==1 & pvold!=1)-sum(pv!=1 & y==1 & pvold==1))/n1+
        (sum(pv==2 & y==2 & pvold!=2)-sum(pv!=2 & y==2 & pvold==2))/n2
    )/2
    
    return(nri)
  }
}

