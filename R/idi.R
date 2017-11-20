idi=function(y,m1,m2,method="multinom",k=3){
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
    fit <- nnet::multinom(y~m1,maxit = 1000,MaxNWts = 2000,trace=F)
    predict.test.probs <- predict(fit,type='probs')
    predict.test.df <- data.frame(predict.test.probs)
    #extract the probablity assessment vector
    pvold=predict.test.df

    fit <- nnet::multinom(y~m1+m2,maxit = 1000,MaxNWts = 2000,trace=F)
    predict.test.probs <- predict(fit,type='probs')
    predict.test.df <- data.frame(predict.test.probs)
    #extract the probablity assessment vector
    pv=predict.test.df

  }else if(option=="tree"){
    #require(rpart)
    y <- as.factor(y)
    fit <- rpart::rpart(y~m1,control = rpart::rpart.control(minsplit = 4))
    predict.test.probs <- predict(fit,type='prob')
    predict.test.df <- data.frame(predict.test.probs)
    #extract the probablity assessment vector
    pvold=predict.test.df

    fit <- rpart::rpart(y~m1+m2,control = rpart::rpart.control(minsplit = 4))
    predict.test.probs <- predict(fit,type='prob')
    predict.test.df <- data.frame(predict.test.probs)
    #extract the probablity assessment vector
    pv=predict.test.df

  }else if(option=="svm"){
    #require(e1071)
    y <- as.factor(y)
    fit <- e1071::svm(y~m1,type="C",kernel="radial",cost=1,scale=T,probability = T)
    predict.test <- predict(fit,m1,probability = T)
    predict.test <- attr(predict.test,"probabilities")
    predict.test.df <- data.frame(predict.test)
    #extract the probablity assessment vector
    pvold=predict.test.df[c("X1","X2","X3")]
    fit <- e1071::svm(y~m1+m2,type="C",kernel="radial",cost=1,scale=T,probability = T)
    predict.test <- predict(fit,cbind(m1,m2),probability = T)
    predict.test <- attr(predict.test,"probabilities")
    predict.test.df <- data.frame(predict.test)
    #extract the probablity assessment vector
    pv=predict.test.df[c("X1","X2","X3")]
  }else if(option=="lda"){
    #require(MASS)
    fit <- MASS::lda(y~m1)
    predict.test.probs <- predict(fit,type='probs')
    predict.test.fit <- predict(fit)
    predict.test <- predict.test.fit$posterior
    predict.test.df <- data.frame(predict.test)
    #extract the probablity assessment vector
    pvold=predict.test.df
    fit <- MASS::lda(y~m1+m2)
    predict.test.probs <- predict(fit,type='probs')
    predict.test.fit <- predict(fit)
    predict.test <- predict.test.fit$posterior
    predict.test.df <- data.frame(predict.test)
    #extract the probablity assessment vector
    pv=predict.test.df


  }else if(option=="prob"){
    pvold=m1
    pv=m2
  }

  idi=(
    var(pv[,1])/(ro1*(1-ro1))+
      var(pv[,2])/(ro2*(1-ro2))+
      var(pv[,3])/(ro3*(1-ro3))
  )/3-
    (
      var(pvold[,1])/(ro1*(1-ro1))+
        var(pvold[,2])/(ro2*(1-ro2))+
        var(pvold[,3])/(ro3*(1-ro3))
    )/3
  return(idi)
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
      fit <- nnet::multinom(y~m1,maxit = 1000,MaxNWts = 2000,trace=F)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pvold=predict.test.df

      fit <- nnet::multinom(y~m1+m2,maxit = 1000,MaxNWts = 2000,trace=F)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pv=predict.test.df

    }else if(option=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~m1,control = rpart::rpart.control(minsplit = 4))
      predict.test.probs <- predict(fit,type='prob')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pvold=predict.test.df

      fit <- rpart::rpart(y~m1+m2,control = rpart::rpart.control(minsplit = 4))
      predict.test.probs <- predict(fit,type='prob')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pv=predict.test.df

    }else if(option=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~m1,type="C",kernel="radial",cost=1,scale=T,probability = T)
      predict.test <- predict(fit,m1,probability = T)
      predict.test <- attr(predict.test,"probabilities")
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pvold=predict.test.df[c("X1","X2","X3","X4")]
      fit <- e1071::svm(y~m1+m2,type="C",kernel="radial",cost=1,scale=T,probability = T)
      predict.test <- predict(fit,cbind(m1,m2),probability = T)
      predict.test <- attr(predict.test,"probabilities")
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pv=predict.test.df[c("X1","X2","X3","X4")]
    }else if(option=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~m1)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.fit <- predict(fit)
      predict.test <- predict.test.fit$posterior
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pvold=predict.test.df
      fit <- MASS::lda(y~m1+m2)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.fit <- predict(fit)
      predict.test <- predict.test.fit$posterior
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pv=predict.test.df


    }else if(option=="prob"){
      pvold=m1
      pv=m2
    }

    idi=(
      var(pv[,1])/(ro1*(1-ro1))+
        var(pv[,2])/(ro2*(1-ro2))+
        var(pv[,3])/(ro3*(1-ro3))+
        var(pv[,4])/(ro4*(1-ro4))
    )/4-
      (
        var(pvold[,1])/(ro1*(1-ro1))+
          var(pvold[,2])/(ro2*(1-ro2))+
          var(pvold[,3])/(ro3*(1-ro3))+
          var(pvold[,4])/(ro4*(1-ro4))
      )/4

    return(idi)
  }
}
