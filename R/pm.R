pm=function(y,d,method="multinom",...){

  y=factor(y)
  y_levels=levels(y)
  y=as.numeric(y)
  d=data.matrix(d)
  num=length(unique(y))

  if(num==3){
    #y is the tri-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical
    #d is the continuous marker, turn out to be the probability matrix when method="prob"

    #define the id
    if(method=="multinom"){
      #require(nnet)
      fit <- nnet::multinom(y~d,...)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(method=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~d,...)
      predict.test.probs <- predict(fit,type='prob')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(method=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~d,...,probability = T)
      predict.test <- predict(fit,d,probability = T)
      predict.test <- attr(predict.test,"probabilities")
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df[c("X1","X2","X3")]
    }else if(method=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~d,...)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.fit <- predict(fit)
      predict.test <- predict.test.fit$posterior
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df

    }else if(method=="prob"){
      pp_sum <- apply(d,1,sum)
      a <- pp_sum<0.999 | pp_sum>1.001
      b <- sum(a)
      if (b!=0){
        cat("ERROR: The input value \"d\" should be a probability matrix.")
        return(NULL)
      }
      pp=d
    }

    return(pp)

  }else if(num==4){
    #y is the four-category multinomial response, must be of dimension n by 4, d is the continuous marker
    #eg. try generating y=rmultinom(1,size=1,prob=cbind(1,exp(d%*%1),exp(d%*%2),exp(d%*%3))) need MASS

    #define the id
    if(method=="multinom"){
      #require(nnet)
      fit <- nnet::multinom(y~d,...)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(method=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~d,...)
      predict.test.probs <- predict(fit,type='prob')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(method=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~d,...,probability = T)
      predict.test <- predict(fit,d,probability = T)
      predict.test <- attr(predict.test,"probabilities")
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df[c("X1","X2","X3","X4")]
    }else if(method=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~d,...)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.fit <- predict(fit)
      predict.test <- predict.test.fit$posterior
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df

    }else if(method=="prob"){
      pp_sum <- apply(d,1,sum)
      a <- pp_sum<0.999 | pp_sum>1.001
      b <- sum(a)
      if (b!=0){
        cat("ERROR: The input value \"d\" should be a probability matrix.")
        return(NULL)
      }
      pp=d
    }
    return(pp)
  }else if(num==2){
    #y is the tri-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical
    #d is the continuous marker, turn out to be the probability matrix when method="prob"

    #define the id
    if(method=="multinom"){
      #require(nnet)
      fit <- nnet::multinom(y~d,...)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
      pp <- data.frame(1-pp,pp)
    }else if(method=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~d,...)
      predict.test.probs <- predict(fit,type='prob')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(method=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~d,...,probability = T)
      predict.test <- predict(fit,d,probability = T)
      predict.test <- attr(predict.test,"probabilities")
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df[c("X1","X2")]
    }else if(method=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~d,...)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.fit <- predict(fit)
      predict.test <- predict.test.fit$posterior
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df

    }else if(method=="prob"){
      pp_sum <- apply(d,1,sum)
      a <- pp_sum<0.999 | pp_sum>1.001
      b <- sum(a)
      if (b!=0){
        cat("ERROR: The input value \"d\" should be a probability matrix.")
        #   return(NULL)
      }
      pp=d
    }
    return(pp)
  }

}
