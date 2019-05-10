library(glmnet)
library(reshape2)
library(ggplot2)
library(tidyverse)

perm <- function(train, perc) {
  len = length(train)
  if(perc*len > 1){
    tmp = train[1:(perc*len)]
    tmp = sample(tmp)
    result = c(tmp, train[floor((perc*len+1)):len])
    return(result)
  }
  return(train)
}



lambda = c(seq(0,0.5,0.01))
#lambda = c(seq(0,1,0.1))
N = 3000
P = 100
set.seed(489)

data = matrix( rnorm(N*P,mean=0,sd=1), N, P) 
training = data[1:(0.7*N),]
test = data[(0.7*N+1):N,]

b1 = c(rep(1, 5))
b2 = c(rep(0, P-5))
b = c(b1,b2)
y = data%*%b
ytraining <- y[1:(0.7*N)]
ytest <- y[(0.7*N+1):N]
ml = c(seq(0,0.55,0.02))
triesPerMl <- 50


result = matrix(rep(0,length(lambda)),1,length(lambda))

for(i in 1:length(ml)){
  for(j in 1:triesPerMl){ 
    #cvreg <- cv.glmnet(data, perm(y,ml[i]), lambda = lambda, alpha = 1)
    #result[i,] <- result[i,]+cvreg$cvm
    lasso.mod <- glmnet(training, perm(ytraining, ml[i]), alpha = 1, lambda = lambda)
    lasso.pred <- predict(lasso.mod, rev(lambda), newx = test)
    sme<-apply(lasso.pred,2,function(x) (mean((x-ytest)^2))^(1/2))
    result[i,] <- result[i,]+sme
  }
  result[i,] <- result[i,]/triesPerMl
  result <- rbind(result,rep(0,length(lambda)))
  
}

#lasso.mod <- glmnet(data, perm(y, 0.3), alpha = 1, lambda = lambda)
#lasso.pred <- predict(lasso.mod, lambda, newx = test)

#sqe = mean((lasso.pred-ytest)^2)^(1/2)


#mean(ytest-lasso.pred)

result = result[1:length(ml),]

bestLambda <- lambda[length(lambda)+1-apply(result, 1,which.min)]


hm = melt(result)

hm[,1] <- ml[hm[,1]]
hm[,2] <- lambda[length(lambda)+1-hm[,2]]

ggplot(hm, aes(Var1,Var2)) + geom_tile(aes(fill=value), colour = "white") +  scale_fill_gradient(low = "white", high = "steelblue") + labs(x = "Mislabeling", y = "lambda") 
ggplot(tibble(mislabeling=ml,lambda=bestLambda),aes(mislabeling,bestLambda))+geom_point()
