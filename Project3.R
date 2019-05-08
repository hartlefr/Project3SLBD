library(glmnet)
library(reshape2)
library(ggplot2)

perm <- function(train, perc) {
len = length(train)
if(perc*len > 1){
tmp = train[1:(perc*len)]
tmp = sample(tmp)
result = c(tmp, train[(perc*len+1):len])
return(result)
}
return(train)
}



lambda = c(seq(0,2,0.05))
N = 300
P = 100
set.seed(489)

data = matrix( rnorm(N*P,mean=0,sd=1), N, P) 
training = data[1:(N/2),]
test = data[((N/2)+1):N,]

b1 = c(rep(1, 5))
b2 = c(rep(0, P-5))
b = c(b1,b2)
y = data%*%b
ytest = y[((N/2)+1):N]
ml = c(seq(0,0.6,0.05))

result = rep(0,length(lambda))

for(i in ml){
cvreg = cv.glmnet(data, perm(y,i), lambda = lambda, alpha = 1)

result = rbind(result,cvreg$cvm)

}
#lasso.mod <- glmnet(data, perm(y, 0.3), alpha = 1, lambda = lambda)
#lasso.pred <- predict(lasso.mod, lambda, newx = test)

#sqe = mean((lasso.pred-ytest)^2)^(1/2)


#mean(ytest-lasso.pred)

result = result[2:(length(ml)+1),]
hm = melt(result)

hm[1]<-rep(ml,length(lambda))
hm[,2]<-lambda[hm[,2]]

ggplot(hm, aes(Var1,Var2)) + geom_tile(aes(fill=value), colour = "white") +  scale_fill_gradient(low = "white", high = "steelblue") + labs(x = "Mislabeling", y = "lambda") 


