library(glmnet)
N = 10000
P = 20
lambda = 1

set.seed(489)

data = matrix( rnorm(N*P,mean=0,sd=1), N, P) 

b1 = c(rep(1, 10))
b2 = c(rep(0, 10))
b = c(b1,b2)
y = data%*%b

lasso.mod <- glmnet(data, y, alpha = 1, lambda = lambda)