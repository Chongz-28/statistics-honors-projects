#Year: 2019
#Author(s): Chongo Nkalamo & Yovna Junglee
#Project: Assignment II (Neural Networks)

##############################################################################
#load libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)

#set random seed
set.seed(1)

#custom function to calculate the total number of neural net parameters
get_params <- function(p,q,d){
  
  nodes <- c(p,d,q) # Order all nodes
  
  total <- 0 # Initialise total
  
  n <- length(nodes) # Length of nodes
  for (i in 1:(n-1)){ # update total using d_l * d_(l+1)
    total = total + (nodes[i]*nodes[i+1])
  }
  
  
  # Add bias vectors
  b <- sum(d)+q
  
  total <- b+total
  
  return(total) # Return total
}

#custom feed forward neural net function derived from first principles.
neural_net = function(X,Y,theta,d,lambda)
{
  
  d1 <- d[1] # Get number of nodes in 1st hidden layer
  d2 <- d[2] # Get number of nodes in 2nd hidden layer
  # X- Feature matrix (input/variables)
  #Y - Response matrix 
  # theta - Concatenate all of the weights and biases into par vector
  # lambda - Regularization hyperparameter
  
  
  # input and output dimensions
  p = dim(X)[2]
  q = dim(Y)[2]
  
  
  # populate weight matrices and bias vector
  index  = 1:(p*d1)
  W1     = matrix(theta[index],p,d1) # Input to hidden layer 1 weights
  index  = max(index)+1:(d1*d2)
  W2     = matrix(theta[index], d1 ,d2) # Hidden layer 1 to Hidden layer 2 weights
  index  = max(index)+1:(d2*q)
  W3    =  matrix(theta[index],d2,q) # hidden layer 2 to output weights
  index  = max(index)+1:d1
  b1     = matrix(theta[index],d1,1) # bias vector for the hidden layer 1 nodes
  index  = max(index)+1:d2
  b2     = matrix(theta[index],d2,1) # bias vector for the hidden layer 2 nodes
  index  = max(index)+1:q
  b3     = matrix(theta[index],q,1) # bias vector for the output layer nodes
  
  
  #-------------
  
  ## Matrix form
  
  # X <- Nxp
  # W_L <- D(L)*D(L-1)
  # a_l <- d(l) * N
  # b_l  : d(l) x N
  
  n <- dim(X)[1]
  a0 <- t(X)
  a1 <- sigma(t(W1)%*%a0 + matrix(rep(b1, n), d1 ,n)) 
  a2 <- sigma(t(W2)%*% a1 + matrix(rep(b2,n),d2,n))
  a3 <- sigma(t(W3)%*%a2 + matrix(rep(b3,n),q,n))
  out <- a3
  
  err <- -1*sum((cost(Y,t(out)))) # Error cost function 
  
  
  reg <- lambda*(sum(W1^2) + sum(W2^2)+ sum(W3^2)) # Regularisation
  
  err.pen <- err + reg # Penalized cost function
  
  out <- list(Predicted=out, Error= err.pen, a1=a1, a2=a2 )
  return (out) 
}

# Sigmoid activation function
sigma <- function(x){
  (1/(1+exp(-x)))
  
}

# Cross entropy as classification problem
cost <- function(x, xhat){
  c <- (x*log(xhat))+((1-x)*(log(1-xhat)))
  
  return(c)
}


# Load data

dat1 <- read.table("Breast Cancer Dataset A.txt", header=T)
samp <- sample(1:nrow(dat1), .80*nrow(dat1)) # Randomize training set

X <- dat1[,1:5]
X<- as.matrix(X)
X <- scale(X)

Y<- dat1[,6]
Y <- as.matrix(Y)

train <- dat1[samp,] # Training set
val <- dat1[-samp,] # Validation set

Xtrain <- train[,1:5]
Ytrain <- train[,6]
Xtrain <- as.matrix(Xtrain)
Ytrain <- as.matrix(Ytrain)

Xtrain <- scale(Xtrain)


# Validation set

XVal <- as.matrix(val[,1:5])
XVal <- scale(XVal)
YVal <- as.matrix(val[,6])


# Parameters

# Parameters

# m=2

p <- 5
d2 <- c(2,2)
q <- 1
npars2 <- get_params(p,q,d2)

mod.train2.err <- numeric(0)
val.train2.err <- numeric(0)


lambda <- seq(0.005,1,by=0.01)

for (i in 1:length(lambda)){
  
  
  obj2 <- function(pars){
    d= c(2,2)
    lambda <- lambda[i]
    res =  neural_net(Xtrain,Ytrain, pars,d,lambda)
    res$Error
  }
  
  res2 = nlm(obj2,runif(npars2,-0.5,0.5), iterlim = 1000)
  
  mod.train2 <- neural_net(Xtrain, Ytrain, res2$estimate, d=c(2,2),lambda[i])
  mod.train2.err[i] <- mod.train2$Error
  
  val.train2 <- neural_net(XVal, YVal, res2$estimate, d=c(2,2),lambda[i])
  val.train2.err[i] <- val.train2$Error
  
}



# m= 3

p <- 5
d3 <- c(3,3)
q <- 1
npars3 <- get_params(p,q,d3)




mod.train3.err <- numeric(0)
val.train3.err <- numeric(0)


for (i in 1:length(lambda)){
  obj3 <- function(pars){
    d= c(3,3)
    lambda <- lambda[i]
    res =  neural_net(Xtrain,Ytrain, pars,d,lambda)
    res$Error
  }
  
  res3 = nlm(obj3,runif(npars3, -0.5,0.5), iterlim = 1000)
  mod.train3 <- neural_net(Xtrain, Ytrain, res3$estimate, d=c(3,3),lambda[i])
  mod.train3.err[i] <- mod.train3$Error
  
  val.train3 <- neural_net(XVal, YVal, res3$estimate, d=c(3,3),lambda[i])
  val.train3.err[i]<- val.train3$Error
  
}


# m= 4


p <- 5
d4 <- c(4,4)
q <- 1
npars4 <- get_params(p,q,d4)

val.train4.err <- numeric(0)
mod.train4.err <- numeric(0)

for (i in 1:length(lambda)){      
  obj4 <- function(pars){
    d= c(4,4)
    lambda <- lambda[i]
    res =  neural_net(Xtrain,Ytrain, pars,d,lambda)
    res$Error
  }
  
  res4 = nlm(obj4,runif(npars4, -0.5,0.5), iterlim =1000)
  
  mod.train4 <- neural_net(Xtrain, Ytrain, res4$estimate, d=c(4,4),lambda[i])
  mod.train4.err[i] <- mod.train4$Error
  
  val.train4 <- neural_net(XVal, YVal, res4$estimate, d=c(4,4),lambda[i])
  val.train4.err[i]<- val.train4$Error
  
}
# m= 5


p <- 5
d5 <- c(5,5)
q <- 1
npars5 <- get_params(p,q,d5)

val.train5.err <- numeric(0)
mod.train5.err <- numeric(0)

for (i in 1:length(lambda)){
  obj5 <- function(pars){
    d= c(5,5)
    lambda <- lambda[i]
    res =  neural_net(Xtrain,Ytrain, pars,d,lambda)
    res$Error
  }
  
  res5 = nlm(obj5,runif(npars5, -0.5,0.5), iterlim =1000)
  
  mod.train5 <- neural_net(Xtrain, Ytrain, res5$estimate, d=c(5,5),lambda[i])
  mod.train5.err[i]<- mod.train5$Error
  
  val.train5 <- neural_net(XVal, YVal, res5$estimate, d=c(5,5),lambda[i])
  val.train5.err[i]<- val.train5$Error
}
### 

#cbind(res2$minimum,res3$minimum,res4$minimum,res5$minimum) ## Choose res 4
#cbind(res2$iterations,res3$iterations,res4$iterations,res5$iterations) ## all less than 1000 
# so convergence

m <-rep(c(2,3,4,5), each=length(lambda))
train.errors <- c(mod.train2.err,mod.train3.err,mod.train4.err,mod.train5.err)
train.errors <- as.data.frame(cbind(m,lambda,Training=train.errors))
train.errors$m <- as.factor(m)
val.errors <- c(val.train2.err,val.train3.err,val.train4.err,val.train5.err)
val.errors <- as.data.frame(cbind(m,lambda,Validation=val.errors))
val.errors$m<- as.factor(m)

p1<- ggplot(data=train.errors, aes(x=lambda, y=Training, color = m)) + geom_line()+
  theme_bw()


p2 <- ggplot(data=val.errors, aes(x=lambda, y=Validation, color = m)) + geom_line()+
  theme_bw()

grid.arrange(p1,p2)

#testing
data.b <- read.table("Breast Cancer Dataset B.txt", header=T)
X.test<- as.matrix(data.b)
X.test <- scale(X.test)
y.dummy <- cbind(rep(1,nrow(xlab))) #dummy column vector to feed the neural network

YVal <- YVal[1:69,] #just to feed the neural net
YVal <- cbind(YVal)

#use function to get weights and biases from training
num <- get_params(ncol(xlab),ncol(YVal),c(2,2))
obj.pred<- function(pars){
  d= c(2,2)
  lambda <- 0.005
  res =  neural_net(X,Y, pars,d,lambda) # Training from the whole dataset
  return(res$Error)
}
res.pred = nlm(obj.pred,runif(num,-.5,.5), iterlim =1000)

#carrying out predictions.
pred <- neural_net(X.test, YVal, res.pred$estimate, d=c(2,2),0.005)
pre.predicted <- t(pred$Predicted)
predicted<- ifelse(pre.predicted >= 0.5,1,0)
predicted <- as.data.frame(cbind(predicted))
#View(predicted)
colnames(predicted)=c("Predictions")

#writing or exporting final predictions.
write.table(predicted, file="Cancer_Pred_NKLCHO001.csv",row.names=F, quote=F, sep=',')


#######
#QUESTION II
#Under the hood

#set random seed
set.seed(1)

# Load data
dat2 <- read.table("UnderTheHood.txt", header=T)
datX <- as.matrix(dat2[,1:2])
datY <- as.matrix(dat2[,3])

samp1 <- sample(1:500, 400)

datX.train <- as.matrix(datX[samp1,])
datY.train <- as.matrix(datY[samp1,])

datX.test <- as.matrix(datX[-samp1,])
datY.test <- as.matrix(datY[-samp1,])


# Set parameters
p1 <- 2
d.hood <- c(6,2)
q <- 1
npars.2 <- get_params(p1,q,d.hood)

lambda1 <- c(0.0025, seq(0.005,1,by=0.01))


hood.train <- numeric(length(lambda1))
hood.test <- numeric(length(lambda1))

for (i in 1:length(lambda1))  {  
  obj.fit <- function(pars){
    d= d.hood
    lambda <- lambda1[i]
    res =  neural_net(datX.train,datY.train, pars,d,lambda)
    res$Error
  }
  
  # Fit model
  res.fit <- nlm(obj.fit, runif(npars.2,-1,1),iterlim=1000)
  
  mod.fit <- neural_net(datX.train, datY.train, theta=res.fit$estimate, d=c(6,2),lambda1[i])
  hood.train[i] <- mod.fit$Error
  
  test.fit <- neural_net(datX.test, datY.test, theta=res.fit$estimate, d=c(6,2),lambda1[i])
  hood.test[i] <- test.fit$Error
}

err.hood <- cbind(lambda1, Training=hood.train,Validation=hood.test)
err.hood <- as.data.frame(err.hood)
err.hood <- cbind(lambda1,melt(err.hood[,2:3]))



ggplot(data=err.hood, aes(x=lambda1,y=value,color=variable))+geom_line()+
  labs(x=expression(lambda), y= "Cross Entropy Error", color="")+theme_bw()
###

# Generate grid of inputs
obj.fit <- function(pars){
  d= d.hood
  lambda <- 0.0025
  res =  neural_net(datX,datY, pars,d,lambda)
  res$Error
}

# Fit model
res.fit <- nlm(obj.fit, runif(npars.2,-1,1),iterlim=1000)
mod.fit <- neural_net(datX, datY, theta=res.fit$estimate, d=c(6,2),0.0025)
#hood.train[i] <- mod.fit$Error
# Generate grid of inputs
x1<- seq(-1,1,length=100)
x2 <- seq(-1,1,length=100)

Xin <- as.matrix(expand.grid(x1,x2)) # Create matrix of inputs
y <- as.matrix(seq(-1,1,length=100*100)) 
# Arbitrary y val so neural net works


# Colour function
colfunc = function(x, n=100){
  xx = seq(min(x), max(x), length=n)
  ii = findInterval(x,xx)
  colr <- colorRampPalette((c('yellow','red','orange')))
  cols = colr(n)
  return(cols[ii])
}

# Fit neural net to input grid
mod.new <- neural_net(Xin, y, theta=res.fit$estimate, d=c(6,2),0.0025)

a1 <- mod.new$a1 # Hidden layer 1 output
a2 <- mod.new$a2 # Hidden layer 2 output
out <- mod.new$Predicted # Final layer output

#testing
# Layer 1
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE))
plot(NULL, xlim=c(-1,1), ylim=c(-1,1), ylab="x2", xlab="x1")
points(Xin[,1],Xin[,2], col=colfunc(a1[1,]), pch=16)
plot(NULL, xlim=c(-1,1), ylim=c(-1,1), ylab="x2", xlab="x1")
points(Xin[,1],Xin[,2], col=colfunc(a1[2,]), pch=16)
plot(NULL, xlim=c(-1,1), ylim=c(-1,1), ylab="x2", xlab="x1")
points(Xin[,1],Xin[,2], col=colfunc(a1[3,]), pch=16)
plot(NULL, xlim=c(-1,1), ylim=c(-1,1), ylab="x2", xlab="x1")
points(Xin[,1],Xin[,2], col=colfunc(a1[4,]), pch=16)
plot(NULL, xlim=c(-1,1), ylim=c(-1,1), ylab="x2", xlab="x1")
points(Xin[,1],Xin[,2], col=colfunc(a1[5,]), pch=16)
plot(NULL, xlim=c(-1,1), ylim=c(-1,1), ylab="x2", xlab="x1")
points(Xin[,1],Xin[,2], col=colfunc(a1[6,]), pch=16)

# Layer 2
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
plot(NULL, xlim=c(-1,1), ylim=c(-1,1),ylab="x2", xlab="x1")
points(Xin[,1],Xin[,2], col=colfunc(a2[1,]), pch=16)
plot(NULL, xlim=c(-1,1), ylim=c(-1,1), ylab="x2", xlab="x1")
points(Xin[,1],Xin[,2], col=colfunc(a2[2,]), pch=16)

# Output layer
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
plot(NULL, xlim=c(-1,1), ylim=c(-1,1),ylab="x2", xlab="x1")
points(Xin[,1],Xin[,2], col=colfunc(out), pch=16)
plot(NULL, xlim=c(-1,1), ylim=c(-1,1),ylab="x2", xlab="x1")
points(datX[,1],datX[,2], col=colfunc(mod.fit$Predicted), pch=16)