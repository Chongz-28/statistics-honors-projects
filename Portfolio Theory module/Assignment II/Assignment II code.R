#Year: 2019
#Author: Chongo Nkalamo
#Project: PT assignment II

##############################################################################
blacklitterman <- function(v, Sig, Pi, p, omeg, gam, tao){
  H <- solve(tao*Sig) + t(p) %*% solve(omeg) %*% p
  Bl.mean <- solve(H) %*% (solve(tao*Sig) %*% Pi + t(p) %*% solve(omeg) %*% v) I <- seq(1,1,length.out = nrow(H))
  I <- cbind(I)
  invHI <- solve(H)
  #global portfolio
  #wGlob <- as.numeric( 1 / (t(I) %*% solve(Sig) %*% I)) * (solve(Sig) %*% I) wben <- c(rep(1/10,10))
  wben<- as.matrix(wben)
  #blacklitterman weights/portfolio
  wBL <- wben + ((1/gam) * solve(Sig)) %*% (Bl.mean - ( I %*% ((t(I) %*% solve(Sig) %*% Bl.mean) * (1 / (t(I) %*% solve(Sig) %*% I)))))
  #s <- sum(wBL) #sum of weights must be 1 to ensure fully invested
  out.bl <- list(posterior.returns = Bl.mean, blacklitterman.weights = wBL) return(wBL)
}
#############################################################################
library(nloptr)
library(quadprog)
load(file = "~/Documents/Portfolio theory/PT-TAA.RData") Entities <- colnames(tsGRet)
Entities <- Entities[-c(grep('STEFI',Entities))] Entities <- Entities[-c(grep('ALSI',Entities))] #market.port <- tsGRet[,12]
ind <- 1:73
dat <- na.omit(tsGRet[,Entities])
dat.RF <- na.omit(tsGRet[,"STEFI"])
dat.mrkt <- na.omit(tsGRet[,"ALSI"])
test.sample <- dat[-ind,]
test.RF <- tsGRet[-ind,"STEFI"] #riskfree for test test.mrkt <- tsGRet[-ind,"ALSI"] #all share index #IS.weights <- matrix(NA,74,10)
rolling.window <- function(x){
  newdat <- dat[x,]
  rf <- dat.RF[x,]
  newdat <- na.omit(newdat)
  mean.returns <- colMeans(newdat, na.rm = TRUE)
  sd.returns <- colStdevs(newdat, na.rm = TRUE)
  15
  Backtesting a Black-Litterman Portfolio
  Chongo Nkalamo
  
  var.returns <- var(newdat, na.rm = TRUE)
  risk.free.train <- colMeans(rf, na.rm = TRUE)
  #annualize
  mean.returns <- mean.returns*(12)
  sd.returns <- sqrt(12)*sd.returns
  var.returns <- var.returns*(12)
  risk.free.train <- risk.free.train*(12)
  one.vec <- rep(1,length(mean.returns))
  init.wts <- one.vec / length(one.vec)
  IS.weights <- matrix(NA,1,length(mean.returns))
  sharpe <- function(x) {
    return(-(x %*% mean.returns - risk.free.train)
           / sqrt(x %*% var.returns %*% x))
  }
  #ensuring fully invested
  constraint <- function(x) {
    return(x%*%one.vec - 1)
  }
  #make use of sqp to solve for tangency portfolio
  soln <- slsqp(init.wts, fn = sharpe, gr = NULL, # target returns
                lower = rep(0,length(init.wts)), # no short-selling upper = rep(1,length(init.wts)), # no leverage
                heq = constraint, # fully invested constraint function control = list(xtol_rel = 1e-8)) # SQP
                IS.weights <- soln$par
                portfolio.return <- IS.weights %*% (mean.returns) #portfolio returns portfolio.volatiliy <- IS.weights %*% var.returns %*% IS.weights #portfolio(market) risk.free.train
                risk.aversion <- (portfolio.return - risk.free.train) / (var(dat.mrkt[x,]))
                risk.aversion <- as.numeric(risk.aversion)
                portfolio.varcov <- var.returns #we shrink to eliminate shortselling alpha <- 1/2 #shrinkage parameters
                portfolio.varcov.new <- (1+alpha)*portfolio.varcov + alpha*diag(1, 10, 10) portfolio.varcov.new <- diag(diag(portfolio.varcov.new), 10, 10)
                benchmark.weights <- c(rep(1/10,10))
                benchmark.weights <- as.matrix(benchmark.weights)