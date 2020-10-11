## Load the Tactical Asset Allocation data from EXCEL
# Filename: JSE-IND-S10-RCPP.R (Demonstrates use of C++ for big-data)
# See Also: JSE-IND-S10-RJAVA.R (Demonstrates use of JVM for big-data)
#
# 1. ICB Industrial Level Indices 
# 2. ALBI (All Bond Index (ALBI) Total Return Index (TRI) Data)
# 3. Money Market Data:  JIBAR and STEFI TRI
# 4. Various Indices: JSE Growth, JSE Value, JSE ALSI, JSE SRI
#
# Situation: Load data from *.csv file and convert into timeSeries object data
#
# Big-Data Issues to consider:
#
#  A: xlsx using rJava (set the Heap size correctly and tune GC)
#  B: openxlsx using rcpp [this file]
# 
# This script file address and demonstrates option B

# Author : Chongo Nkalamo (2019)

## 0. Clearr environemnt and remove all plots
rm(list=ls()) # clear environment
# if(!is.null(dev.list())) dev.off() # remove all RStudio plots

## 2. Packages
# 2.1.  install.packages("<name of the package>")
# 2.2.  library("<name of the package>")
# 2.3.  any(grepl("<name of your package>", 
#       installed.packages()))
## Use the openxlsx package (Rcpp)
install.packages("openxlsx")
install.packages("Rcpp")
install.packages("timeSeries")
library(openxlsx)
library(Rcpp)
library(zoo)
library(xts)
library(timeSeries)
library(rbenchmark)
library(nloptr) # for SQP
library(quadprog) # for QP
## 3. Paths
# 3.1.  setwd("<location of your dataset>")
rootp0 <- getwd()
setwd("..") # move up one level in the directory tree
rootp <- getwd() #set root path
setwd(path.expand('~'))
filen <- "PT-TAA-JSE-Daily-1994-2017.xlsx"
fpath <- "/Bongz/Documents/Portfolio theory/" 
ffilen  <- paste(rootp,fpath,filen,sep="") 

## 4. load the dataset by sheet
#importing data in excel sheet with 4 spread sheets 
dfS <- list()
for (i in 1:4){
  dfS[[i]] <- read.xlsx(ffilen, sheet = i,detectDates = TRUE)
}
dim(dfS[[3]]) #setting into a dataframe
# if you need to convert column `A` to date then use
# df$A <- as.POSIXct(df$A,format="%H:%M:%S")

## 5. Keep only the specified list of Tickers
Entities = c('X1','STEFI','ALBI','J203','J500',sprintf("J5%d",seq(10,90,by = 10))) 
Items    = c('Date','TRI','Stefi')
# find Tickers in colnames, and
# TRI at the attribute type and reference and join
for (i in c(1,2,3,4)){
  # logical FALSE vector
  tI0 <- logical(length = length(colnames(dfS[[i]]))) #Vector of falses
  tI1 <- tI0 #Vector of falses
  # find the Entities in the data frame
  for (j in 1:length(Entities)){
    tI0 <- tI0 | grepl(Entities[j],colnames(dfS[[i]]))
  }
  # find the Items in the data frame
  for (k in 1:length(Items)){
    tI1 <- tI1 | grepl(Items[[k]],dfS[[i]][2,])
  }
  # combined the logical indices
  tI <- tI0 & tI1
  # remove the columns not required
  dfS[[i]] <- dfS[[i]][,tI]
  # remove the first two rows (as they are not dates)
  dfS[[i]] <- dfS[[i]][-c(1,2),]
  # rename the first column name to Dates
  names(dfS[[i]])[1] <- "Date"
  # clean up the remaining column names
  newColNames <- strsplit(colnames(dfS[[i]]),":")
  for (m in 2:length(newColNames)){
    names(dfS[[i]])[m] <- newColNames[[m]][1]
  }
}
# get the dimensions of the remaining data-frame list elements
for (i in 1:length(dfS)){
  print(dim(dfS[[i]])) # throw to console
}

## 6. Clean and convert into a single timeSeries object
# 6.1. Initialise the timeSeries object with first data frame 
iN <- 1
tsTAA <- timeSeries(dfS[[iN]][,2:ncol(dfS[[iN]])],as.Date(dfS[[iN]][,1])) 
print(dim(tsTAA)) # print dimensions to console
#  correct the column names
# 6.3. Concatenate additional timeSeries columns on to the object
for (i in c(2,3,4)){
  # consider iterative merging using inherited zoo properties
  # the first column is the Date column the rest are features we do this
  # to ensure that the dates are correctly aligned when time series are merged
  tsTAA <- cbind(tsTAA,timeSeries(dfS[[i]][,2:ncol(dfS[[i]])],as.Date(dfS[[i]][,1])))
  print(dim(tsTAA))
  print(colnames(tsTAA))
}
# 6.4 Set the units to TRI
setFinCenter(tsTAA) <- "Johannesburg"
# 6.5 Fix the colname errors introduce during the cbind
names(tsTAA)[grep("TS.1.1",names(tsTAA))] <- "ALBI"
names(tsTAA)[grep("TS.1.2",names(tsTAA))] <- "STEFI"
names(tsTAA)[grep("TS.1",names(tsTAA))] <- "ALSI"

## 7. Convert from Daily Sampled Data to Monthly Sampled Data
# 7.1. Decimate the daily data to monthly data
tsTAA <- daily2monthly(tsTAA)  #orders your dates chronologically
# 7.2 Visualise the data on a single plot
#   combine and visualise and prettify the y-axis
plot(tsTAA,plot.type = c("single"), 
     format = "auto", 
     at=pretty(tsTAA), 
     ylab = "Returns",
     main = "TRI for sectors", las = 1)

## 8. Compute returns Geometric (Arithmetic)
# We need to manage the missing data NA (Not A Number)
# 8.1 remove all header NA data first
# omit NA 
tsTAA <- na.omit(tsTAA) # but we will revisit this in the next section
#   remove all rows with NAs and compute the index 

tsIdx <- index2wealth(tsTAA)
#   ensure that the date range is complete
#   explicitly compute the daily geometric returns

tsGRet <- diff(log(tsIdx)) #differences a time series 

## 6. Plot two plots on the same figure
#   matrix of figures
par(mfrow=c(2, 1)) 
#   set the scaling for the first graph
par(mar = c(bottom=1.5, 5.1, top=4, 2.1))
#   plot the single set of time-series
plot(tsIdx,plot.type = c("single"), 
     format = "auto", 
     ylab = "Price",
     main="Monthly Price Index",
     cex.main=0.7, cex.lab=0.7, cex.axis=0.7, las = 1,xlab = "Time/Date") 
#   include the legend
legend("bottomleft",names(tsIdx),cex=0.7)
#   set the scaling for the second graph
par(mar = c(bottom=4, 5.1, top=1.5, 2.1))
#   plot the time-series
plot(tsGRet,plot.type = c("single"), 
     format="%B\n%Y",
     at=pretty(tsGRet), 
     ylab = "Returns",
     main = "Monthly Sampled Geometric Returns",
     cex.main=0.7, cex.lab=0.7, cex.axis=0.7)
#   include the legend
legend("bottomleft",names(tsGRet),cex=0.7)

## 11. Save the workspace and with the prepared data
save(tsGRet,tsTAA,tsIdx,file = "~/Documents/Portfolio theory/PT-TAA.RData")
save.image
unlink("PT-TAA.RData")
unlink(".RData")


#EXPERIMENT 1

##########################################################

load(file = "~/Documents/Portfolio theory/PT-TAA.RData") #ALREADY PREPROCEESED DATA

Entities <- colnames(tsGRet)
# remove the money market assets (we will compute excess returns!)
#our tickers
Entities <- Entities[-c(grep('STEFI',Entities))]
Entities <- Entities[-c(grep('ALSI',Entities))]

#splitting the data
ind <- 1:74
train.sample <- tsGRet[ind,]
test.sample <- tsGRet[-ind,]

#extract risk free rates (stefi) for the training and test period
train.RF <- train.sample[,"STEFI"] #risk free rate for training period.
test.RF <- test.sample[,"STEFI"] #risk free for test period.

#final test and training samples that include only our tickers
train.sample <- train.sample[,Entities]
test.sample <- test.sample[,Entities]

#compute geometric means
mean.returns <- colMeans(train.sample, na.rm = TRUE) 
sd.returns <- colStdevs(train.sample, na.rm = TRUE)  
var.returns <- var(train.sample, na.rm = TRUE)
risk.free.train <- colMeans(train.RF, na.rm = TRUE)
any(is.na(train.sample)) #checking if we still have missing values

#annualize data
mean.returns <- mean.returns*12
sd.returns <- sqrt(12)*sd.returns
var.returns <- var.returns*12
risk.free.train <- risk.free.train*12

##############################
#plot
plot(sd.returns, mean.returns, 
     ylab = "Expected Ann. Return [%]", xlab = "Ann. Volatility [%]", 
     main = "Hist. Risk & Ret. + Eff. Front.", xlim = c(0,0.3))
grid()
text(sd.returns, mean.returns,labels = names(mean.returns), cex= 1, pos = 4)

##############################

#tangency portfolio
one.vec <- rep(1,length(mean.returns))
init.wts <- one.vec / length(one.vec)
IS.weights <- matrix(NA,1,length(mean.returns))


sharpe <- function(x) {
  return(-(x %*% mean.returns - risk.free.train) / sqrt(x %*% var.returns %*% x))
}

#ensuring fully invested
constraint <- function(x) {
  return(x%*%one.vec - 1)
}

#make use of sqp to solve for tangency portfolio
soln <- slsqp(init.wts, fn = sharpe, gr = NULL, # target returns
              lower = rep(0,length(init.wts)), # no short-selling
              upper = rep(1,length(init.wts)), # no leverage
              heq = constraint, # fully invested constraint function
              control = list(xtol_rel = 1e-8)) # SQP
IS.weights <- soln$par
print(IS.weights)

#compute the sharpe ratio for this period
portfolio.return <- IS.weights %*% (mean.returns) 
#portfolio returns for buy and hold period
portfolio.volatiliy <- IS.weights %*% var.returns %*% IS.weights
#portfolio variance for buy and hold period
portfolio.sharp <- (portfolio.return-risk.free.train) 
/ sqrt(portfolio.volatiliy)

#TEST DATA
#new period, observations 75 - 148
#exponentiate the returns from the test period. 
#These will give the relative % changes from the beignning of the period to end.
compound.returns <- exp(test.sample) 
test.RF <- exp(test.RF)

#loop through
#get out a weight matrix
OOS.weights <- matrix(rep(NA), nrow(test.sample),length(names(test.sample))) #initialize
OOS.weights[1,] <- IS.weights #set intial, in-sample weights in the weight matrix

for(i in 2:nrow(test.sample)){
  OOS.weights[i,] <- as.numeric(compound.returns[i,] * OOS.weights[i-1,])
  / as.numeric(compound.returns[i,]) %*% OOS.weights[i-1,]
  
}

###########################
#cumprod(rtrns.test)
#fts <-timeSeries(rtrns.test)
#plot(ts)
############################

portfolio.returns.test <- matrix(NA,74,1)
for (i in 1:nrow(compound.returns)) {
  portfolio.returns.test[i] <- OOS.weights%*%t(compound.returns[i,])
}

portfolio.returns.test <- timeSeries(portfolio.returns.test)
plot(cumprod(portfolio.returns.test), type ="l", col = "blue", 
     ylab = "Cumulative Returns", xlab ="Time")

#annualized
portfolio.returns.prod <- prod(portfolio.returns.test^(12/74))
portfolio.variance.test <- var(portfolio.returns.test*(12))
test.RF <- prod(test.RF^(12/74)) #annualized

portfolio.test.SR <- 
  (portfolio.returns.prod - test.RF) / sqrt(portfolio.variance.test)

#plots
par(mfrow = c(1,2))
wts4 <- compound.returns%*%as.matrix(IS.weights)
plot(cumprod(wts4) ,col = "red",type = "l",
     ylab = "cumulative returns", las =1
     ,xlab = "time")
legend("topleft",legend = c("uncompounded"), col =c("red"), lty = 1)
plot(cumprod(portfolio.returns.test), type ="l", 
     col = "blue", ylab = "cumulative returns",las =1
     ,xlab = "time")
legend("topleft",legend = c("compounded"), col =c("blue"), lty = 1)

#time series plot
plot.ts(portfolio.returns.test, ylab = "Returns", col = "red")

#constant sharpe ratio weights
wts4 <- compound.returns%*%as.matrix(IS.weights)
returns.constant <- prod(wts4^(12/74))
vol.constant <- var(wts4*12)
sr.constant <- (returns.constant-test.RF)/sqrt(vol.constant) 
#GIVES A SHARPE RATIO OF 0.376

#EXPERIMENT 2

#experiment 2
# LOAD ALREADY PREPROCEESED DATA
load(file = "~/Documents/Portfolio theory/PT-TAA.RData")

Entities <- colnames(tsGRet)
# remove the money market assets (we will compute excess returns!)
#our tickers
Entities <- Entities[-c(grep('STEFI',Entities))]
Entities <- Entities[-c(grep('ALSI',Entities))]

ind <- 1:73
dat <- na.omit(tsGRet[,Entities])
dat.RF <- tsGRet[,"STEFI"]
test.sample <- dat[-ind,]
test.RF <- tsGRet[-ind,"STEFI"] #stefi for training

#IS.weights <- matrix(NA,74,10)
rolling.window <- function(x){
  
  newdat <- dat[x,]
  rf <- dat.RF[x,]
  newdat <- na.omit(newdat)
  mean.returns <- colMeans(newdat, na.rm = TRUE) 
  sd.returns <- colStdevs(newdat, na.rm = TRUE)  
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
                lower = rep(0,length(init.wts)), # no short-selling
                upper = rep(1,length(init.wts)), # no leverage
                heq = constraint, # fully invested constraint function
                control = list(xtol_rel = 1e-8)) # SQP
  IS.weights <- soln$par
  #print(IS.weights)
  portfolio.return <- IS.weights %*% mean.returns 
  #portfolio returns for buy and hold period
  portfolio.volatiliy <- IS.weights %*% var.returns %*% IS.weights 
  #portfolio variance for buy and hold period
  portfolio.sharp <- (portfolio.return-risk.free.train) 
  / sqrt(portfolio.volatiliy) 
  #return(portfolio.sharp)
  return(IS.weights)
}

compound.returns <- exp(test.sample) 
test.RF <- exp(test.RF)
IS.weights <- matrix(NA,74,10)
rtrns.vec <- c(rep(0,74))
for (i in 1:74) {
  IS.weights[i,] = rolling.window(i:(73+i))
  rtrns.vec[i] <- IS.weights[i,]%*%t(compound.returns[i,])
}



portfolio.returns.prod <- prod(rtrns.vec^(12/74))
portfolio.variance.test <- var(rtrns.vec^(12))
test.RF <- prod(test.RF^(12/74)) #annualized

plot.ts(rtrns.vec, ylab = "returns",col ="blue",xlim =c(0,60))
#time series of the performance
par(mfrow = c(1,1))

portfolio.test.SR <- (portfolio.returns.prod - test.RF)
/ sqrt(portfolio.variance.test)



lines(cumprod(rtrns.vec), col ="red")
legend("topleft", legend = c("exp I", "exp II"),
       col = c("blue","red"), lty = 1)