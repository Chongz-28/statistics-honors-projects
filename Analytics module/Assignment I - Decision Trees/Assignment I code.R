#Year: 2019
#Authors: Chongo Nkalamo & Yovna Junglee
#Project: Analytics assignment I

########################################
#QUESTION 1

# Load Packages required
#library
library(tidyr)
library(tidyverse)
library(randomForest)
library(gbm)
library(tree)
library(ggcorrplot)
library(knitr)
library(leaps)
library(doParallel)
library(foreach)
# Set seed to project number
set.seed(23)

#loading data
load("clean_data.RData")
par(mfrow = c(1,2))
hist(df$SalePrice, xlab = "SalePrice", 
     main ="Histogram of SalePrice", freq = F, col ="yellow")
hist(log(df$SalePrice), xlab = "log(SalePrice)", 
     main ="Histogram of log(SalePrice)", freq = F, col ="yellow")

load("clean_data.RData") # Load data set
inds <- 1:81
num.vars <- sapply(inds, function(x){is.numeric(df[,x])})
ind.num.vars <- which(num.vars) # Indices of all numeric variables
num.df <- df[,c(ind.num.vars)] # DF of all numeric vars
cor <- cor(num.df[,-1]) # Correlation(without X)
cor <- as.data.frame(cor)
cor.SALES <- cor[37,] # Find correlation values with sale price
high.ind <- which(abs(cor.SALES)>.5) # Extract only highly correlated values
high.cor.sales <- cor[c(high.ind),c(high.ind)] # Rebuild correlation matrix
min <- min(high.cor.sales)
max <- max(high.cor.sales)
# lab = T gives vals
ggcorrplot(high.cor.sales)+ scale_fill_gradient2(
  limit = c(min-0.02,max), low = "white", high = "darkblue",
  mid = "purple", midpoint = (min+max)/2)


plot(df$SalePrice~df$GrLivArea,
     col=ifelse(df$GrLivArea>=4000,"red","black"),
     xlab="GrLivArea",ylab="Sale Prices", pch=16)
abline(v=4000, lty=2, col='red')

load("FINALDATA.RData")

#data wranggling

load("factors.RData")

library(ggplot2)
library(reshape2)

melt.data <- melt(df.factors)

# 1-10
df.melts1= melt(df.factors[,1:10], id.vars = c("SalePrice"),
                measure.vars = colnames(df.factors)[2:10])
p <- ggplot(data = df.melts1, aes(x=value, y=SalePrice)) + 
  geom_boxplot(aes(fill="orange"))
+theme(axis.text.x = 
         element_text(angle = 90, hjust = 1))
p + facet_wrap( ~ variable, scales="free")


df.final <- na.omit(df.keep) # Remove NAS

df.final <- df.final %>% filter(GrLivArea <=4000) # Ref decock pdf # outliers

# Set training and test data set
train=sample(1:nrow(df.final), nrow(df.final)*.85)
df.test = df.final[-train,]
df.train = df.final[train,]

# Number of parameters
p<-22

tree1 <- tree(log(SalePrice)~., data = df.train)
# summary(tree1)
plot(tree1)
text(tree1,pretty=0)

#cost complexity pruning.
#we grew the full tree, now we prune.
# plot(prune.tree(tree1))

#extract alpha/cost complexity values
prune.tree(tree1)
alphas<- prune.tree(tree1)$k
alpha <- c(-Inf,1.782,2.691,2.839,3.697,4.403,8.645,12.279)
nodes <- c(10,9,8,7,6,5,4,3)
kable(rbind(alpha,nodes), format = "latex")

bag.train = randomForest(log(SalePrice) ~.,
                         data=df.train,mtry = 22,
                         ntree= 1000, importance = TRUE)


ran.forest <- randomForest(log(SalePrice)~., data = df.train, 
                           ntree = 1000, mtry = sqrt(p), 
                           importance = TRUE, na.action = na.exclude)
ran.forest1 <- randomForest(log(SalePrice)~., data = df.train, 
                            ntree = 1000, mtry = p/2, 
                            importance = TRUE, na.action = na.exclude)
ran.forest2 <- randomForest(log(SalePrice)~., data = df.train, 
                            ntree = 1000, mtry = p/3, 
                            importance = TRUE, na.action = na.exclude)


# PLOT OOB ERROR
plot(bag.train$mse, type='l',col='purple', xlab="Number of trees",
     ylab="Out-of-bag error",ylim=c(0.021,0.027))
lines(ran.forest$mse, type='l', col='orange')
lines(ran.forest1$mse, type='l',col='red')
lines(ran.forest2$mse, type='l', col='green')
legend("topright",legend=c(expression(paste("m=",p)),
                           expression(paste("m=",sqrt(p))),
                           expression(paste("m=",p/2)),
                           expression(paste("m=",p/3))),
       col=c("purple","orange","red","green"),pch=16, cex=.7, 
       box.lty = 0)

df.boost <- df.train %>% mutate(SalePrice=log(SalePrice))

# B=1000
# Boosting using d=1

boost.tree1 <- gbm(SalePrice ~., data=df.boost,
                   distribution="gaussian",
                   n.trees=1000, interaction.depth=1,
                   shrinkage=0.01,
                   bag.fraction=1, cv.folds=10, n.cores=3)
# Boosting using d=2
boost.tree2 <- gbm(SalePrice ~., data=df.boost,
                   distribution="gaussian",
                   n.trees=1000, interaction.depth=2,
                   shrinkage=0.01,
                   bag.fraction=1, cv.folds=10, n.cores=3)
# Boosting using d=3
boost.tree3 <- gbm(SalePrice ~., data=df.boost,
                   distribution="gaussian",
                   n.trees=1000, interaction.depth=3,
                   shrinkage=0.01,
                   bag.fraction=1, cv.folds=10, n.cores=3)


#plot OOB
plot(boost.tree1$cv.error,type='l',col='green', 
     ylab="Cross validation error", xlab="Number of trees", 
     ylim=c(0.0175, 0.1))
lines(boost.tree2$cv.error, type='l', col='orange')
lines(boost.tree3$cv.error, type='l', col='red')
# lines(boost.tree12$cv.error, type='l',col='blue')
# lines(boost.tree22$cv.error, type='l',col='darkgreen')
# lines(boost.tree32$cv.error, type='l',col='violet')
legend("topright",
       legend=c("B=1000 d=1","B=1000 d=2","B=1000 d=3"),col=c("green","orange","red"),pch=16,cex=.7,
       box.lty=0)
Model <-c("model 1","model 2","model 3","model 4","model 5")
cv.error <- c(0.1620859,0.0195533,0.01924226,0.019688,0.01971477)
kable(rbind(Model,cv.error),format="latex")


df.final <- na.omit(df.keep) # Remove NAS
df.final$SalePrice <- log(df.final$SalePrice)
df.final <- df.final %>% filter(GrLivArea <=4000) 
# Ref decock pdf # outliers

# Set training and test data set
train=sample(1:nrow(df.final), nrow(df.final)*.85)
df.test = df.final[-train,]
df.train = df.final[train,]

# FACTORS NOT IN TEST SETS
df.train = df.train[,-c(grep("Heating",colnames(df.train)), grep("Exterior1st",colnames(df.train)),grep("Electrical",
                                                                                                        colnames(df.train)),
                        grep("GarageQual",colnames(df.train)))]


#fit linear model with all variables 
model.1 <- glm(log(SalePrice)~., family = gaussian("identity"), 
               data = df.train)
model.1.1 <- glm(SalePrice~., family = gaussian("identity"), 
                 data = df.train)
model.1.2 <- glm(SalePrice~1, family = gaussian("identity"),
                 data = df.train)

summary(model.1)


##rebuild, only with reduced variables
model.2 <- glm(log(SalePrice) ~ BsmtQual + CentralAir + KitchenQual + 
                 YearRemodAdd + TotalBsmtSF + GrLivArea + GarageArea + 
                 MSZoning + OverallQual + GarageFinish, 
               family = gaussian("identity"), data = df.train)
summary(model.2)

#stepAIC
y <- df.train$SalePrice

stepAIC(model.1.1, trace =T, scope = list(upper = y~., lower = ~1), 
        direction = "backward") #backward AIC
stepAIC(model.1.2, trace =T, scope = list(upper = y~., lower = ~1), 
        direction = "forward") #forward AIC

#all subset regression
#consider al subsets with method metric being adjusted r-sq
#leaps(x = df.train[,-23], y = df.train$SalePrice, int = TRUE, method = c("adjr2"), names = names(df.train[,23]))

#cross validation
library(doParallel)
library(foreach)
detectCores()
cl <- makeCluster(max(1,detectCores()-1)) 
## Initiate cluster and never use all of them!
registerDoParallel(cl)


cv.lm <- function(formula, data, K){
  
  # create CV sets
  m <- model.frame(data)
  set.seed(23)
  rand <- sample(K, length(m[[1L]]), replace = TRUE)
  table(rand)
  
  rss <- numeric(0)
  predict <- numeric(0)
  X <- numeric(0)
  Observed <- numeric(0)
  
  uniquefolds = unique(rand)
  
  # Iterate through sets
  foreach(i=1:K)%dopar%{
    i = uniquefolds[i]
    trainingFold = m[rand != i, , drop = FALSE]
    testFold = m[rand ==i, , drop = FALSE]
    
    mod <- glm(formula,data=trainingFold,family = gaussian("identity"))
    predictTestFold <- predict(mod, testFold)
    
    X <- c(X, as.integer(row.names(testFold))) #Obs number
    Observed <- c(Observed, testFold$SalePrice) # Actual Values
    predict<- c(predict,predictTestFold) # Predicted
    rss<- c(rss,((predictTestFold-testFold$SalePrice)^2)) #RSS
  }
  
  mse <- mean(rss, na.rm=T) #MSE
  output= list(Formula=formula,
               X=X, Observed=Observed, Predictions=predict,
               ResidualSumSquares =rss, MeanSquareError=mse)
  return (output)
}

#fullmodel<-cv.lm(SalePrice~., df.train,10)
#comp<-as.data.frame(cbind(fullmodel$X, fullmodel$Predictions, fullmodel$Observed))

#fullmodel$MeanSquareError # gives MSE

# Want to compare different models

formula <- c("SalePrice~.", "SalePrice~1", "SalePrice ~ BsmtQual 
             + CentralAir + KitchenQual + YearRemodAdd + TotalBsmtSF + GrLivArea 
             + GarageArea +
             MSZoning + OverallQual + GarageFinish","SalePrice ~ BsmtQual 
             + CentralAir +
             KitchenQual + FireplaceQu + GarageFinish + MSZoning
             + OverallQual + 
             YearBuilt + YearRemodAdd + TotalBsmtSF + GrLivArea + FullBath + 
             TotRmsAbvGrd + GarageArea", "SalePrice ~ BsmtQual + 
             CentralAir + KitchenQual  + GarageFinish + MSZoning + OverallQual + 
             YearRemodAdd + TotalBsmtSF + GrLivArea + FullBath + GarageArea")

# Apply CV to both models
output<-lapply(formula, cv.lm, K=10,data=df.train)

# Compare the models
output[[1]]$Formula
output[[1]]$MeanSquareError

output[[2]]$Formula
output[[2]]$MeanSquareError

output[[3]]$Formula
output[[3]]$MeanSquareError

output[[4]]$Formula
output[[4]]$MeanSquareError

output[[5]]$Formula
output[[5]]$MeanSquareError

#choose fourth model
#predict
model.3 <- glm(SalePrice ~ BsmtQual + CentralAir + KitchenQual + 
                 FireplaceQu + GarageFinish + MSZoning + OverallQual 
               + YearBuilt + YearRemodAdd 
               + TotalBsmtSF + GrLivArea + FullBath 
               + TotRmsAbvGrd 
               + GarageArea, family = gaussian("identity"), 
               data = df.train)

predictions <- predict(model.3, newdata = df.test) #predictions
obs <- df.test$SalePrice
error <- (obs - predictions)**2
mean(error) #mean square error for our given model.

#plots that depict our analysis
plot(model.3)

# COMPARE BAGGING AND BOOSTING
plot(boost.tree3$cv.error,type='l',col='green', 
     ylab="Error", xlab="Number of trees", ylim=c(0.02,.05))
lines(ran.forest2$mse, type='l', col='purple')
legend("topright",legend=c("Boosting CV Error","Random Forest OOB Error"), 
       col=c("green","purple"),pch=16, box.lty=0, cex=.7)

## RUN PREDICTIONS TO GET MSE


# Regression Tree Preds

predict.reg <- predict(tree2, newdata = df.test[,-23]) 
#carry out predictions using our refined model
log.tests <- log(df.test$SalePrice)
out <- data.frame(cbind(actuals = log.tests,
                        predicted = predict.reg)) 
#will calculate your prediction accuracy,
#sees how closely correlated actuals are with preds

mse.reg <- mean((log.tests-predict.reg)^2)

# boosting

predict.boost <- predict(boost.tree3, newdata=df.test[,-23])
mse.boost <- mean((log.tests-predict.boost)^2)

#random forest

predict.rf<- predict(ran.forest2, newdata=df.test[,-23])
mse.rf <- mean((log.tests-predict.rf)^2)


compare.mse <- cbind(RegressionTree = round(mse.reg,4), 
                     Boosting=round(mse.boost,4),
                     RandomForest = round(mse.rf,4))

kable(compare.mse, caption="Compare mean square error values on test set"
      , format="latex")

boost.imp <- as.data.frame(summary(boost.tree3,plotit=F))

ggplot(boost.imp[1:5,], aes(x = reorder(var,rel.inf),
                            y = rel.inf, fill = rel.inf)) + 
  labs(y = 'Variable Importance - Boosting', x = "", 
       fill="Relative influence") + coord_flip() +
  theme(legend.position = "none") +geom_bar(stat = "identity") 
+theme_bw()


#QUESTION2
#libraries
library(caret)
library(randomForest)
library(h2o)
library(knitr)

# Load data set
blocks <- read.csv("blocks.csv", header =T)
blocks <- blocks[,-1] # Remove X colums
blocks$class <- as.factor(blocks$class)
# Change response variable to factor - classification


# Set seed to project number
set.seed(23)


# To preserve overall class distribution of data set
# Take .8 proportion out of each y factor levels
# for accute representation of the data in training set

trainIndex <- createDataPartition(blocks$class, p = .8, 
                                  list = FALSE, 
                                  times = 1)
#head(trainIndex)

blocks_train <- blocks[trainIndex,] # Training set
blocks_test <- blocks[-trainIndex,] # Test set

# h2o

localH2O = h2o.init(ip = "localhost", port = 54321, 
                    startH2O = TRUE,min_mem_size = "2g")
h2o.clusterInfo()

# create h2o objetcs

blocksTrain_h2o <- as.h2o(blocks_train)
blocksTest_h2o <- as.h2o(blocks_test)


# Analyse data set
freq <- table(blocks$class)
freq = ((freq)/nrow(blocks))*100
freq = as.data.frame(freq) # get proportion of observations for each class
# compare this proportion and confusion matrix at the end of 
# to see if model accurately predicts rare occuring events

# Imbalanced data
ggplot(data = freq, aes(x = Var1, y = Freq)) 
+ geom_bar(stat = "identity", fill ="orange") + 
  labs(y = "% of class", x = "class")+theme_bw()


# RANDOM FOREST
p <-10 # number of predictor variables


# set hyperparameters
mtries <- seq(1,10, by=2)
ntrees = c(500,1000,1500,2000)
max_depth = c(1,4,8)


hyper_params <- list(mtries = mtries, ntrees = ntrees, 
                     max_depth = max_depth)

# use OOB MSE as random forest no need for CV
rfGrid<-h2o.grid(algorithm = "randomForest",
                 model_id= "rf_grid",
                 hyper_params = hyper_params,
                 x = 1:10, y = 11,
                 training_frame = blocksTrain_h2o,
                 #validation_frame = datTest_h2o,
                 nfolds = 0,
                 seed = 23)



summary.rf <- rfGrid@summary_table # get summary of models in DF

# Reformat data frame
summary.rf$logloss <- as.double(summary.rf$logloss)
summary.rf$mtries <- as.integer(summary.rf$mtries)

col1 <- c("mtries","ntrees","max_depth")
col2 <- c("Number of randomly selected predictor variables",
          " Number of trees grown","Depth of tree from root 
          to terminal nodes")
col3 <- c("1, 3, 5, 7, 9", "500, 1000, 1500, 2000","1, 4, 8")
tab <- cbind(col1, col2, col3)
colnames(tab) <- c("Hyperparameter","Description","Values")
kable(tab, caption="Summary of hyperparameters in random forest", 
      format="latex")

gridlabs <- c('1'='Max depth=1', '4'='Max depth=4','8'='Max depth=8')
ggplot(summary.rf, aes(x = mtries, y = round(logloss,3),
                       colour=ntrees,group = ntrees)) + labs(y="OOB Log Loss")+
  geom_point() + geom_line()+ geom_hline(yintercept=min(summary.rf$logloss), col="red",lty=2)+
  facet_grid( ~ max_depth,labeller=labeller(max_depth=gridlabs))+
  theme_bw()


# compare the performance of first 5 models 

models.rf=list()
for(i in 1:5){
  models.rf[i] = h2o.getModel(rfGrid@model_ids[[i]])
}

# x axis label with parameters
xlabel.rf = NULL
for(i in 1:5){
  xlabel.rf[i] = paste(models.rf[[i]]@allparameters$ntrees, 
                       models.rf[[i]]@allparameters$mtries, 
                       models.rf[[i]]@allparameters$max_depth)
}

#  logloss 
loglossOOB.rf = NULL
for(i in 1:5){
  loglossOOB.rf[i] = h2o.logloss(models.rf[[i]])
}

# test set  logloss 
# TEST SET  logloss 
loglossTest.rf = NULL
for (i in 1:5) {
  loglossTest.rf[i] <- h2o.logloss
  (h2o.performance(h2o.getModel(rfGrid@model_ids[[i]]), 
                   
                   newdata = blocksTest_h2o))
}

## plot logloss of first 5 models

plot(loglossTest.rf[1:5], xaxt = "n",xlab=
       "Grid Parameters (ntrees, mtries, max_depth)", type = "b",
     col ="purple",ylim=c(0.08,0.0885),ylab="Log Loss")
axis(1, at=1:5, labels=xlabel.rf[1:5])
lines(loglossOOB.rf[1:5], type = "o", col ="orange")
abline(h=min(loglossTest.rf[1:5]), lty=2, col='darkgrey')
abline(h=min(loglossOOB.rf[1:5]), lty=2, col='darkgrey')
legend("topleft",c("OOB","Test"),pch = 21, pt.bg = "white", 
       lty = 1, col = c("orange",'purple'), box.lty=0, cex=.7)

max_depth.gbm=c(4,8)
n.trees=c(500,1000,1500)
learning_rate=c(0.001,0.01)

hyper_params_gbm <- list(ntrees = n.trees,
                         max_depth = max_depth.gbm, learn_rate = learning_rate)

#10 fold CV <- CV error
gbm_grid <- h2o.grid(algorithm = "gbm",
                     hyper_params = hyper_params_gbm,
                     x = 1:10, y = 11,
                     training_frame = blocksTrain_h2o,
                     nfolds = 3,
                     seed = 23)

summary_gbm <- gbm_grid@summary_table
summary_gbm$logloss <- as.double(summary_gbm$logloss)
summary_gbm$max_depth <- as.integer(summary_gbm$max_depth)
col11 <- c("learning_rate","ntrees","max_depth")
col22 <- c(" Rate at which the learning occurs",
           " Number of trees grown",
           "Depth of tree from root to terminal nodes")
col33 <- c(".001, 0.01", "500, 1000, 1500", "4, 8")
tab1<-cbind(col11,col22,col33)
colnames(tab1) <-c("Hyperparameter",
                   "Description","Values")
kable(tab1, caption="Summary of hyperparameters used in boosting",
      format="latex")
# Use diagram to explain how the hyperparams affect performance
# want to minimize loglosss
gridlabs_gbm <- c('0.001'='Learning rate=0.001',
                  '0.01'='Learning rate=0.01')
ggplot(summary_gbm, aes(x = max_depth, 
                        y = round(logloss,3),
                        colour=ntrees,group = ntrees)) 
+ labs(y='CV Log Loss') +
  geom_point() + geom_line()+ geom_hline(yintercept=min(summary_gbm$logloss), col="red",lty=2)+
  facet_grid( ~ learn_rate,labeller=labeller(learn_rate=gridlabs_gbm))+
  theme_bw()
# compare the performance of first 5 models 

models.gbm=list()
for(i in 1:5){
  models.gbm[i] = h2o.getModel(gbm_grid@model_ids[[i]])
}

# x axis label with parameters
xlabel.gbm = NULL
for(i in 1:5){
  xlabel.gbm[i] = paste(models.gbm[[i]]@allparameters$ntrees, 
                        models.gbm[[i]]@allparameters$max_depth, 
                        models.gbm[[i]]@allparameters$learn_rate)
}

# Log loss
loglosscv.gbm = NULL
for(i in 1:5){
  loglosscv.gbm[i] = h2o.logloss(models.gbm[[i]],xval=TRUE)
}

# test set deviance
# TEST SET RMSE
loglossTest.gbm = NULL
for (i in 1:5) {
  loglossTest.gbm[i] <- h2o.logloss
  (h2o.performance(h2o.getModel(gbm_grid@model_ids[[i]]), 
                   newdata = blocksTest_h2o))
}

## plot logloss of first 5 models

plot(loglossTest.gbm[1:5], xaxt = "n",xlab=
       "Grid Parameters (ntrees, max_depth, learn_rate)", type = "b",
     col ="purple", ylab="Cross Validation log loss", ylim=c(0.05,0.15))
lines(loglosscv.gbm[1:5], type = "o", col ="orange")
axis(1, at=1:5, labels=xlabel.gbm[1:5])
abline(h=min(loglossTest.gbm[1:5]), lty=2, col='darkgrey')
abline(h=min(loglosscv.gbm[1:5]), lty=2, col='darkgrey')
legend("topleft",c("CV","Test"),pch = 21, pt.bg = "white", 
       lty = 1, col = c("orange",'purple'), box.lty=0, cex=.7)

##get confusion matrix random forest
best_rf <- h2o.getModel(rfGrid@model_ids[[1]])
best_gbm <- h2o.getModel(gbm_grid@model_ids[[1]])

# get log loss
logloss_rf <- h2o.logloss(best_rf)
logloss_gbm <- h2o.logloss(best_gbm, xval=T)

# get predictions
predictionsTest_rf <- h2o.predict(best_rf, blocksTest_h2o)
yhatTest_rf = as.factor(as.matrix(predictionsTest_rf$predict))
cm_rf<-confusionMatrix(yhatTest_rf, blocks_test$class) # Confusion matrix

##get confusion matrix boosting
predictionsTest_gbm <- h2o.predict(best_gbm, blocksTest_h2o)
yhatTest_gbm = as.factor(as.matrix(predictionsTest_gbm$predict))
cm_gbm<-confusionMatrix(yhatTest_gbm, blocks_test$class) 
# Confusion matrix  #CONFUSION MATRIX (1st)
cm_rf$table
# for rf#CONFUSION MATRIX (2)
cm_gbm$table
# for gbm
stats <- cbind(c(logloss_rf, logloss_gbm),rbind(cm_rf$overall[1], 
                                                cm_gbm$overall[1]))
stats <- cbind(stats,1-stats[,2])
rownames(stats)= c("Random Forest", "Boosting")
colnames(stats)=c("Log loss", "Accuracy", "Misclassification")
kable(stats,caption="Comparing performance metrics", format="latex")


by.class <- cbind(round(cm_rf$byClass[,1],3), round(cm_gbm$byClass[,1],3),round(cm_rf$byClass[,2],3),
                  round(cm_gbm$byClass[,2],3))
colnames(by.class) <-cbind("RF Sensitivity","Boosting Sensitivity", 
                           "RF Specificity", "Boosting Specificity")


kable(by.class, caption ="Comparing sensitivity and specificity", format="latex")h2o.varimp_plot(best_rf) # Variable importance plot



## Plots:


load("FINALDATA1.RData")
library(dplyr)
library(tree)
df.final <- na.omit(df.keep) # Remove NAS

df.final <- df.final %>% filter(GrLivArea <=4000) # Ref decock pdf # outliers

# Set training and test data set
train=sample(1:nrow(df.final), nrow(df.final)*.85)
df.test = df.final[-train,]
df.train = df.final[train,]

tree1 <- tree(log(SalePrice)~., data = df.train)
# summary(tree1)
plot(tree1)
text(tree1,pretty=0)






#boxplots
load("factors.RData")

library(ggplot2)
library(reshape2)

melt.data <- melt(df.factors)


# 1-10
df.melts1= melt(df.factors[,1:10], id.vars = c("SalePrice"),
                measure.vars = colnames(df.factors)[2:10])
p <- ggplot(data = df.melts1, aes(x=value, y=SalePrice)) + 
  geom_boxplot(aes(fill="orange"))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
p + facet_wrap( ~ variable, scales="free")
#11-21
df.melts2= melt(df.factors[,cbind(1, 11:21)], id.vars = c("SalePrice"),
                measure.vars = colnames(df.factors)[c(11:21)])


p2 <- ggplot(data = df.melts2, aes(x=value, y=SalePrice)) + 
  geom_boxplot(aes(fill="orange"))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
p2 + facet_wrap( ~ variable, scales="free")
