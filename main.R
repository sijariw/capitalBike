#Data setup and Package installation
library(here);library(stargazer);library(DAAG);library(MASS);library(boot);library(caret);library(klaR);library(rpart);library(tree)
mainTable<-readr::read_csv(here("datasets", "mainTable.csv"));attach(mainTable)
set.seed(1)

#crossval datasetup
train = sample(1:nrow(mainTable), nrow(mainTable)*.5)

# mlregression
day.lm=lm(count~season+month+holiday+weekday+workingday+weathertype+acttempdenorm+tempdenorm+humiditydenorm+windspeeddenorm)
summary(day.lm)
step=stepAIC(day.lm,direction="backward")
step$anova
day.lm2=lm(count~season+month+holiday+weathertype+tempdenorm+humiditydenorm+windspeeddenorm)
summary(day.lm2)

# validation set approach
day.lm2tst=lm(count~season+month+holiday+weathertype+tempdenorm+humiditydenorm+windspeeddenorm,subset=train)
mean((count-predict(day.lm2tst,mainTable))[-train]^2)
par(mfrow=c(1,1),bg="grey")

plot(day.lm2,pch=16)
# yields=1691315 as mse

# regression tree
# set half of the observations as the training dataset (validation-set approach)
tree.mainTable=tree(count~season+month+holiday+weathertype+tempdenorm+humiditydenorm+windspeeddenorm,mainTable,subset=train)
# the context of a regression tree, the deviance is simply the sum of squared errors for the tree.
summary(tree.mainTable)

# plot the tree
plot(tree.mainTable)
text(tree.mainTable,pretty=0)

# cross validation to determine the best tree
cv.mainTable=cv.tree(tree.mainTable)
cv.mainTable
plot(cv.mainTable$size,cv.mainTable$dev,type='b')
# use unpruned tree to make prediction on the test data
# calcualte the predicted value of DV on the test data using the trained model 
yhat=predict(tree.mainTable,newdata=mainTable[-train,])
# yields: 2040195

# true value of DV on the test data
mainTable.test=mainTable[-train,"count"]
plot(yhat,mainTable.test)
abline(0,1)
# MSE (Mean of Squared Errors)
mean((yhat-mainTable.test)^2)

# if you would like to prune the tree
prune.mainTable=prune.tree(tree.mainTable,best=9)
plot(prune.mainTable)
text(prune.mainTable,pretty=0)
# use unpruned tree to make prediction on the test data
# calcualte the predicted value of DV on the test data using the trained model 
yhat2=predict(prune.mainTable,newdata=mainTable[-train,])
# true value of DV on the test data
mainTable.prunetest=mainTable[-train,"count"]
plot(yhat2,mainTable.prunetest)
abline(0,1)
# MSE (Mean of Squared Errors)
mean((yhat2-mainTable.prunetest)^2)

