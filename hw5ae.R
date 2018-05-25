library(tidyverse)
airlines<- read.csv("/home/subhar/AirlinesCluster.csv")
glimpse(airlines)
summary(airlines)

#install.packages("caret")
library(caret)
#install.packages("reshape2")
library(ggplot2)
library(reshape2)

preproc = preProcess(airlines)

airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)
#for clustering, gather distances 
distances = dist(airlinesNorm, method="euclidean")

hierClust = hclust(distances, method="ward.D")
plot(hierClust, labels=FALSE)
glimpse(airlinesNorm)

ClusterGroups = cutree(hierClust, k = 5)
table(ClusterGroups)

k = 5
set.seed(2018)
KMC = kmeans(airlinesNorm, centers = k, iter.max = 1000)
str(KMC)
table(KMC$cluster)#for i in colnames(airlines){ print("Table for", %i), }
#################################################
stocks<- read.csv("StocksCluster.csv")
table(stocks$PositiveDec)
6234/(5256+6234)
#install.packages("Hmisc")
library(Hmisc)
res<- rcorr(as.matrix(stocks))
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )}

summary(stocks)

set.seed(2018)

TrainRows = createDataPartition(stocks$PositiveDec, p = 0.7, list = FALSE)

spl = rep(FALSE,nrow(stocks))

spl[TrainRows] = TRUE

stocksTrain = subset(stocks, spl==TRUE)

stocksTest = subset(stocks, spl==FALSE)


stocksLog = glm(PositiveDec~., data=stocksTrain, family="binomial")
PredictTrain = predict(stocksLog, type="response")
table(stocksTrain$PositiveDec, PredictTrain >.5)
predTestLog = predict(stocksLog, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, predTestLog >.5)
969+3699
4668/8106

(428+1517)/3474

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL


library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)

summary(normTrain)
summary(normTest)
stocksLog

set.seed(2018)
km = kmeans(normTrain, centers = 3)
table(km$cluster)
install.packages("flexclust")
library(flexclust)

km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTest)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest ==2)
stocksTest3 = subset(stocksTest, clusterTest ==3)
stocksTrain1= subset(stocksTrain, km$cluster == 1)
stocksTrain2= subset(stocksTrain, km$cluster == 2)
stocksTrain3= subset(stocksTrain, km$cluster == 3)
summary(stocksTrain1$PositiveDec)
summary(stocksTrain2$PositiveDec)
summary(stocksTrain3$PositiveDec)

stocksMod1 = glm(PositiveDec~., data=stocksTrain1, family="binomial")
PredictTrain1 = predict(stocksMod1, type="response")
stocksMod2 = glm(PositiveDec~., data=stocksTrain2, family="binomial")
PredictTrain2 = predict(stocksMod2, type="response")
stocksMod3 = glm(PositiveDec~., data=stocksTrain3, family="binomial")
PredictTrain3 = predict(stocksMod3, type="response")

summary(stocksMod1)
summary(stocksMod2)
summary(stocksMod3)


predTest1 = predict(stocksMod1, newdata=stocksTest1, type="response")
predTest2 = predict(stocksMod2, newdata=stocksTest2, type="response")
predTest3 = predict(stocksMod3, newdata=stocksTest3, type="response")

table(stocksTest1$PositiveDec, predTest1 >.5)

table(stocksTest2$PositiveDec, predTest2 >.5)

table(stocksTest3$PositiveDec, predTest3 >.5)

> AllPredictions = c(predTest1, predTest2, predTest3)
> 
  > AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
> table(AllOutcomes, AllPredictions >.5)
