library(tidyverse)
library(caret)
library(neuralnet)
thyroid <- read_csv("C:/Users/subha/Documents/Spring Semester/Analytics Edge/thyroid-disease-ann-thyroid.csv")
set.seed(2018)
spl = rep(FALSE,nrow(thyroid))
TrainRows = createDataPartition(y=thyroid$Y, p=0.7, list = FALSE)
spl[TrainRows] = TRUE
train = subset(thyroid, spl==TRUE)
test = subset(thyroid, spl==FALSE)

summary(test$X21)


set.seed(2018)
nn = neuralnet(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21 , data=train, 
               hidden=c(10,5), linear.output=T)
plot(nn)
# Black lines are weights, blue lines are biases
# It's hard to read, but we can extract any coefficient we want from the model output
str(nn)
str(nn$weights)

head(test)

trainpred_nn <- neuralnet::compute(nn,train[,1:21])
str(trainpred_nn)
trainpred_nn = trainpred_nn$net.result
summary(trainpred_nn)
SSE = sum((trainpred_nn - train$Y)^2)
SST = sum((mean(train$Y) - train$Y)^2)
R2_trainnn = 1 - SSE/SST
R2_trainnn


prediction_nn <- neuralnet::compute(nn,test[,1:21])
str(prediction_nn)
prediction_nn = prediction_nn$net.result
summary(prediction_nn)

# Let's compute our test set R2
SSE = sum((prediction_nn - test$Y)^2)
SST = sum((mean(train$Y) - test$Y)^2)
R2_nn = 1 - SSE/SST
R2_nn


###########################333

train$L1 = train$Y==1

train$L2 = train$Y==2

train$L3 = train$Y==3

test$L1 = test$Y==1

test$L2 = test$Y==2

test$L3 = test$Y==3

###################################3
set.seed(2018)

nn1 = neuralnet(L1 + L2 + L3 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21, data=train, hidden=c(10,5), linear.output=FALSE)
plot(nn1)
nn1$weights[[1]][[1]][2,1]

trainpred_nn1 <- neuralnet::compute(nn1, train[,1:21])
testpred_nn1 <- neuralnet::compute(nn1,test[,1:21])
str(trainpred_nn1)
trainpred_nn1 = trainpred_nn1$net.result
testpred_nn = testpred_nn1$net.result
summary(prediction_nn)

predtrain <- max.col(trainpred_nn)
predtest <- max.col(testpred_nn)
table(predtest)
table(predtrain)


library(rpart)
library(rpart.plot)
tree<- rpart(Y~., data=train[,1:22], method = "class")
prp(tree)


predTrainCART = predict(tree)[,1:3]

t_pred = predict(tree,test)[,1:3]

table(test$Y, max.col(t_pred))
27+57+1044


1128/1131

  predTrainCART
summary(spamLog)

prp(spamCART)
table(train$Y, max.col(predTrainCART))

66+134+2428


2628/2641
rpart(Y~., data=train[,1:22], method = "class")
