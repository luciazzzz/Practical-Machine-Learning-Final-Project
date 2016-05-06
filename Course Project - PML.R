setwd("~/Desktop")
traindata_raw<-read.csv('pml-training.csv',header=TRUE,sep=",")
questiondata_raw<-read.csv('pml-testing.csv',header=TRUE,sep=",")
dim(traindata_raw)
dim(questiondata_raw)

for(i in c(8:ncol(traindata_raw)-1)) {traindata_raw[,i] = as.numeric(as.character(traindata_raw[,i]))}
for(i in c(8:ncol(questiondata_raw)-1)) {questiondata_raw[,i] = as.numeric(as.character(questiondata_raw[,i]))}

head(traindata_raw)
head(is.na(traindata_raw))

keep_set1 <- colnames(traindata_raw[colSums(is.na(traindata_raw)) == 0])[-(1:7)]
traindata <- traindata_raw[keep_set1]
keep_set1

keep_set2 <- colnames(questiondata_raw[colSums(is.na(questiondata_raw)) == 0])[-(1:7)]
questiondata <- questiondata_raw[keep_set2]
keep_set2

dim(traindata)
dim(questiondata)

####### Machine Learning - model building
####### Recursive partitioning Model
library(Hmisc)
library(caret)
library(randomForest)
library(foreach)
library(rattle)
set.seed(62339)
options(warn=-1)

####### Splitting original traindata into training and testing datasets.
inTrain <- createDataPartition(traindata$classe, p=0.75, list=FALSE)
training_set <- traindata[inTrain,]
testing_set <- traindata[-inTrain,]

dim(training_set)
dim(testing_set)

rpmodel<- train(classe ~ ., method="rpart", data=training_set)
rpmodel$finalMode

fancyRpartPlot(rpmodel$finalModel, sub='')

rpcv <- predict(rpmodel, newdata=testing_set)
rpcvmatrix<-confusionMatrix(rpcv, testing_set$classe)
rpcvmatrix$overall

####### Random Forest Model
rfcontrol <- trainControl(method="cv", 5)
rfmodel <- train(classe ~ ., data=training_set, method="rf", trControl=rfcontrol, ntree=250)
rfmodel

predictRF <- predict(rfmodel, testing_set)
confusionMatrix(testing_set$classe, predictRF)

accuracy <- postResample(predictRF, testing_set$classe)
accuracy
oose <- 1 - as.numeric(confusionMatrix(testing_set$classe, predictRF)$overall[1])
oose

result <- predict(rfmodel, questiondata[, -length(names(questiondata))])
result

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

x<-questiondata
result <- predict(rfmodel, x[, -length(names(x))])
result