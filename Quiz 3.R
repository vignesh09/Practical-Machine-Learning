library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
colnames(segmentationOriginal)
train_in = createDataPartition(y = segmentationOriginal$Case,p=0.7, list=FALSE)
train_data = segmentationOriginal[train_in,]
test_date = segmentationOriginal[-train_in,]
set.seed(125)
mod_fit = train(Case ~.,method="rpart",data=train_data)
mod_fit$finalModel


#olive
library(pgmm)
data(olive)
olive = olive[,-1]
modfit =  train(Area ~ ., method = "rpart", data = olive)
predict(modfit, newdata=as.data.frame(t(colMeans(olive))))

#south African Heart Disease
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
colnames(SAheart)
mod_fitSA = train(chd ~ age+alcohol+obesity+tobacco+typea+ldl, method="glm",family="binomial",data=trainSA)
missClass = function(values, prediction){sum(((prediction > 0.5) * 1) != values) / length(values)}
missClass(testSA$chd,predict(mod_fitSA,newdata = trainSA))
missClass(trainSA$chd,predict(mod_fitSA,newdata = testSA))



#vowel.train
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
library(randomForest)
modvowel <- randomForest(y ~ ., data = vowel.train)
order(varImp(modvowel), decreasing = T)