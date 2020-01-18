library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
training = subset(training, select = c("diagnosis", "IL_11","IL_13","IL_16","IL_17E","IL_1alpha","IL_3","IL_4","IL_5","IL_6"))
prepc = preProcess(training[-1,], method = "pca", pcaComp = 9)
trainPc = predict(prepc, training[-1,])
model_fit = train(diagnosis ~ ., method = "glm", data=trainPc)

testing = subset(testing, select = c("diagnosis", "IL_11","IL_13","IL_16","IL_17E","IL_1alpha","IL_3","IL_4","IL_5","IL_6"))
testpc = predict(prepc,testing[-1,])
confusionMatrix( predict(model_fit,testpc), testpc$diagnosis)
