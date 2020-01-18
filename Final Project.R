library(caret)
library(ggplot2)


#loading the training and testing data
#we replace "NA" with ""
train_data = read.csv("C:/Users/vignesh-pc/Downloads/pml-training.csv", na.strings = "")
test_data = read.csv("C:/Users/vignesh-pc/Downloads/pml-testing.csv",na.strings = "")

#since we have a lot of columns with null values and they dont add any value to prediction, we remove them
train_data = train_data[,colSums(is.na(train_data)) == 0]
test_data = test_data[,colSums(is.na(test_data)) == 0]

#we dont need serial no and name
train_data  = train_data[,-c(1:7)]
test_data = test_data[,-c(1:7)]

#we create a validation set so that we test our different models and finally get the best model to predict again test data
train_in = createDataPartition(train_data$classe, p = 0.7, list = FALSE)
train_data_ = train_data[train_in,]
valid_data = train_data[-train_in,]
train_in = createDataPartition(train_data_$classe, p = 0.8, list = FALSE)
train_data = train_data_[train_in,]
test_data = train_data[-train_in,]

featurePlot(train_data$roll_belt, train_data$classe)

#since we a large dataset to work with, we have made a training data with 11k observations, validation dataset with 6k observations and testing dataset with 2k observations
#apart from all these we have given testing data, which is untouched.
#we have made all these to use ensemble of different methods

#GLM
model_rf = train(classe~., method="rf",data=train_data)

predict_rf = predict(model_rf,valid_data)

confusionMatrix(predict_rf, valid_data$classe)
# 
# 
# mod_rf = train(classe~.,method="rf",data=train_data)
# mod_gbm = train(classe~.,method="gbm",data=train_data)
# mod_lda = train(classe~.,method="lda",data=train_data)
# 
# pred_rf = predict(mod_rf,newdata=valid_data)
# pred_gbm = predict(mod_gbm,newdata=valid_data)
# pred_lda = predict(mod_lda,newdata=valid_data)
# 
# C_mod = data.frame(pred_rf,pred_gbm,pred_lda,classe=valid_data$classe)
# c_mod_model = train(classe~.,method="rf",data=C_mod)
# c_mod_pred = predict(c_mod_model,C_mod)
# confusionMatrix( c_mod_pred,C_mod$classe)
# 
dim(train_data)
train_data[,1:119]
prepc = preProcess(train_data[,1:119], method = "pca", pcaComp = 20)
trainPc = predict(prepc, train_data[,])
model_fit = train(classe ~ ., method = "rf", data=trainPc)
predict_rf = predict(model_rf,valid_data)
confusionMatrix(predict_rf, valid_data$classe)