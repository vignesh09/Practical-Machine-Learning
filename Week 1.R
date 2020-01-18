library(caret)
library(kernlab)
data("spam")
train_ = createDataPartition(y=spam$type,p = 0.75, list = FALSE)
train_data = spam[train_,]
test_data = spam[-train_,]
dim(train_data)
dim(spam)
model_ = train(type ~., data = train_data, method = "glm")
model_
predictions_ = predict(model_, newdata = test_data)
predictions_
confusionMatrix(predictions_,test_data$type)
