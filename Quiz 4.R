library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test)
set.seed(33833)
colnames(vowel.train)
vowel.test$y=as.factor(vowel.test$y)
vowel.train$y=as.factor(vowel.train$y)
mod_fit = train(y~.,method = "rf",data=vowel.train)
pred = predict(mod_fit,newdata = vowel.test)
pred
vowel.test
confusionMatrix(pred,vowel.test$y)


#Alzehmiers disease
library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

colnames(training)

mod_rf = train(diagnosis~.,method="rf",data=training)
mod_gbm = train(diagnosis~.,method="gbm",data=training)
mod_lda = train(diagnosis~.,method="lda",data=training)

pred_rf = predict(mod_rf,newdata=testing)
pred_gbm = predict(mod_gbm,newdata=testing)
pred_lda = predict(mod_lda,newdata=testing)

C_mod = data.frame(pred_rf,pred_gbm,pred_lda,diagnosis=testing$diagnosis)
c_mod_model = train(diagnosis~.,method="rf",data=C_mod)
c_mod_pred = predict(c_mod_model,C_mod)
confusionMatrix( c_mod_pred,C_mod$diagnosis)



#concerete data
set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)

mod_cs = train(CompressiveStrength ~.,method="lasso",data=training)

#no of visistors
library(lubridate) # For year() function below

dat = read.csv("C:/Users/vignesh-pc/Downloads/gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

library(forecast)

mod_ts = bats(tstrain)
fcast <- forecast(mod_ts, level = 95, h = dim(testing)[1])
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
  dim(testing)[1]