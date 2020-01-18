library(ISLR)
library(ggplot2)
library(caret)
data("Wage")
Wage = subset( Wage,select = -c(logwage))
summary(Wage)
train_index = createDataPartition(y=Wage, p = 0.7, list=FALSE)
train_data = Wage[train_index,]
test_data = Wage[-train_index,]
colnames(train_data)
qplot(age,wage,data=Wage)
