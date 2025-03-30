setwd("C:/Users/mural/OneDrive/Desktop/SEM 5/INT234")
data <- read.csv("bank-full.csv",sep = ";")
str(data)
summary(data)
View(data)
data$y <- factor(data$y,labels=c("no","yes"))

categorical <- c('job', 'marital', 'education', 'default', 'housing', 'loan', 
                          'contact', 'month', 'poutcome')
numerical<- c('age', 'balance', 'day', 'duration', 'campaign', 'pdays', 'previous')

data[categorical] <- lapply(data[categorical], as.factor)
print(table(data$y))

set.seed(123)
library(caTools)
split <- sample.split(data$y,SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

library(rpart)

tree <- rpart(y ~.,data=train,method = "class")
print(tree)

prediction <- predict(tree,test,type = "class")
library(caret)
cm <- confusionMatrix(prediction,test$y)
cm
library(rpart.plot)
rpart.plot(tree, type = 4, extra = 104, fallen.leaves = TRUE, cex = 0.7)
