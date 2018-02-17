#Importing required packages
library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)
library(MASS)
library(car)
library(mice)
library(Hmisc)

mydata <- read.csv('train.csv',header=T,na.strings=c("NA"))
md.pattern(mydata)
tempData <- mice(mydata,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
tempData$imp$Age   
titanic <- complete(tempData,1)

#titanic <- read.csv("Data1.csv")
#titanic2 <- read.csv("testnew.csv")

set.seed(144)

split <- sample.split(titanic$Survived, SplitRatio = 0.7)

train <- filter(titanic, split == TRUE)
test <- filter(titanic, split == FALSE)

str(train) 

LogModel <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                  Fare + Embarked, data=train, family="binomial")

summary(LogModel)


LogModel2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                  Fare, data=train, family="binomial")

summary(LogModel2)

LogModel3 <- glm(Survived ~ Pclass + Sex + Age + SibSp + 
                   Fare, data=train, family="binomial")

summary(LogModel3)

LogModel4 <- glm(Survived ~ Pclass + Sex + Age + SibSp, data=train, family="binomial")

summary(LogModel4)


predTestLog2 <- predict(LogModel4, newdata=titanic2, type="response") 
# type on default gives log-odds, "response" gives predicted probabilities

write.csv(predTestLog2, file = "Titanic_R.csv")
#predTestLog2 <- round(predTestLog2)

# Confusion matricies based on decision tree threshold 
table(test$Survived, predTestLog > 0.5)


test$estimatedcost = as.numeric((test$estimatedcost))

mean(test$estimatedcost, na.rm = TRUE)
summary(test$estimatedcost)

# ROC curve for logistic regression
rocr.log.pred <- prediction(predTestLog, test$Survived)
logPerformance <- performance(rocr.log.pred, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.log.pred, "auc")@y.values)
