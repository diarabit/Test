library(ggplot2)
library(gridExtra)
setwd("D:/BigData R분석 고급/student")

d1=read.table("heart.csv", sep=",", header=TRUE)

#boxplot(d1)
str(d1)
summary(d1)

data <- d1
data$target <- as.factor(data$target)
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <-  as.factor(data$restecg)
data$exang <-  as.factor(data$exang)
data$slope <-  as.factor(data$exang)
data$ca <- as.factor(data$ca)
data$thal <-  as.factor(data$thal)

str(data)
summary(data)
plot(data)
boxplot(data[, ])

#grid.arrange(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, nrow=2, ncol=5)


set.seed(9367)
index <- sample(nrow(data), nrow(data)*0.8)

train <- data[index,]
test <- data[-index, ]


# 결정트리(ctree)
library(party)
model_ctree <- ctree(target ~ ., train)
plot(model_ctree)
model_ctree

result_ctree <- predict(model_ctree, test, type="response")
head(result_ctree, 10)
plot(result_ctree)


# 랜덤 포레스트
if (!require(randomForest)) install.packages('randomForest')
library(randomForest)
model_random <- randomForest(x = train[-14],　y = train$target,　ntree = 30)
model_random
summary(model_random)
result_random <- predict(model_random, test[-14], type = "class")

head(result_random , 5)
plot(result_random)


# KNN
library(class)
library(dplyr)
library(caret)

data$target <- as.integer(data$target)
data$sex <- as.integer(data$sex)
data$cp <- as.integer(data$cp)
data$fbs <- as.integer(data$fbs)
data$restecg <-  as.integer(data$restecg)
data$exang <-  as.integer(data$exang)
data$slope <-  as.integer(data$exang)
data$ca <- as.integer(data$ca)
data$thal <-  as.integer(data$thal)

train_knn <- data[index,]
test_knn <- data[-index, ]

train_cl <- train_knn[ , c('target')]
test_cl <- test_knn[ , c('target')]

str(train)
str(test)
model_knn <- knn(train = train_knn[,-14], test = test_knn[, -14], cl = train_cl, k = 10) 

tab <- table(model_knn, test_cl)
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


# SVM 머신러닝 알고리즘 -----------------------------------------------------------
if(!require(e1071)) install.packages("e1071")
library(e1071)

model_svm <- svm(target ~ ., data=train)

result_svm <- predict(model_svm, test[, -14])

# 로지스틱
model.min <- glm(target ~ 1, family=binomial, data = train) # binomial distribution
model.full <- glm(target ~ ., family=binomial, data = train)
model_logi <- step(model.min, direction="both", scope=list(upper=model.full,lower=model.min))
summary(model_logi)
model_logi

result_logi <- predict(model_logi, newdata = test, type = "response")

summary(result_logi) 
head(result_logi, 10)


library(ROCR)
library(Epi)
library(dplyr)

pr <- prediction(result_logi, test$target)
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)

cutoffs <- data.frame(cut = prf@alpha.values[[1]], 
                      fpr = prf@x.values[[1]], 
                      tpr = prf@y.values[[1]])

cutoffs$sum <- cutoffs$tpr + (1 - cutoffs$fpr)
cutoffs <- arrange(cutoffs, desc(sum))
head(cutoffs, 10)

cutoff.1 <- ifelse(result_logi >= 0.6, 1, 0)
cutoff.2 <- ifelse(result_logi >= 0.46, 1, 0)
summary(cutoff.1)
summary(cutoff.2)

auc <- performance(pr, measure = "auc")
auc@y.values[[1]]
ROC(result_logi, test$target)

cor(data[, c('cp', 'oldpeak', 'ca', 'thal', 'sex', 'thalach', 'trestbps', 'exang', 'restecg')])




str(test)
confusionMatrix(table(test$target, result_ctree))
confusionMatrix(table(test$target, result_random))
confusionMatrix(table(test_knn$target, model_knn))
confusionMatrix(table(test$target, result_svm))

confusionMatrix(as.factor(cutoff.1), test$target)
confusionMatrix(as.factor(cutoff.2), test$target)

