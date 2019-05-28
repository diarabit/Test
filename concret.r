library(xlsx)
library(dplyr)
library(ggplot2)
library(ROCR)

setwd("D:/BigData R분석 고급/student")
data <- read.xlsx("Concrete_Data.xlsx", 1)
head(data)

#plot(data)
boxplot(data)

set.seed(4)
index <- sample(nrow(data), nrow(data)*0.8)
summary(data)

train <- data[index,]
test <- data[-index, ]

fit.full <- lm(strength ~ ., train)
fit.min <- lm(strength ~ 1, train)
step_model <- step(fit.min, direction="both", scope=list(upper=fit.full,lower=fit.min))

summary(step_model)

# 선형예측
model.pred<- predict(step_model, newdata=test)
summary(model.pred)

actuals_preds <-
  data.frame(cbind(actuals=test$strength, prediction=model.pred))

correlation_accuracy <- cor(actuals_preds) 
correlation_accuracy
summary(correlation_accuracy)

d <- cbind(id=1:nrow(actuals_preds), actuals_preds)

ggplot(d, aes(id)) + 
  geom_line(aes(y = prediction, colour = 'prediction')) + 
  geom_line(aes(y = actuals, colour = 'actuals'))

