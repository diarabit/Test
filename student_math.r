setwd("D:/BigData R분석 고급/student")
d1=read.table("student-mat.csv",sep=",",header=TRUE)
#d2=read.table("student-por.csv",sep=";",header=TRUE)

#d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
#print(nrow(d3)) # 382 students

head(d1)

summary(d1)

install.packages("psych")
library(dplyr)
library(ggplot2)
library(ROCR)
library(psych)

boxplot(d1)

d1 <- d1[d1$absences < 15, ]


set.seed(1111) # 0.34, 0.30
set.seed(4856) # 0.35, 0.36
set.seed(292100) # 0.29, 0.55
set.seed(9436) # 0.32, 0.52
set.seed(3075) # 0.33, 0.45
set.seed(0075) # 0.33, 0.38
set.seed(9367) # 0.34, 0.35
index <- sample(nrow(d1), nrow(d1)*0.9)

d1$age <- as.factor(d1$age)
d1$Medu <- as.factor(d1$Medu)
d1$Fedu <- as.factor(d1$Fedu)
d1$traveltime <- as.factor(d1$traveltime)
d1$studytime <- as.factor(d1$studytime)
d1$failures <- as.factor(d1$failures)
d1$famrel <- as.factor(d1$famrel)
d1$freetime <- as.factor(d1$freetime)
d1$goout <- as.factor(d1$goout)
d1$Dalc <- as.factor(d1$Dalc)
d1$Walc <- as.factor(d1$Walc)
d1$health <- as.factor(d1$health)


train <- d1[index,]
test <- d1[-index, ]

train$G2 <- NULL
train$G3 <- NULL
test$G2 <- NULL
test$G3 <- NULL


# 선형회귀
fit.full <- lm(G1 ~ ., train)
fit.min <- lm(G1 ~ 1, train)
step_model <- step(fit.full, direction="both", scope=list(upper=fit.full,lower=fit.min))

step_model$anova

lm.model.forward <- lm(step_model)
summary(lm.model.forward)
#plot(lm.model.forward) 

# 선형예측
model.pred.forward <- predict(lm.model.forward, newdata=test)
summary(model.pred.forward)


# 정확도 계산
actuals_preds.for <-
  data.frame(cbind(actuals=test$G1, prediction=model.pred.forward))

correlation_accuracy.for <- cor(actuals_preds.for) 
correlation_accuracy.for
nrow(actuals_preds.for)
d <- cbind(id=1:40, actuals_preds.for)

ggplot(d, aes(id)) + 
  geom_line(aes(y = prediction, colour = 'prediction')) + 
  geom_line(aes(y = actuals, colour = 'actuals'))


library(FNN)
train2 <- train[, -31]
test2 <- test[, -31]

train2_target <- train[, 31]
test2_target <- test[, 31]

pr <- knn(train2, test2, cl = train2_target, k=1)
summary(pr)
