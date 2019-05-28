setwd("D:/BigData R분석 고급/student")
d1=read.table("insurance.csv", sep=",", header=TRUE)
#d2=read.table("student-por.csv",sep=";",header=TRUE)

#d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
#print(nrow(d3)) # 382 students

head(d1)

summary(d1)

library(dplyr)
library(ggplot2)
library(ROCR)
library(psych)
library(glmnet)
library(car)

boxplot(d1)
colnames(d1)

data <- d1
data <- select(d1, -'region')
#data <- d1[d1$charges < 35000, ]


set.seed(9367) # 0.31, 0.55
index <- sample(nrow(data), nrow(data)*0.8)

#data$age <- as.factor(data$age)


train <- data[index,]
test <- data[-index, ]


# 선형회귀
fit.full <- lm(charges ~ ., train)
fit.min <- lm(charges ~ 1, train)
step_model <- step(fit.min, direction="both", scope=list(upper=fit.full,lower=fit.min))
summary(step_model)


#step_model$anova

# 회귀계수의 95% 신뢰구간 확인
confint(step_model)

# 표준화 잔차의 분포를 시각적으로 확인하기
plot(rstandard(step_model))

# 정규성 검증
shapiro.test(rstandard(step_model))

par(mfrow=c(2,2))
plot(step_model)
par(mfrow=c(1,1))

# 선형성 확인
crPlots(step_model)

#다중 공선성
vif(step_model)

# 선형예측
model.pred<- predict(step_model, newdata=test)
summary(model.pred)

# 예측 
#pr <- prediction(model.pred, test$G1) #<-- 분류에서만 사용 가능하단다
#prf <- performance(pr, measure = 'tpr', x.measure = 'fpr' )
#plot(prf)

# 정확도 계산
actuals_preds <-
  data.frame(cbind(actuals=test$charges, prediction=model.pred))

correlation_accuracy <- cor(actuals_preds) 
correlation_accuracy
summary(correlation_accuracy)
d <- cbind(id=1:nrow(actuals_preds), actuals_preds)

ggplot(d, aes(id)) + 
  geom_line(aes(y = prediction, colour = 'prediction')) + 
  geom_line(aes(y = actuals, colour = 'actuals'))



