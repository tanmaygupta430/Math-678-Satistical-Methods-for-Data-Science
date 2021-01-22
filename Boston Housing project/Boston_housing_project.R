#variable selection
library(MASS)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(leaps)
library(rpart)
library(mgcv)
library(glmnet)
library(boot)
library(rpart.plot)
library(e1071)
data("Boston")
dim(Boston)
head(Boston)
glimpse(Boston)
sum(is.na(Boston))
sum(duplicated(Boston))
Boston %>%
  gather(key, val, -medv) %>%
  ggplot(aes(x = val, y = medv)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE, col = "blue") +
  facet_wrap(~key, scales = "free") +
  theme_gray() +
  ggtitle("Scatter plot of dependent variables vs medv")

set.seed(123)
train_index<- sample(nrow(Boston), nrow(Boston)* 0.8)
Boston_train<- Boston[train_index, ]
Boston_test<- Boston[-train_index, ]
summary(Boston_train)
summary(Boston_test)

#linear model on train
model_reg <- lm(medv ~ ., data = Boston_train)
model_reg_sum <- summary(model_reg)
model_reg_sum
#linear model on test 
model_reg_test<- predict(model_reg, newdata = Boston_test)
model_reg_test_sum<- summary(model_reg_test)
model_reg_test_sum
head(model_reg_test)
#calculating MSPE
model_mspe<- model1.mspe <- mean((model_reg_test - Boston_test$medv) ^ 2)
model_mspe

#using best subset 
model_1 <- regsubsets(medv ~., data= Boston_train, nbest=1, nvmax= 13)
summary(model_1)
#Variable selection using stepwise 
nullmodel <- lm(medv ~ 1, data = Boston_train)
fullmodel <- lm(medv ~ ., data = Boston_train)
#forward selection
model_2<-step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "forward")
#backwar selection
model.step.b <- step(fullmodel, direction = "backward")
#stepwise selection
model_3<- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "both")
summary(model_3)

#linear model without indus and age 
best_reg <- lm(medv ~ . -indus -age, data = Boston_train)
best_reg_sum <- summary(best_reg)
best_reg_sum

summary(best_reg)$r.sq
#linear model without indus and age on test
best_reg_test <- predict(best_reg, newdata = Boston_test)
summary(best_reg_test)
head(best_reg_test)
#calculating MSPE
model2.mspe <- mean((best_reg_test - Boston_test$medv) ^ 2)
model2.mspe

plot(best_reg_test,Boston_test$medv)
#for mse
library(DMwR)
regr.eval(Boston_test$medv, best_reg_test)
#actul and predicted correlation
actuals_pred<- data.frame(cbind(actuals= Boston_test$medv, predicteds= best_reg_test))
correlation_accuracy <- cor(actuals_pred)
correlation_accuracy
head(actuals_pred)

library(DMwR)
regr.eval(Boston_test$medv, best_reg_test)

summary(best_reg_test)$r.sq

#Linear SVM 
library(e1071)
set.seed(2)
model_svm<- svm(medv~ .,data=Boston_train, kernel= 'linear')
summary(model_svm)

set.seed(2)
tune_svm_lin<- tune(svm, medv~ ., data= Boston_train, kernel= "linear", ranges=list(cost=c(0.01,.1,1,5,10)))
summary(tune_svm_lin)

set.seed(100)
model_svm_1<- svm(medv~ ., data= Boston_train, kernel= "linear", cost= tune_svm_lin$best.parameters$cost)
summary(model_svm_1)

test_svm_lin<- predict(model_svm_1, Boston_test)
head(test_svm_lin)
test_svm_lin

summary(model_svm_1)$mse
confusionMatrix(Boston_test$medv, test_svm_lin)
Boston_test$medv
test_svm_lin

z<- 1:length(Boston_test$medv)
plot(z, Boston_test$medv, pch=18, col= 'black')
lines(z, test_svm_lin, lwd='2', col='red')
points(Boston_test$medv,test_svm_lin,col="blue", pch=4)

plot(tune_svm_lin)

#checkinhg the accuracy
#for MSE
library(DMwR)
regr.eval(Boston_test$medv, test_svm_lin)

#actual and predicted
actuals_pred<- data.frame(cbind(actuals= Boston_test$medv, predicteds=test_svm_lin ))
correlation_accuracy <- cor(actuals_pred)
correlation_accuracy
actuals_pred 

table(true= Boston_test$medv, pred= test_svm_lin)
# Radial SVM 
library(e1071)
model_reg = svm(medv~., data=Boston_train)
print(model_reg)

pred_1<- predict(model_reg, Boston_test)
print(pred_1)

#checking accuracy
#install.packages("DMwR")
#MSE-
library(DMwR)
regr.eval(Boston_test$medv, pred_1)
#actual and predicted
actuals_pred<- data.frame(cbind(actuals= Boston_test$medv, predicteds=pred_1 ))
correlation_accuracy <- cor(actuals_pred)
correlation_accuracy
head(actuals_pred) 

#tuning for finding the best parameters
set.seed(2)
tune_in<- tune(svm,medv~ ., data= Boston_train, kernel= 'radial', ranges= list(cost=c(0.01,.1,1,5,10,100), gamma=c(.01,.1,1,5,10)))
summary(tune_in)

#again tuning with narrower values
set.seed(2)
tune_in_2<- tune(svm,medv~ ., data= Boston_train, kernel= 'radial', ranges= list(cost=c(0.01,.1,1,5), gamma=c(.01,.02,.1)))
summary(tune_in_2)

plot(tune_in_2)
#Now making the Radial SVM with the tuned parameters
model_reg_1 = svm(medv~., data=Boston_train, cost= tune_in_2$best.parameters$cost, gamma= tune_in_2$best.parameters$gamma)
summary(model_reg_1)
#Now we predict for our test set with tuned parameters
pred_2<- predict(model_reg_1, Boston_test)
head(pred_2)

#plotting the points with our test prediction
x<- 1:length(Boston_test$medv)
plot(x, Boston_test$medv, pch=18, col= 'black')
lines(x, pred_2, lwd='2', col='red')
points(Boston_test$medv,pred_2,col="blue", pch=4)



#checking the accuracy of our new model 
#MSE for our updated model with tuned parameters
library(DMwR)
regr.eval(Boston_test$medv, pred_2)

#for actual and predicted plot
actuals_pred<- data.frame(cbind(actuals= Boston_test$medv, predicteds=pred_2 ))
correlation_accuracy <- cor(actuals_pred)
correlation_accuracy
# Final Prediction of our test set using radial SVM with tuned parameters 
actuals_pred 

#KNN 
library(class)
knn.pred<- knn(Boston_train, Boston_test$medv,Boston_train, k=1) 
table(Boston_test,knn.pred)

library(caret)
#making the training model with best value of k 
knn_1<- train(medv ~ ., data = Boston_train, method = "knn", preProcess = c("center", "scale"))
knn_1

#predicting the values with KNN on our test set
knnpredict<- predict(knn_1, newdata= Boston_test)
head(knnpredict)

#checking the accuracy 
#for actual and predicted 
actuals_pred<- data.frame(cbind(actuals= Boston_test$medv, predicteds=knnpredict ))
correlation_accuracy <- cor(actuals_pred)
correlation_accuracy
actuals_pred
#for MSE 
library(DMwR)
regr.eval(Boston_test$medv, knnpredict)
