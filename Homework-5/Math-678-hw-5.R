#Q4_part_2
par(xpd=NA)
plot(NA,NA,type='n',xlim=c(-2,2),ylim=c(-3,3),xlab='X1',ylab='X2')
#X2 < 1
lines(x=c(-2,2),y= c(1,1))
#X1<1 with X2<1
lines(x=c(1,1),y=c(-3,1))
text(x=(-2+1)/2, y=-1, labels=c(-1.8))
text(x=1.5, y=-1, labels=c(0.63))
#X2<2 with X2>=1
lines(x=c(-2,2),y=c(2,2))
text(x=0,y=2.5, labels= c(2.49))
#X1<0 with X2<2 and X2>=1
lines(x= c(0,0),y=c(1,2))
text(x=-1,y=1.5, labels=c(-1.06))
text(x=1,y=1.5,labels=c(0.21))
#Q8_part_a
library(ISLR)
set.seed(1)
train<- sample(1:nrow(Carseats),nrow(Carseats)/2)
carseats.train<- Carseats[train, ]
carseats.test<- Carseats[-train, ]
#Q8_part_b
library(tree)
tree.carseats<- tree(Sales~ ., data= carseats.train)
summary(tree.carseats)
#run separate
plot(tree.carseats)
text(tree.carseats, pretty= 0)
#
errorrate<- predict(tree.carseats, newdata= carseats.test)
mean((errorrate- carseats.test$Sales)^2)
#Q8_part_c
cv.carseats= cv.tree(tree.carseats, FUN= prune.tree)
par(mfrow= c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type='b')
plot(cv.carseats$k, cv.carseats$dev, type='b')
#best size=9
pruned.carseats<- prune.tree(tree.carseats, best=8)
par(mfrow= c(1,1))
plot(pruned.carseats)
text(pruned.carseats, pretty=0)
#
pred.prune<- predict(pruned.carseats,carseats.test)
mean((carseats.test$Sales- pred.prune)^2)
#Q8_part_d
install.packages('randomForest')
library(randomForest)
bagging.carseats<- randomForest(Sales~ ., data = carseats.train, mtry= 10, ntree = 500, importance= TRUE)
bagging.pred<- predict(bagging.carseats,carseats.test)
mean((carseats.test$Sales- bagging.pred)^2)
#
importance(bagging.carseats)
#Price,ShelveLoc and CompPrice are three most important variables 
#Q8_part_e
random_f<- randomForest(Sales ~., data= carseats.train, mtry=3,ntree=500,importance=TRUE)
random_pred<-predict(random_f,carseats.test)
mean((carseats.test$Sales- random_pred)^2)
#
importance(random_f)
#We again see that price shevlock and compprice are the most important var
#Q10_a
library(ISLR)
Hitters<- na.omit(Hitters)
Hitters$Salary<- log(Hitters$Salary)
#Q10_b
train<- 1:200
Hitters.train= Hitters[train,]
Hitters.test= Hitters[-train,]
#Q10_c
install.packages('gbm')
library(gbm)
set.seed(103)
pows=seq(-10, -0.2, by=0.1)
lambdas= 10^pows
length.lambdas= length(lambdas)
train.errors= rep(NA, length.lambdas)
test.errors= rep(NA,length.lambdas)
for(i in 1:length.lambdas){
  boost.hitters= gbm(Salary~ ., data= Hitters.train, distribution = 'gaussian',
  n.trees= 1000, shrinkage= lambdas[i])
  train.pred= predict(boost.hitters, Hitters.train, n.trees= 1000)
  test.pred= predict(boost.hitters,Hitters.test, n.trees= 1000)
  train.errors[i]= mean((Hitters.train$Salary- train.pred)^2)
  test.errors[i]= mean((Hitters.test$Salary- test.pred)^2)
}
plot(lambdas, train.errors, type='b',xlab='Shrinkage',ylab='Train MSE',col='blue',pch=20)
#Q10_d
plot(lambdas,test.errors, type='b',xlab='Shrinkage',ylab='Test MSE',col='blue',pch=20 )
#
min(test.errors)
#
lambdas[which.min(test.errors)]
#Q10_e
install.packages('glmnet')
library(glmnet)
lm.fit= lm(Salary~ ., data= Hitters.train)
lm.pred= predict(lm.fit, Hitters.test)
mean((Hitters.test$Salary- lm.pred)^2)
#
set.seed(1)
x= model.matrix(Salary ~., data= Hitters.train)
y= Hitters.train$Salary
x.test= model.matrix(Salary~ ., data= Hitters.test)
lassso.fit= glmnet(x,y,alpha=1)
lasso.pred= predict(lassso.fit, s=0.01, newx= x.test)
mean((Hitters.test$Salary- lasso.pred)^2)
#Both linear model and regularization like Lasso have higher test MSE than boosting
#Q10_f
library(gbm)
boosting.best<- gbm(Salary~ ., data= Hitters.train, distribution = 'gaussian',
                    n.trees= 1000, shrinkage= lambdas[which.min(test.errors)])
summary(boosting.best)
#CAtBat, CRBI and CWalks are three most important variables in that order.
#10_g
library(randomForest)
set.seed(25)
rf.hitters<- randomForest(Salary~ ., data= Hitters.train, ntree=500, mtry=19)
rf.pred<- predict(rf.hitters, Hitters.test)
mean((Hitters.test$Salary- rf.pred)^2)
#Test MSE for bagging is 0.23 which is slightly lower than the best MSE for boosting
#9.7_3_a
x1= c(3,2,4,1,2,4,4)
x2= c(4,2,4,4,1,3,1)
colors=(c('blue','blue','blue','blue','red','red','red'))
plot(x1,x2,col=colors,xlim=c(0,6),ylim=c(0,6))
#3_b
plot(x1,x2,col=colors,xlim=c(0,),ylim=c(0,5))
abline(-0.5,1)
#3_c
#0.5−X1+X2>0
#3_d
plot(x1,x2,col=colors,xlim=c(0,5),ylim=c(0,5))
abline(-0.5,1)
abline(-1,1,lty=2)
abline(0,1,lty=2)
#3_e
plot(x1,x2,col=colors,xlim=c(0,5),ylim=c(0,5))
abline(-0.5,1)
arrows(2,1,2,1.5)
arrows(2,2,2,1.5)
arrows(4,4,4,3.5)
arrows(4,3,4,3.5)
#3_f
#A slight movement of observation #7 (4,1) blue would not have an effect on the maximal margin hyperplane since its movement would be outside of the margin.
#3_g
plot(x1,x2,col=colors,xlim=c(0,5),ylim=c(0,5))
abline(-.8,1)
#−0.8−X1+X2>0
#3_h
plot(x1,x2,col=colors,xlim=c(0,5),ylim=c(0,5))
points(c(4),c(2),col=c("blue"))
#9.7_7_a
library(ISLR)
gas.median= (median(Auto$mpg))
factor_med= ifelse(Auto$mpg> gas.median, 1, 0)
Auto$mpglevel= as.factor(factor_med)
#7_b
install.packages('e1071')
library(e1071)
set.seed(3250)
tune.in<- tune(svm,mpglevel~ ., data=Auto, kernel='linear', ranges= list(cost= c(0.01,0.1,1,5,10,100)))
summary(tune.in)
#We see that cross-validation error is minimized for cost=1.
#7_c
set.seed(22)
tune.out= tune(svm, mpglevel~ ., data=Auto, kernel= "polynomial", ranges= list(cost=c(0.1,1,5,10),degree=c(2,3,4)))
summary(tune.out)
#The lowest cross-validation error is obtained for cost=10 and degree=2.
set.seed(461)
tune.out<- tune(svm, mpglevel~ ., data=Auto, kernel='radial',ranges=list(cost= c(0.1,1,5,10), gamma= c(0.01,.1,1,5,10,100)))
summary(tune.out)
#Finally, for radial basis kernel, cost=10 and gamma=0.01.
#9.7_d
svm.linear<- svm(mpglevel~ ., data=Auto,kernel="linear",cost=1)
svm.poly= svm(mpglevel~ ., data= Auto, kernel='polynomial',cost=10,degree=2)
svm.radical= svm(mpglevel~ ., data=Auto, kernel='radial',cost=10,gamma=0.01)
plotpairs<- function(fit){
  for(name in names(Auto)[!(names(Auto) %in% c('mpg','mpglevel','name'))]) {
    plot(fit,Auto,as.formula(paste('mpg~',name,sep="")))
  }
}
plotpairs(svm.linear)
plotpairs(svm.poly)
plotpairs(svm.radial)