#chap_7_Q4
x = -2:2
y = c(1 + 0 + 0, # x = -2
      1 + 0 + 0, # x = -1
      1 + 1 + 0, # x = 0
      1 + (1-0) + 0, # x = 1
      1 + (1-1) + 0 # x =2
)
plot(x,y)
#Q9
#a
set.seed(1)
library(MASS)
attach(Boston)
#
lm.fit = lm(nox ~ poly(dis, 3), data = Boston)
summary(lm.fit)
#
dislim = range(dis)
dis.grid = seq(from = dislim[1], to = dislim[2], by = 0.1)
lm.pred = predict(lm.fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, lm.pred, col = "red", lwd = 2)
#Summary shows that all polynomial terms are significant while predicting nox using dis. Plot shows a smooth curve fitting the data fairly well.
#b
all.rss= rep(NA, 10)
for(i in 1:10){
  lm.fit = lm(nox ~ poly(dis, i), data = Boston)
  all.rss[i] = sum(lm.fit$residuals^2)
}
all.rss
#c
library(boot)
all.deltas= rep(NA,10)
for (i in 1:10) {
  glm.fit = glm(nox ~ poly(dis, i), data = Boston)
  all.deltas[i] = cv.glm(Boston, glm.fit, K = 10)$delta[2]
}
plot(1:10, all.deltas, xlab = "Degree", ylab = "CV error", type = "l", pch = 20, 
     lwd = 2)
#A 10-fold CV shows that the CV error reduces as we increase degree from 1 to 3, stay almost constant till degree 5, and the starts increasing for higher degrees. We pick 4 as the best polynomial degree.
#d
#We see that dis has limits of about 1 and 13 respectively. We split this range in roughly equal 4 intervals and establish knots at [4,7,11]. Note: bs function in R expects either df or knots argument. If both are specified, knots are ignored.
library(splines)
sp.fit= lm(nox~ bs(dis, df=4, knots= c(4,7,11)), data=Boston)
summary(sp.fit)
#
sp.predict= predict(sp.fit, list(dis=dis.grid))
plot(nox~dis, data= Boston, col="darkgrey")
lines(dis.grid,sp.predict, col="red", lwd=2)
#The summary shows that all terms in spline fit are significant. Plot shows that the spline fits data well except at the extreme values of dis, (especially dis>10).
#e
#We fit regression splines with dfs between 3 and 16.
all.cv= rep(NA,16)
for(i in 3:16){
  lm.fit= lm(nox~bs(dis,df=i),data=Boston)
  all.cv[i]= sum(lm.fit$residuals^2)
}
all.cv[-c(1,2)]
#Train RSS monotonically decreases till df=14 and then slightly increases for df=15 and df=16.
#F
library(boot)
set.seed(42)
spline.mse=c()
for(df in 3:10){
  Boston.model=model.frame(nox~bs(dis,df=df),data=Boston)   
  names(Boston.model)=c('nox','bs.dis')                    
  # Note that because we are automatically setting the cutoffs we must do so in the entire dataset, otherwise predictions cannot be made.
  spline.fit=glm(nox~bs.dis,data=Boston.model)
  mse=cv.glm(spline.fit,data=Boston.model,K=10)$delta[1]
  spline.mse=c(spline.mse,mse)
}
plot(3:10,spline.mse,type='l',xlab='Df',ylab='Cross Val. MSE for Splines')
x=which.min(spline.mse)
points(x+2,spline.mse[x],col='red',pch=20,cex=2)
#It is clear that fitting model with 10 degree of freedom performs well in this dataset.
#Q10_a
library(ISLR)
set.seed(25)
train= sample(1:nrow(College),500)
test= -train
#
library(leaps)
forward= regsubsets(Outstate~., data=College, method='forward',nvmax=17)
plot(1/nrow(College)*summary(forward)$rss,type='l',xlab='Number of Predictors',ylab='Train MSE score',xaxt='n')
axis(side=1,at=seq(1,17,2),labels= seq(1,17,2))
#From the plot we see that a good choice is the model with 7 predictors. The predictors in this model are shown below.
#
which(summary(forward)$which[7,-1])
#b
#Here we fit a GAM by making use of smoothing splines for each of the predictors selected, except 'Private' since it is a qualitative predictor.
install.packages('gam')
library(gam)
gam.fit = gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) + s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), data = College[train, ])
par(mfrow = c(2, 3))
plot(gam.fit, se = T, col = "blue")
#c
gam.pred= predict(gam.fit, College[test,])
gam.mse= mean((College[test,'Outstate']-gam.pred)^2)
gam.mse
#Evaluating the model on the test set performs better than in the training set, since the MSE obtained is lower on the test set. Furthermore, below we can see that about 78% of the variance encountered in the data is explained by this model.
gam.tss= mean((College[test,'Outstate']- mean(College[test,'Outstate']))^2)
test.rss= 1- gam.mse/gam.tss
test.rss
#Q10_d
summary(gam.fit)
#From the output of ANOVA we can not that there is a significant evidence that a non-linear relationship for Expend,Grad.Rate,PhD,Personal, and Room.Board is present.