library("ISLR")
summary(Weekly)
plot(Weekly)
cor(Weekly[,-9])
table(Weekly$Direction)/sum(table(Weekly$Direction))
#Q6 part2
glm.fit <- glm(Direction~.-Year-Today,data=Weekly,family="binomial")
summary(glm.fit)
#part-3
glm.probs <- predict(glm.fit,type = "response")
glm.pred <- rep("Down",nrow(Weekly))
glm.pred[glm.probs>0.5] = "Up"
table(glm.pred,Weekly$Direction)
mean(glm.pred == Weekly$Direction)
558/(558+47)
56/(428+56)
#part-d
train <- Weekly[,"Year"] <= 2008

glm.fit <- glm(Direction~Lag2,data = Weekly,subset = train, family = "binomial")
summary(glm.fit)
glm.probs <- predict(glm.fit,Weekly[!train,],type = "response")

glm.pred <- rep("Down",nrow(Weekly))
glm.pred[glm.probs>0.5] = "Up"

table(glm.pred,Weekly[,"Direction"]) # Confusion Matrix.
mean(glm.pred == Weekly[,"Direction"]) # Fraction of correct predictions.
#part-e
library(MASS)

lda.fit <- lda(Direction~Lag2,data=Weekly,subset=train)
lda.fit

lda.pred <- predict(lda.fit,Weekly[!train,])
lda.class <- lda.pred$class

table(lda.class,Weekly[!train,9])

mean(lda.class == Weekly[!train,9])
#part-f
qda.fit <- qda(Direction~Lag2,data=Weekly,subset=train)
qda.fit
qda.pred <- predict(qda.fit,Weekly[!train,])
qda.class <- qda.pred$class

table(qda.class,Weekly[!train,9])
mean(qda.class == Weekly[!train,9])
#part-g
library(class)

train.X <- cbind(Weekly[train,3])
test.X <- cbind(Weekly[!train,3])

train.Direction <- Weekly[train,c(9)]
test.Direction <- Weekly[!train,c(9)]

knn.pred <- knn(train.X,test.X,train.Direction,k=1)

table(knn.pred,test.Direction)
mean(knn.pred == test.Direction)

#part-i
glm.fit <- glm(Direction~Lag2,data = Weekly,subset = train, family = "binomial")
summary(glm.fit)
glm.probs <- predict(glm.fit,Weekly[!train,],type = "response")

glm.pred <- rep("Down",nrow(Weekly))
glm.pred[glm.probs>0.55] = "Up"

table(glm.pred,Weekly[,"Direction"]) # Confusion Matrix.
mean(glm.pred == Weekly[,"Direction"]) # Fraction of correct predictions.
# Non Linear Transformation
# Up to Cubic power
glm.fit <- glm(Direction~Lag2+I(Lag2^2)+I(Lag2^3),data = Weekly,subset = train, family = "binomial")
summary(glm.fit)

glm.probs <- predict(glm.fit,Weekly[!train,],type = "response")

glm.pred <- rep("Down",nrow(Weekly))
glm.pred[glm.probs>0.5] = "Up"

table(glm.pred,Weekly[,"Direction"]) # Confusion Matrix.
mean(glm.pred == Weekly[,"Direction"]) # Fraction of correct predictions.
# Log
glm.fit <- glm(Direction~log10(abs(Lag2)),data = Weekly,subset = train, family = "binomial")
summary(glm.fit)

train.X <- cbind(Weekly[train,3])
test.X <- cbind(Weekly[!train,3])

train.Direction <- Weekly[train,c(9)]
test.Direction <- Weekly[!train,c(9)]


errors <- c()

maxK <- 100
step <- 2

for(j in seq(1,maxK,step)){
  knn.pred <- knn(train.X,test.X,train.Direction,k=j)
  table(knn.pred,test.Direction)
  errors <- c(1-mean(knn.pred == test.Direction),errors)
}

data <- cbind(seq(1,maxK,step),errors)
plot(data,type="l",xlab="k")

knn.pred <- knn(train.X,test.X,train.Direction,k=which.min(errors))
table(knn.pred,test.Direction)

mean(knn.pred == test.Direction)

# Q5 from 5.4
#part a
library(ISLR)
glm.fit=glm(default~income+balance,family='binomial',data=Default)
print(glm.fit)
#part b

#iset.seed(3)
subset=sample(1:1000,500) 
print(subset)
#ii
glm.fit=glm(default~income+balance,family='binomial',data=Default,subset=subset)
print(glm.fit)
#iii
glm.resp=predict(glm.fit,Default[-subset,],type='response')
glm.pred=ifelse(glm.resp>0.5,'Yes','No')
glm.pred
#iv
mean(glm.pred!=Default[-subset,'default'])
#part c
DefaultValidationFn = function(formula=default~income+balance,n=1000,s=500,seed){
  set.seed(seed)
  subset=sample(n,s)                                
  glm.fit=glm(formula,family='binomial',     
              data=Default,subset=subset) 
  
  glm.resp=predict(glm.fit,Default[-subset,],type='response') 
  
  mean(glm.pred!=Default[-subset,'default'])                 
}

for(i in 1:3) print(DefaultValidationFn(seed=i))
#part d
for(i in 1:3) print(DefaultValidationFn(formula=default~income+balance+student,seed=i))