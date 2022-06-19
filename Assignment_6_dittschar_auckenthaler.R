library(MASS)

#----------------------
# a)
#----------------------
#load data 
data <- read.table("./prostate.txt")

# normalization over column 1 to 8 
for(i in 1:8 ){ 
  data[,i] <- (data[,i] - mean(data[,i]))/ sd(data[,i])
}

#split data 
train <- data[data$train == TRUE, ]
test <- data [data$train == FALSE, ]

#----------------------
# b) LOOCV, 5- and 10-fold cross-validation on the training data set
#----------------------
library (boot)
library( leaps)

#CHECK IT AGAIN!!!!
#NOT NEEDED INSIDE HERE--------------------------------
set.seed (1)
regfit.best <- regsubsets (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45,data = train, nvmax = 8)
test.mat <- model.matrix (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45,data = train)

val.errors <- rep (0, 8)
for (i in 1:8) {
  coefi <- coef(regfit.best, id= i)
  pred <- test.mat[, names (coefi)] %*% coefi
  val.errors[i] <- mean((test$lpsa-pred)^2)
}
#--------------------------------------------------
#ross validation 5 
k.5<-5
n<- nrow(train)
set.seed (1)

folds <- sample ( rep (1:k.5, length = n))
cv.errors.5 <- matrix (NA, k.5, 8,
                     dimnames = list (NULL , paste (1:8)))
for (j in 1:k.5) {
  best.fit.5 <- regsubsets (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45,data = train[folds != j, ],
                            nvmax = 8)
  
  for (i in 1:8) {
    coefi <- coef(best.fit.5, id= i)
    pred <- test.mat[, names (coefi)] %*% coefi
    cv.errors.5[j,i] <- mean((test$lpsa-pred)^2)
  }
}
mean.cv.errors.5 <- apply (cv.errors.5 , 2, mean)
mean.cv.errors.5
par (mfrow = c(1, 1))
plot (mean.cv.errors.5 , type = "b")

reg.best.5 <- regsubsets (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45,data = train[folds != j, ],
                          nvmax = 8)
coef(reg.best.5,1)

k.10<-10
n<- nrow(train)
set.seed (1)

folds <- sample ( rep (1:k.10, length = n))
cv.errors.10 <- matrix (NA, k.10, 8,
                       dimnames = list (NULL , paste (1:8)))
for (j in 1:k.10) {
  best.fit.10 <- regsubsets (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45,data = train[folds != j, ],
                            nvmax = 8)#
  
  for (i in 1:8) {
    coefi <- coef(best.fit.10, id= i)
    pred <- test.mat[, names (coefi)] %*% coefi
    cv.errors.10[j,i] <- mean((test$lpsa-pred)^2)
  }
}
mean.cv.errors.10 <- apply (cv.errors.10 , 2, mean)
mean.cv.errors.10
par (mfrow = c(1, 1))
plot (mean.cv.errors.10 , type = "b")

reg.best.10 <- regsubsets (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45,data = train[folds != j, ],
                          nvmax = 8)
coef(reg.best.10,1)

#5-fold cross-calidation on training data
#set.seed (1)
#cv.error.5 <- rep (0, 5)
#for (i in 1:5) {
   #glm.fit.5 <- glm (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45, data = train)
  # cv.error.5[i] <- cv.glm (train , glm.fit.5 , K = 5)$delta[1]
#}
#cv.error.5


#10-fold cross-calidation on training data
#set.seed (1)
#cv.error.10 <- rep (0, 10)
#for (i in 1:10) {
  #glm.fit.10 <- glm (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45, data = train)
  #cv.error.10[i] <- cv.glm (train , glm.fit.10 , K = 10)$delta[1]
#}
#cv.error.10


#Linear regression 
set.seed(1)
lm.fit.train.lr <- lm (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45, data = train)
model_summ <-summary(lm.fit.train.lr)
#train MSE
mean(model_summ$residuals^2)
#test MSE
mean((test$lpsa - predict.lm(lm.fit.train.lr, test)) ^ 2)





##### HIER FEHLT NOCH ETWAS!!!

#---------------------------
# c) fit ridge regression model
#---------------------------
#install.packages("glmnet")
library  (glmnet)
x.train <- model.matrix (lpsa ~ .,train)[, -1]
x.test <- model.matrix (lpsa ~ .,test)[, -1]
y.train <- train$lpsa
y.test <- test$lpsa

grid <- 10^ seq (10, -2, length = 100)
set.seed (1)
ridge.mod <- glmnet (x.train, y.train, alpha = 0, lambda = grid)
#plot showing the values of the coefficients in relation to the parameter λ 
plot(ridge.mod, xvar = "lambda",col = 1:8, label = TRUE,cex.axis = 1, cex.lab = 1.5)

#--------------------------
# d)  function cv.glmnet() that automatically performs k-fold cross validation using k = 10 folds.
#--------------------------
set.seed(1)
cv.model.r <- cv.glmnet(x.train, y.train, alpha = 0)
plot(cv.model.r,cex.axis = 1, cex.lab = 1.5)
best.lamdba.r = cv.model.r$lambda.min
best.lamdba.r

#coefficients for best lamdba:
set.seed(1)
best.model.r <- glmnet(x.train, y.train, alpha = 0, lambda = best.lamdba.r)
coef(best.model.r)

#train set MSE
ridge.pred.train <- predict (ridge.mod , s = best.lamdba.r ,
                       newx = x.train) 
train.mse.r <- mean ((ridge.pred.train - y.train)^2)
train.mse.r

#test set MSE
ridge.pred <- predict (ridge.mod , s = best.lamdba.r ,
                       newx = x.test) 

test.mse.r <- mean ((ridge.pred - y.test)^2)
test.mse.r
#--------------------------
# e) fit lasso model
#--------------------------
set.seed (1)
lasso.mod <- glmnet (x.train, y.train, alpha = 1,
                     lambda = grid)
#plot coefficients in relation to the parameter λ 
plot(lasso.mod, xvar = "lambda",col = 1:8, label = TRUE, cex.axis = 1, cex.lab = 1.5)


#--------------------------
# f) function cv.glmnet() that automatically performs k-fold cross validation using k = 10 folds.
#--------------------------
set.seed(1)
cv.model.l <- cv.glmnet (x.train, y.train, alpha = 1)
plot (cv.model.l,cex.axis = 1, cex.lab = 1.5)
best.lamdba.l <- cv.model.l$lambda.min
best.lamdba.l

# find coefficients to best lamdba model
set.seed(1)
best.model.l <- glmnet(x.train, y.train, alpha = 0, lambda = best.lamdba.l)
coef(best.model.l)

#train set MSE
lasso.pred.train <- predict (lasso.mod , s = best.lamdba.l ,
                             newx = x.train) 
train.mse.l <- mean ((lasso.pred.train - y.train)^2)
train.mse.l

#test set MSE
lasso.pred <- predict (lasso.mod , s = best.lamdba.l ,
                       newx = x.test)
test.mse.l<- mean ((lasso.pred - y.test)^2)
test.mse.l
