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


#LOOCV
set.seed (1)
glm.fit <- glm (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45, data = train)
cv.error.loocv <- cv.glm (train , glm.fit)$delta[1]
#MSE
cv.error.loocv

#5-fold cross-validation on training data
set.seed (1)

glm.fit.5 <- glm (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45, data = train)
cv.error.5 <- cv.glm (train , glm.fit.5 , K = 5)$delta[1]
#mse 5-cross validation
cv.error.5


#10-fold cross-validation on training data
set.seed (1)
glm.fit.10 <- glm (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45, data = train)
cv.error.10 <- cv.glm (train , glm.fit.10 , K = 10)$delta[1]
#mse 10-cross validation
cv.error.10



#Linear regression 

set.seed(1)
lm.fit.train.lr <- lm (lpsa ~ lcavol+ lweight+ age + lbph + svi+ lcp+ gleason+pgg45, data = train)

#test MSE
mean((test$lpsa - predict.lm(lm.fit.train.lr, test)) ^ 2)

# glm.fit.pred <- predict(glm.fit.train, test)
# 
# mean((test$lpsa - glm.fit.pred)^2)
# #coefficient 
# coef(lm.fit.train.lr)

#---------------------------
# c) fit ridge regression model
#---------------------------
#install.packages("glmnet")
library  (glmnet)
x.train <- model.matrix (lpsa ~ .,train)[, -1]
x.test <- model.matrix (lpsa ~ .,test)[, -1]
y.train <- train$lpsa
y.test <- test$lpsa

set.seed (1)
grid <- 10^ seq (10, -2, length = 100)
ridge.mod <- glmnet (x.train, y.train, alpha = 0, lambda = grid)
#plot showing the values of the coefficients in relation to the parameter λ 
plot(ridge.mod, xvar = "lambda",col = 1:8, label = TRUE,cex.axis = 1, cex.lab = 1.5)

#--------------------------
# d)  function cv.glmnet() that automatically performs k-fold cross validation using k = 10 folds.
#--------------------------
set.seed (1)
cv.model.r <- cv.glmnet(x.train, y.train, alpha = 0)
plot(cv.model.r,cex.axis = 1, cex.lab = 1.5)
best.lamdba.r = cv.model.r$lambda.min
best.lamdba.r

#coefficients for best lambda:
set.seed (1)
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
# also, glmnet() transforms qualitative variables like the trainTrue column
# into dummy variables
set.seed (1)
cv.model.l <- cv.glmnet (x.train, y.train, alpha = 1)
plot (cv.model.l,cex.axis = 1, cex.lab = 1.5)
best.lamdba.l <- cv.model.l$lambda.min
best.lamdba.l

lasso.coef <- predict(lasso.mod, type="coefficients", s=best.lamdba.l)
lasso.coef
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


