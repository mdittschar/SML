
library (MASS)
library (PRROC)
library (boot)
library (leaps)
library (glmnet)
library (randomForest)
library (tree)
library (e1071)

#load data sets
wine.white = read.csv("../winequality-white.csv",sep = ";")
wine.red =  read.csv("../winequality-red.csv",sep = ";")

#inspect dataframe
str(wine.white)
summary(wine.white)
#Check is data has nan values
dim(wine.white)
sum(is.na(wine.white))
dim(wine.red)
sum(is.na(wine.red))

#--------------------------
#distribution if the wine quality
#---------------------------
dist.white.wine <-table(wine.white$quality)
dist.white.wine
barplot(dist.white.wine, xlab= "Quality of white wines", ylab="Number of white wines",ylim = c(0,2200))


#---------------------
#Linear Correlation
#---------------------
cor.val= cor(wine.white)
#uniq.white= unique(wine.white$quality)
#len.uniq.white=length(unique(wine.white$quality))
#palette()[1:(length(unique(wine.white$quality)))]
#pairs(wine.white[1:(ncol(wine.white)-1)],main = "Correlation Matrix for white wine data",pch = 21,bg = c(palette()[1:(length(unique(wine.white$quality)))])[unclass(wine.white$quality)])
cor.white= cor(wine.white [1:(ncol(wine.white)-1)],wine.white$quality )

#heatmap
col= colorRampPalette(c("blue", "white", "red"))(256)
heatmap(cor.val , Colv = NA, Rowv = NA,col=col, main= "Correlation Heatmap white wine", margins = c(10,10), xlab="Variables", ylab="Variables")
legend(x="left", legend=c("-1", "0", "1"),fill=c("blue", "white", "red"))

#boxplot
boxplot(wine.white[1:(ncol(wine.white)-1)])



# Split in training and test set 
set.seed (1)
testIndex.white = sample(1:4898, 4898*.3)
train.white.x = wine.white[-testIndex.white,1:11]
train.white.x["quality"] = wine.white$quality[-testIndex.white]
train.white.y = wine.white$quality[-testIndex.white]
test.white.x = wine.white[testIndex.white,1:11]
test.white.x["quality"] = wine.white$quality[testIndex.white]
test.white.y = wine.white$quality[testIndex.white]
# data.frame(x=x, y=as.factor(y))

#Split Red wine dataset
set.seed (1)
testIndex.red = sample(1:1599, 1599/4)
train.red.x = wine.red[-testIndex.red,1:11]
train.red.x["quality"] = wine.red$quality[-testIndex.red]
train.red.y = wine.red$quality[testIndex.red]
test.red.x = wine.red[testIndex.red,1:11]
test.red.x["quality"] = wine.red$quality[testIndex.red]
test.red.y = wine.red$quality[testIndex.red]

#Normalization of the data 

means = colMeans(train.white.x)
vars = apply(train.white.x, 2, var)

means.r = colMeans(train.red.x)
vars.r = apply(train.red.x, 2, var)

for(i in 1:11 ){
  train.white.x[,i] <- (train.white.x[,i] - means[i])/ sqrt(vars[i])
  test.white.x[,i] <- (test.white.x[,i] - means[i])/ sqrt(vars[i])
  
  train.red.x[,i] <- (train.red.x[,i] - means[i])/ sqrt(vars[i])
  test.red.x[,i] <- (test.red.x[,i] - means[i])/ sqrt(vars[i])
}

for(i in 1:11 ){
  wine.white[,i] <- (wine.white[,i] - colMeans(wine.white)[i])/ sqrt(apply(wine.white, 2, var)[i])
  
}

for(i in 1:11 ){
  wine.red[,i] <- (wine.red[,i] - colMeans(wine.red)[i])/ sqrt(apply(wine.red, 2, var)[i])
  
}

#--------------------------
# Multiple Linear Regression
#--------------------------

lm.white = lm(quality~., data= train.white.x)
sum.lm= summary(lm.white)
#train mse
mean(sum.lm$residuals^2)

#test mse 
lm.predict = predict(lm.white, test.white.x)
mean((test.white.x$quality - lm.predict)^2)

#---------------------------
# ridge 
#--------------------------

train.mat.white = model.matrix (quality ~ ., data=train.white.x)
train.y.white = train.white.x$quality

test.mat.white = model.matrix (quality ~., data= test.white.x)
test.y.white= test.white.x$quality


set.seed (1)
grid <- 10^ seq (10, -2, length = 100)
ridge.mod.white = glmnet (train.mat.white, train.y.white, alpha = 0, lambda = grid)
#plot(ridge.mod.white, xvar = "lambda",col = 1:11, label = TRUE,cex.axis = 1, cex.lab = 1.5)

set.seed (1)
cv.model.r.white = cv.glmnet(train.mat.white, train.y.white, alpha = 0)
#plot(cv.model.r.white,cex.axis = 1, cex.lab = 1.5)
best.lamdba.white = cv.model.r.white$lambda.min
best.lamdba.white

#coefficients for best lambda:
set.seed (1)
best.model.white = glmnet(train.mat.white, train.y.white, alpha = 0, lambda = best.lamdba.white)
coef(best.model.white)

#train set MSE
ridge.pred.train.white = predict (ridge.mod.white , s = best.lamdba.white ,
                             newx = train.mat.white) 

train.mse.r.white = mean ((ridge.pred.train.white - train.y.white)^2)
train.mse.r.white

#test set MSE
ridge.pred.white = predict (ridge.mod.white , s = best.lamdba.white ,
                       newx =test.mat.white) 

test.mse.r.white = mean ((ridge.pred.white - test.y.white)^2)
test.mse.r.white

#-----------------------------
# lasso
#-----------------------------
train.mat.white = model.matrix (quality ~ ., data=train.white.x)
train.y.white = train.white.x$quality

test.mat.white = model.matrix (quality ~., data= test.white.x)
test.y.white= test.white.x$quality

set.seed (1)
lasso.mod.white = glmnet (train.mat.white, train.y.white, alpha = 1,
                     lambda = grid)
#plot coefficients in relation to the parameter λ 
#plot(lasso.mod, xvar = "lambda",col = 1:8, label = TRUE, cex.axis = 1, cex.lab = 1.5)

set.seed (1)
cv.model.l.white = cv.glmnet (train.mat.white,  train.y.white, alpha = 1)
#plot (cv.model.l.white,cex.axis = 1, cex.lab = 1.5)
best.lamdba.l.white = cv.model.l.white$lambda.min
best.lamdba.l.white

lasso.coef = predict(lasso.mod.white, type="coefficients", s=best.lamdba.l.white)
lasso.coef
#train set MSE
lasso.pred.train.white = predict (lasso.mod.white , s = best.lamdba.l.white ,
                             newx = train.mat.white) 
train.mse.l.white = mean ((lasso.pred.train.white - train.y.white)^2)
train.mse.l.white

#test set MSE
lasso.pred.white = predict (lasso.mod.white , s = best.lamdba.l.white ,
                       newx = test.mat.white)
test.mse.l.white= mean ((lasso.pred.white - test.y.white)^2)
test.mse.l.white


#---------------------------
# Logistic regression 
#----------------------------
train.white.y = factor(train.white.y)
test.white.y = factor(test.white.y)
log.x = train.white.x
log.x$quality = ifelse (as.integer(log.x$quality) > 6, 1, 0)
# log.x$quality = as.numeric(log.x$quality)
# 
# log.x[log.x$quality >6, "quality"] = 1
# log.x[log.x$quality <= 6,"quality"] = 0
# glm.pred[glm.probs > .5] <- "1"
set.seed (1)
glm.model.white= glm(quality~.,data= log.x, family = "binomial")
glm.probs = predict (glm.model.white, test.white.x, type="response")
glm.probs[which(glm.probs > .5)] = 1
glm.probs[which(glm.probs <= .5)] = 0
table(glm.probs, test.white.x$quality > 6)
summary(glm.model.white)

#----------------------------
#LDA
#------------------------------
# fit lda on train data
set.seed(1)
lda.fit.white= lda (quality~. , data= train.white.x)
# get predictions for training and test data
#train.lda.pred.white = predict(lda.fit.white, train.white.x, type="response")
test.lda.pred.white = predict(lda.fit.white, test.white.x, type="response")
# get train and test accuracy
#mean(train.lda.pred.white$class == train.white.x$quality)
mean(test.lda.pred.white$class == test.white.x$quality)
# get confusion matrices
#table(train.lda.pred.white$class,train.white.x$quality)
table(test.lda.pred.white$class, test.white.x$quality)


#--------------------------
# Tree learning
#--------------------------
set.seed (1)
# train.white.x["quality"] = as.factor(train.white.x$quality)
# tree.wine = tree(quality~.,data= train.white.x, mindev=5*1e-3)
# plot(tree.wine)
# text(tree.wine)
# tree.predict  = predict(tree.wine, test.white.x)

set.seed (1)
tree.wine.cat = tree(factor(quality)~.,data= log.x)
plot(tree.wine.cat)
text(tree.wine.cat)
tree.predict  = predict(tree.wine.cat, test.white.x, type="class")
table(tree.predict, test.white.x$quality > 6)
tree.cat.acc = mean(tree.predict == ifelse(test.white.x$quality > 6, 1,0))
print(tree.cat.acc)
#--------------------------
#  Random Forest
#--------------------------
set.seed (1)
rf.wine = randomForest(factor(quality) ~ ., data= train.white.x)
rf.predict = predict(rf.wine, test.white.x)
rf.acc = mean(rf.predict == test.white.x$quality)


# random forest with categories
set.seed (1)
rf.cat.accs = c()
mdg = c()
for (i in 1:11){
  rf.cat.wine = randomForest(factor(quality) ~ ., mtry = i, data= log.x, importance=TRUE)
  mdg = append(mdg, rf.cat.wine$importance[,"MeanDecreaseGini"])
  rf.cat.predict = predict(rf.cat.wine, test.white.x)
  table(rf.cat.predict, test.white.x$quality > 6)
  rf.cat.acc = mean(rf.cat.predict == ifelse(test.white.x$quality > 6, 1,0))
  rf.cat.accs = append(rf.cat.accs, rf.cat.acc)
}

plot(rf.cat.accs,ylim=c(0,1))
varImpPlot(rf.cat.wine)
for(k in 1:11){
  plot(mdg[seq(k, length(mdg), 11)], ylim=c(0, 260))
}

rf.cat.accs = c()
for (i in c(100,200,500,1000,2000)){
  rf.cat.wine = randomForest(factor(quality) ~ ., ntree=i, data= log.x, importance=TRUE)
  print(rf.cat.wine$importance[,"MeanDecreaseGini"])
  rf.cat.predict = predict(rf.cat.wine, test.white.x)
  table(rf.cat.predict, test.white.x$quality > 6)
  rf.cat.acc = mean(rf.cat.predict == ifelse(test.white.x$quality > 6, 1,0))
  rf.cat.accs = append(rf.cat.accs, rf.cat.acc)
}

plot(rf.cat.accs,ylim=c(0,1))
varImpPlot(rf.cat.wine)

#---------------------------
# Naive Bayes
#---------------------------

set.seed(1)
# learn naive bayes classifier on th train set
white.bayes = naiveBayes(quality ~., data=log.x)
print(white.bayes)
# predict on test set
bayes_predict = predict(white.bayes, test.white.x, type="class")
# test accuracy on the test set
bayes_acc = mean(bayes_predict == ifelse(test.white.x$quality > 6, 1,0))
str(white.bayes)
#---------------------------
# PCA
#---------------------------
white.pca <- prcomp(wine.white[1:11], center = TRUE,scale. = TRUE)
str(white.pca)
#plot(white.pca$rotation[,"PC1"],white.pca$rotation[,"PC2"] )
plot(x=white.pca$x[,"PC1"], y=white.pca$x[,"PC2"], xlab="PC1", ylab="PC2", col=wine.white$quality)#, xlim=c(-0.6, 0.6))#, ylim=c(-.7, .7))
arrows(x0=rep(0,12), y0=rep(0,12), x1=white.pca$rotation[,"PC1"]*15, y1=white.pca$rotation[,"PC2"]*15, xlab="PC1", ylab="PC2")
text(x=white.pca$rotation[,"PC1"]*15-.45, y=white.pca$rotation[,"PC2"]*15+.45, labels=colnames(wine.white)[1:11])
title("PCA on normalized data")

#---------------------------
# task b) Append a) to winequality.red 
#---------------------------
str(wine.red)
summary(wine.red)

dist.red.wine =table(wine.red$quality)
dist.red.wine
barplot(dist.red.wine, xlab= "Quality of red wine", ylab="Number of red wines",ylim = c(0,700))

cor.red= cor(wine.red)
heatmap(cor.red , Colv = NA, Rowv = NA,col=col, main= "Correlation Heatmap red wine", margins = c(10,10), xlab="Variables", ylab="Variables")
legend(x="left", legend=c("-1", "0", "1"),fill=c("blue", "white", "red"))
# 
#--------------------------
# Multiple Linear Regression
#--------------------------

lm.red = lm(quality~., data= train.red.x)
sum.lm.red= summary(lm.red)
sum.lm.red
#train mse
mean(sum.lm.red$residuals^2)

#test mse 
lm.predict.red = predict(lm.red, test.red.x)
mean((test.red.x$quality - lm.predict.red)^2)



#---------------------------
# ridge 
#--------------------------

train.mat.red = model.matrix (quality ~ ., data=train.red.x)
train.y.red = train.red.x$quality

test.mat.red = model.matrix (quality ~., data= test.red.x)
test.y.red= test.red.x$quality


set.seed (1)
grid <- 10^ seq (10, -2, length = 100)
ridge.mod.red = glmnet (train.mat.red, train.y.red, alpha = 0, lambda = grid)
#plot(ridge.mod.red, xvar = "lambda",col = 1:11, label = TRUE,cex.axis = 1, cex.lab = 1.5)

set.seed (1)
cv.model.r.red = cv.glmnet(train.mat.red, train.y.red, alpha = 0)
plot(cv.model.r.red,cex.axis = 1, cex.lab = 1.5)
best.lamdba.red = cv.model.r.red$lambda.min
best.lamdba.red

#coefficients for best lambda:
set.seed (1)
best.model.red = glmnet(train.mat.red, train.y.red, alpha = 0, lambda = best.lamdba.red)
coef(best.model.red)

#train set MSE
ridge.pred.train.red = predict (ridge.mod.red , s = best.lamdba.red ,
                                newx = train.mat.red) 

train.mse.r.red = mean ((ridge.pred.train.red - train.y.red)^2)
train.mse.r.red

#test set MSE
ridge.pred.red = predict (ridge.mod.red , s = best.lamdba.red ,
                          newx =test.mat.red) 

test.mse.r.red = mean ((ridge.pred.red - test.y.red)^2)
test.mse.r.red

#-----------------------------
# lasso
#-----------------------------
train.mat.red = model.matrix (quality ~ ., data=train.red.x)
train.y.red = train.red.x$quality

test.mat.red = model.matrix (quality ~., data= test.red.x)
test.y.red= test.red.x$quality

set.seed (1)
lasso.mod.red = glmnet (train.mat.red, train.y.red, alpha = 1,
                        lambda = grid)
#plot coefficients in relation to the parameter λ 
#plot(lasso.mod, xvar = "lambda",col = 1:8, label = TRUE, cex.axis = 1, cex.lab = 1.5)

set.seed (1)
cv.model.l.red = cv.glmnet (train.mat.red,  train.y.red, alpha = 1)
#plot (cv.model.l.red,cex.axis = 1, cex.lab = 1.5)
best.lamdba.l.red = cv.model.l.red$lambda.min
best.lamdba.l.red

lasso.coef = predict(lasso.mod.red, type="coefficients", s=best.lamdba.l.red)
lasso.coef
#train set MSE
lasso.pred.train.red = predict (lasso.mod.red , s = best.lamdba.l.red ,
                                newx = train.mat.red) 
train.mse.l.red = mean ((lasso.pred.train.red - train.y.red)^2)
train.mse.l.red

#test set MSE
lasso.pred.red = predict (lasso.mod.red , s = best.lamdba.l.red ,
                          newx = test.mat.red)
test.mse.l.red= mean ((lasso.pred.red - test.y.red)^2)
test.mse.l.red


#---------------------------
# Logistic regression 
#----------------------------
train.red.y = factor(train.red.y)
test.red.y = factor(test.red.y)
log.x = train.red.x
log.x$quality = ifelse (as.integer(log.x$quality) > 6, 1, 0)
# log.x$quality = as.numeric(log.x$quality)
# 
# log.x[log.x$quality >6, "quality"] = 1
# log.x[log.x$quality <= 6,"quality"] = 0
# glm.pred[glm.probs > .5] <- "1"

set.seed (1)
glm.model.red= glm(quality~.,data= log.x, family = "binomial")
glm.probs = predict (glm.model.red, test.red.x, type="response")
glm.probs[which(glm.probs > .5)] = 1
glm.probs[which(glm.probs <= .5)] = 0
table(glm.probs, test.red.x$quality > 6)
summary(glm.model.red)

