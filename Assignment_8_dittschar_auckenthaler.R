
library (MASS)
library (PRROC)
library (boot)
library (leaps)
library (glmnet)
library (randomForest)
library (tree)

#load data sets
wine.white = read.csv("../winequality-white.csv",sep = ";")
wine.red =  read.csv("../winequality-red.csv",sep = ";")

#inspect dataframe
str(wine.white)
summary(wine.white)

#distribution if the wine quality
dist.white.wine <-table(wine.white$quality)
dist.white.wine
barplot(dist.white.wine, xlab= "Quality of white wines", ylab="Number of white wines",ylim = c(0,2200))

#Correlation Plot
corr(wine.white)
#uniq.white= unique(wine.white$quality)
#len.uniq.white=length(unique(wine.white$quality))
#palette()[1:(length(unique(wine.white$quality)))]
#pairs(wine.white[1:(ncol(wine.white)-1)],main = "Correlation Matrix for white wine data",pch = 21,bg = c(palette()[1:(length(unique(wine.white$quality)))])[unclass(wine.white$quality)])
cor(wine.white [1:(ncol(wine.white)-1)], use = "complete.obs")


# Split in training and test set 
set.seed (1)
summary(wine.white)

testIndex.white = sample(1:4898, 4898/4)
train.white.wine.x = wine.white[-testIndex.white,]
train.white.wine.y = wine.white$quality[testIndex.white]
test.white.wine.x = wine.white[testIndex.white,]
test.white.wine.y = wine.white$quality[testIndex.white]
#--------------------------
# Multiple Linear Regression
# > lm.fit <- lm(medv ∼ lstat + age , data = Boston)
#> summary (lm.fit)
#--------------------------

#---------------------------
# Logistic regression / bzw. LDA
#----------------------------
train.white.wine.x$quality = factor(train.white.wine.x$quality)
test.white.wine.x$quality = factor(test.white.wine.x$quality)

glm.model.white= glm (quality~. , data= train.white.wine.x, family = binomial)
glm.probs = predict (glm.model.white, test.white.wine.x, type="response")

#LDA
# fit lda on train data
set.seed(1)
lda.fit.white= lda (quality~. , data= train.white.wine.x)
# get predictions for training and test data
#train.lda.pred.white = predict(lda.fit.white, train.white.wine.x, type="response")
test.lda.pred.white = predict(lda.fit.white, test.white.wine.x, type="response")
# get train and test accuracy
#mean(train.lda.pred.white$class == train.white.wine.x$quality)
mean(test.lda.pred.white$class == test.white.wine.x$quality)
# get confusion matrices
#table(train.lda.pred.white$class,train.white.wine.x$quality)
table(test.lda.pred.white$class, test.white.wine.x$quality)


#---------------------------
# task b) Append a) to winequality.red 
#---------------------------
str(wine.red)
summary(wine.red)

dist.red.wine =table(wine.red$quality)
dist.red.wine
barplot(dist.red.wine, xlab= "Quality of red wine", ylab="Number of red wine",ylim = c(0,2200))

#--------------------------
# Multiple Linear Regression
# > lm.fit <- lm(medv ∼ lstat + age , data = Boston)
#> summary (lm.fit)
#--------------------------

lm.white <- lm(quality~., data= wine.white[trainIndex.white,])
summary(lm.white)

lm.red <- lm(quality~., data= wine.red[trainIndex.red,])
summary(lm.red)

#--------------------------
# Tree learning
#--------------------------
train.white.x$quality = as.factor(train.white.x$quality)
tree.wine = tree(quality~.,data= train.white.x, mindev=5*1e-3)
plot(tree.wine)
text(tree.wine)
tree.predict  = predict(tree.wine, test.x)

#--------------------------
#  Random Forest
#--------------------------

rf.wine = randomForest(quality ~ ., data= train.white.x)
rf.predict = predict(rf.wine, test.white.x)
rf.acc = mean(rf.predict == test.white.x$quality)
# trainIndex = sample(1:1599, 1599/4)
# train.red.wine.x = winequality.red[trainIndex,]
# train.red.wine.y = winequality.red$quality[trainIndex]
# 
# test.red.wine.x = winequality.red[-trainIndex,]
# test.red.wine.y = winequality.red$quality[-trainIndex]