
library (MASS)
library (PRROC)
library (boot)
library (leaps)
library (glmnet)
library (randomForest)
library (tree)

#load data sets
wines.white = read.csv("../winequality-white.csv",sep = ";")
wines.red =  read.csv("../winequality-red.csv",sep = ";")

#inspect dataframe
str(wines.white)
summary(wines.white)

#distribution if the wine quality
dist.white.wine = table(wines.white$quality)
dist.white.wine
barplot(dist.white.wine, xlab= "Quality of white wines", ylab="Number of white wines",ylim = c(0,2200))


# Split in training and test set 
set.seed (1)
summary(wines)

trainIndex.white = sample(1:4898, 4898/4)
train.white.x = wines.white[trainIndex.white,]
train.white.y = wines.white$quality[trainIndex.white]

test.white.x = wines.white[-trainIndex.white,]
test.white.y = wines.white$quality[-trainIndex.white]

trainIndex.red = sample(1:1599, 1599/4)
train.red.x = wines.red[trainIndex.red,]
train.red.y = wines.red$quality[trainIndex.red]

test.red.x = wines.red[-trainIndex.red,]
test.red.y = wines.red$quality[-trainIndex.red]

#--------------------------
# Multiple Linear Regression
# > lm.fit <- lm(medv ∼ lstat + age , data = Boston)
#> summary (lm.fit)
#--------------------------

lm.white <- lm(quality~., data= wines.white[trainIndex.white,])
summary(lm.white)

lm.red <- lm(quality~., data= wines.red[trainIndex.red,])
summary(lm.red)

#---------------------------
# task b) Append a) to winequality.red 
#---------------------------
str(winequality.red)
summary(winequality.red)

dist.red.wine = table(winequality.red$quality)
dist.red.wine
barplot(dist.red.wine, xlab= "Quality of red wines", ylab="Number of red wines",ylim = c(0,2200))

# trainIndex = sample(1:1599, 1599/4)
# train.red.wine.x = winequality.red[trainIndex,]
# train.red.wine.y = winequality.red$quality[trainIndex]
# 
# test.red.wine.x = winequality.red[-trainIndex,]
# test.red.wine.y = winequality.red$quality[-trainIndex]