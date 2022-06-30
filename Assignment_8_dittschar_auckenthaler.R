
library (MASS)
library (PRROC)
library (boot)
library (leaps)
library (glmnet)
library (randomForest)
library (tree)

#load data sets
winequality.white<-read.csv("../winequality-white.csv",sep = ";")
winequality.red <- read.csv("../winequality-red.csv",sep = ";")

#inspect dataframe
str(winequality.white)
summary(winequality.white)

#distribution if the wine quality
dist.white.wine <-table(winequality.white$quality)
dist.white.wine
barplot(dist.white.wine, xlab= "Quality of white wines", ylab="Number of white wines",ylim = c(0,2200))


# Split in training and test set 
set.seed (1)
wines = read.csv("winequality-white.csv", sep=";")
summary(wines)

trainIndex = sample(1:4898, 4898/4)
train.white.wine.x = winequality.white[trainIndex,]
train.white.wine.y = winequality.white$quality[trainIndex]

test.white.wine.x = winequality.white[-trainIndex,]
test.white.wine.y = winequality.white$quality[-trainIndex]
#--------------------------
# Multiple Linear Regression
# > lm.fit <- lm(medv ∼ lstat + age , data = Boston)
#> summary (lm.fit)
#--------------------------




#---------------------------
# task b) Append a) to winequality.red 
#---------------------------
str(winequality.red)
summary(winequality.red)

dist.red.wine <-table(winequality.red$quality)
dist.red.wine
barplot(dist.red.wine, xlab= "Quality of red wines", ylab="Number of red wines",ylim = c(0,2200))

trainIndex = sample(1:1599, 1599/4)
train.red.wine.x = winequality.red[trainIndex,]
train.red.wine.y = winequality.red$quality[trainIndex]

test.red.wine.x = winequality.red[-trainIndex,]
test.red.wine.y = winequality.red$quality[-trainIndex]