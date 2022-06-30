
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
dist.white.wine = table(wine.white$quality)
dist.white.wine
barplot(dist.white.wine, xlab= "Quality of white wines", ylab="Number of white wines",ylim = c(0,2200))


# Split in training and test set 
set.seed (1)
summary(wine.white)

trainIndex.white = sample(1:4898, 4898/4)
train.white.x = wine.white[trainIndex.white,]
train.white.y = wine.white$quality[trainIndex.white]

test.white.x= wine.white[-trainIndex.white,]
test.white.y = wine.white$quality[-trainIndex.white]
set.seed(1)
trainIndex.red = sample(1:1599, 1599/4)
train.red.x = wine.red[trainIndex.red,]
train.red.y = wine.red$quality[trainIndex.red]

test.red.x = wine.red[-trainIndex.red,]
test.red.y = wine.red$quality[-trainIndex.red]


#---------------------------
# task b) Append a) to winequality.red 
#---------------------------
str(winequality.red)
summary(winequality.red)

dist.red.wine = table(winequality.red$quality)
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


# trainIndex = sample(1:1599, 1599/4)
# train.red.wine.x = winequality.red[trainIndex,]
# train.red.wine.y = winequality.red$quality[trainIndex]
# 
# test.red.wine.x = winequality.red[-trainIndex,]
# test.red.wine.y = winequality.red$quality[-trainIndex]