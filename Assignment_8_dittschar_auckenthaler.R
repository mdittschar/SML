
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
#cor(wine.white [1:(ncol(wine.white)-1)], use = "complete.obs")
cor.white= cor(wine.white [1:(ncol(wine.white)-1)],wine.white$quality )

col= colorRampPalette(c("blue", "white", "red"))(256)
heatmap(cor.val , Colv = NA, Rowv = NA,col=col, main= "Correlation Heatmap white wine", margins = c(10,10), xlab="Variables", ylab="Variables")
legend(x="left", legend=c("-1", "0", "1"),fill=c("blue", "white", "red"))
#boxplot
boxplot(wine.white[1:(ncol(wine.white)-1)])




# Split in training and test set 
set.seed (1)
summary(wine.white)

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
testIndex.red = sample(1:4898, 4898/4)
train.red.x = wine.red[-testIndex.red,1:11]
train.red.y = wine.red$quality[testIndex.red]
test.red.x = wine.red[testIndex.red,1:11]
test.red.y = wine.red$quality[testIndex.red]

#Normalization of the data 

means = colMeans(train.white.x)
vars = apply(train.white.x, 2, var)

means.r = colMeans(train.red.x)
vars.r = apply(train.red.x, 2, var)

for(i in 1:11 ){
  train.white.x[,i] <- (train.white.x[,i] - means[i])/ vars[i]
  test.white.x[,i] <- (test.white.x[,i] - means[i])/ vars[i]
}

# train.white.x = as.data.frame(t((t(train.white.x) - means)/sqrt(vars)))
# test.white.x = as.data.frame(t((t(test.white.x) - means)/sqrt(vars)))

train.red.x = t((t(train.red.x) - means.r)/sqrt(vars.r))
test.red.x = t((t(test.red.x) - means.r)/sqrt(vars.r))

#--------------------------
# Multiple Linear Regression
# > lm.fit <- lm(medv ∼ lstat + age , data = Boston)
#> summary (lm.fit)
#--------------------------

lm.white <- lm(quality~., data= train.white.x)
summary(lm.white)
lm.predict = predict(lm.white, test.white.x)



#---------------------------
# Logistic regression / bzw. LDA
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
# task b) Append a) to winequality.red 
#---------------------------
#str(wine.red)
#summary(wine.red)

#dist.red.wine =table(wine.red$quality)
#dist.red.wine
#barplot(dist.red.wine, xlab= "Quality of red wine", ylab="Number of red wines",ylim = c(0,2200))


# trainIndex = sample(1:1599, 1599/4)
# train.red.wine.x = winequality.red[trainIndex,]
# train.red.wine.y = winequality.red$quality[trainIndex]
# 
# test.red.wine.x = winequality.red[-trainIndex,]
# test.red.wine.y = winequality.red$quality[-trainIndex]


# 
# lm.red <- lm(quality~., data= wine.red[-testIndex.red,])
# summary(lm.red)