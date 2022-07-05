library (MASS)
library (PRROC)
library (boot)
library (leaps)
library (glmnet)
library (randomForest)
library (tree)
library (e1071)

#load data sets
frog.train = read.csv("../AnuranCalls_train.csv",sep = ",")
frog.test  =  read.csv("../AnuranCalls_test.csv",sep = ",")

#----------------
# inspection of dataset
#----------------

summary (frog.train)

dist.frogfam <-table(frog.train$Family)
dist.frogfam
barplot(dist.frogfam, xlab= "Family of frog", ylab="Number of observation")

#-----------------
# Split in training and test set 
#-----------------
set.seed (1)
testIndex = sample(1:(nrow(frog.train)), (nrow(frog.train)*.3))
train = frog.train[-testIndex, 1: (length(frog.train))]
test  = frog.train[testIndex,1:(length(frog.train))]

#Normalization of the data 

means = colMeans(train[1:(length(train)-1)])
vars = apply(train[1:(length(train)-1)], 2, var)

for(i in 1:11 ){
  train[,i] = (train[,i] - means[i])/ sqrt(vars[i])
  test[,i] = (test[,i] - means[i])/ sqrt(vars[i])
}

#----------------
# logistic regression
#----------------
train$Family = factor(train[,23])
test$Family = factor(test[,23])
set.seed (1)
glm.model= glm(Family~.,data= train, family = "binomial")
glm.probs = predict (glm.model, test, type="response")
glm.probs[which(glm.probs > .5)] = 1
glm.probs[which(glm.probs <= .5)] = 0
glm.probs[which(glm.probs == 0)]= "Hylidae"
glm.probs[which(glm.probs == 1)]= "Leptodactylidae"
#confusion matrix
tab.glm= table(glm.probs,test$Family)
tab.glm
#accuracy
acc.glm= mean(glm.probs== test$Family)
acc.glm
#summary(glm.model)

#-----------------
# Tree
#------------------
set.seed (1)
tree = tree(Family~.,data= train)
plot(tree)
tree.predict  = predict(tree, test, type="class")
table(tree.predict)
#accuracy
acc.tree= mean(tree.predict == test$Family)
acc.tree
summary(tree)
#confusion matrix
tab.tree = table(tree.predict, test$Family)
tab.tree


#-------------------------------
# c) Learn a Naive Bayes Classifier
#--------------------------------
set.seed(1)
# learn naive bayes classifier on th train set
bayes = naiveBayes(Family ~., data=train)
# predict on test set
bayes.predict = predict(bayes, test, type="class")
#confusion matrix
tab.bayes= table(bayes.predict, test$Family)
tab.bayes
# test accuracy on the test set
acc.bayes = mean(bayes.predict == test$Family)
acc.bayes

#------------------
# Random forest
#------------------
set.seed(1)
rf = randomForest(Family ~ ., data= train)

summary(rf)
# predict on test set
yhat.rf=predict(rf, newdata= test)
#confusion matrix
tab.rf= table(yhat.rf, test$Family)
tab.rf
# test accuracy for random forest
acc.rf= mean(yhat.rf == test$Family)
acc.rf


# Bagging
set.seed(1)
# mtry is set to 22 because all parameters are evaluted for splitting
bag= randomForest(Family ~., data= train, mtry=(length(train)-1), importance = TRUE)
summary(bag)
yhat.bag <- predict (bag , newdata = test)
#confusion matrix
tab.bag= table(yhat.bag, test$Family)
tab.bag
# test accuracy 
acc.bag<-mean(yhat.bag == test$Family)
acc.bag

# tune model for each possible mtry
oob.mean = c()
for (k in 1:(length(train)-1)){
  oob.error = c()
  m = k
  set.seed(1)
  # train on full training set
  forest = randomForest(Family ~ ., mtry=k, data= train)
  # predict on full test set
  rf.pred.final = predict(forest, test)
  oob.error = sum(rf.pred.final != test$Family)/length(rf.pred.final)
  oob.mean = append(oob.mean, oob.error)
}
plot(oob.mean,col=ifelse(oob.mean == c(min(oob.mean)), 'red', 'black'),pch = 19,xlab="mtry", ylab="OOB error")
axis(1, at=seq(1,22, by=1))
title("OOB error dependent on mtry")
# mtry value of lowest oob-error
best.mtry= which.min(oob.mean)

set.seed(1)
rf.best = randomForest(Family ~ ., mtry=best.mtry, data= train)
rf.pred.best = predict(rf.best, test)
#confusion matrix
tab.rf.best= table(rf.pred.best , test$Family)
tab.rf.best
#accuracy
acc.rf.best= mean(rf.pred.best == test$Family)
acc.rf.best

#set.seed(1)
#forest.10 = randomForest(Family ~ ., mtry=10, data= train)
#rf.pred.10 = predict(forest.10, test)
#table(rf.pred.10 , test$Family)
#acc.rf.10= mean(rf.pred.10 == test$Family)
#acc.rf.10  

#set.seed(1)
#forest.11 = randomForest(Family ~ ., mtry=11, data= train)
#rf.pred.11 = predict(forest.11, test)
#table(rf.pred.11 , test$Family)
#acc.rf.11= mean(rf.pred.11 == test$Family)
#acc.rf.11 

#set.seed(1)
#forest.12 = randomForest(Family ~ ., mtry=12, data= train)
#rf.pred.12 = predict(forest.12, test)
#table(rf.pred.12 , test$Family)
#acc.rf.12= mean(rf.pred.12 == test$Family)
#acc.rf.12  

#set.seed(1)
#forest.15 = randomForest(Family ~ ., mtry=15, data= train)
#rf.pred.15 = predict(forest.15, test)
#table(rf.pred.15 , test$Family)
#acc.rf.15= mean(rf.pred.15 == test$Family)
#acc.rf.15  

#------------------
# Support vector machine
#------------------
#linear kernel
set.seed (1)
svmfit.l=svm ( Family ~ ., data = train, kernel = "linear",gamma = 1, cost = 1)
#confusion matrix
tab.svm.l= table (true = test$Family, pred = predict (svmfit.l,newdata = test))
tab.svm.l
#accuracy
acc.svm.l= mean((predict(svmfit.l,newdata = test))== test$Family)
acc.svm.l

#radial kernel
set.seed(1)
tune.out.r = tune (svm , Family ~ ., data = train,kernel = "radial",ranges = list (cost = c(0.1, 1, 10, 100, 1000),gamma = c(0.5, 1, 2, 3, 4)))
#confusion matrix
tab.smv.r=table (true = test$Family, pred = predict (tune.out.r$best.model,newdata = test))
tab.smv.r
#accuracy
acc.svm.r= mean((predict(tune.out.r$best.model,newdata = test))== test$Family)
acc.svm.r

#polynomial kernel with degree 3 
set.seed(1)
tune.out.p= tune (svm , Family ~ ., data = train,kernel = "polynomial",ranges = list (cost = c(0.1, 1, 10, 100, 1000),gamma = c(0.5, 1, 2, 3, 4)))
#confusion matrix
tab.smv.p= table (true = test$Family, pred = predict (tune.out.p$best.model,newdata = test))
tab.smv.p
#accuracy
acc.svm.p= mean((predict(tune.out.p$best.model,newdata = test))== test$Family)
acc.svm.p

#------------------------------------
#b) best model on test data
#------------------------------------

pred.best = predict(rf.best, frog.test)
write.table(pred.best, file = "dittschar_auckenthaler_test_response.txt", sep = "\t",
            row.names = FALSE, col.names = NA)
