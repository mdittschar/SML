library (MASS)
library (PRROC)
library (boot)
library (leaps)
library (glmnet)
library (randomForest)
library (tree)
library (e1071)
library (gbm)

#load data sets
frog.train = read.csv("../AnuranCalls_train.csv",sep = ",")
frog.test  =  read.csv("../AnuranCalls_test.csv",sep = ",")

#MCC function 
MCC=function(confusion.matrix){
  TP= confusion.matrix[1,1]
  TN= confusion.matrix[2,2]
  FP= confusion.matrix[1,2]
  FN= confusion.matrix[2,1]
  mcc= (TP*TN-FP*FN)/(sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
  print(mcc)
}

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

for(i in 
    1:11 ){
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
#MCC
mcc.glm= MCC(tab.glm)
#accuracy
acc.glm= mean(glm.probs== test$Family)
acc.glm
summary(glm.model)


#glm model without not significant variables
set.seed(1)
glm.model.2= glm(Family~.- MFCC_7- MFCC_22- MFCC_6 -MFCC_9-MFCC_1-MFCC_2 ,data= train, family = "binomial")
glm.probs.2 = predict (glm.model.2, test, type="response")
glm.probs.2[which(glm.probs.2 > .5)] = 1
glm.probs.2[which(glm.probs.2 <= .5)] = 0
glm.probs.2[which(glm.probs.2 == 0)]= "Hylidae"
glm.probs.2[which(glm.probs.2 == 1)]= "Leptodactylidae"
tab.glm.2= table(glm.probs.2,test$Family)
tab.glm.2
mcc.glm.2= MCC(tab.glm.2)
acc.glm.2= mean(glm.probs.2== test$Family)
acc.glm.2
#-----------------
# Tree
#------------------
set.seed (1)
tree = tree(Family~.,data= train)
plot(tree)
text(tree)
tree.predict  = predict(tree, test, type="class")
#accuracy
acc.tree= mean(tree.predict == test$Family)
acc.tree
summary(tree)
#confusion matrix
tab.tree = table(tree.predict, test$Family)
tab.tree
#MCC
mcc.tree= MCC(tab.tree)


#-------------------------------
# Naive Bayes Classifier
#--------------------------------
set.seed(1)
# learn naive bayes classifier on th train set
bayes = naiveBayes(Family ~., data=train)
# predict on test set
bayes.predict = predict(bayes, test, type="class")
#confusion matrix
tab.bayes= table(bayes.predict, test$Family)
tab.bayes
#MCC
mcc.bayes= MCC(tab.bayes)
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
#MCC
mcc.rf= MCC(tab.rf)
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
#MCC
mcc.bag= MCC(tab.bag)
# test accuracy 
acc.bag<-mean(yhat.bag == test$Family)
acc.bag


# tune model for each possible mtry
mcc.rf.final= c()
ntree= c()
mtry= c()
for (k in 1:(length(train)-1)){
  for (i in c(200,500,1000)){
    set.seed(1)
    # train on full training set
    forest = randomForest(Family ~ ., mtry=k, ntree=i, data= train)
    forest.pred = predict(forest, test)
    tab= table(forest.pred , test$Family)
    print(paste("Mtry:",k,"ntree:",i))
    mcc.best= MCC(tab)
    mcc.rf.final= append(mcc.rf.final,mcc.best)
    mtry= append(mtry,k)
    ntree= append(ntree,i)
  }  
}

best= which.max(mcc.rf.final)

set.seed(1)
rf.best= randomForest(Family ~ ., mtry=mtry[best], ntree= ntree[best],data= train)
rf.pred.best= predict(rf.best, test)
tab.rf.best= table(rf.pred.best , test$Family)
tab.rf.best
mcc.rf.best= MCC(tab.rf.best)
#accuracy
acc.rf.best= mean(rf.pred.best == test$Family)
acc.rf.best


#------------------
# Support vector machine
#------------------
#linear kernel
set.seed (1)
svmfit.l=svm ( Family ~ ., data = train, kernel = "linear",gamma = 1, cost = 1)
#confusion matrix
tab.svm.l= table (true = test$Family, pred = predict (svmfit.l,newdata = test))
tab.svm.l
#MCC
mcc.svm.l= MCC(tab.svm.l)
#accuracy
acc.svm.l= mean((predict(svmfit.l,newdata = test))== test$Family)
acc.svm.l


#radial kernel
#set.seed(1)
#tune.out.r = tune (svm , Family ~ ., data = train,kernel = "radial",ranges = list (cost = c(0.1, 1, 10, 100, 1000),gamma = c(0.5, 1, 2, 3, 4)))
#confusion matrix
#tab.smv.r=table (true = test$Family, pred = predict (tune.out.r$best.model,newdata = test))
#tab.smv.r
#MCC
#mcc.svm.r= MCC(tab.smv.r)
#accuracy
#acc.svm.r= mean((predict(tune.out.r$best.model,newdata = test))== test$Family)
#acc.svm.r

#polynomial kernel with degree 3 
set.seed(1)
tune.out.p= tune (svm , Family ~ ., data = train,kernel = "polynomial",ranges = list (cost = c(0.1, 1, 10, 100, 1000),gamma = c(0.5, 1, 2, 3, 4)))
#confusion matrix
tab.svm.p= table (true = test$Family, pred = predict (tune.out.p$best.model,newdata = test))
tab.svm.p
#MCC
mcc.svm.p= MCC(tab.svm.p)
#accuracy
acc.svm.p= mean((predict(tune.out.p$best.model,newdata = test))== test$Family)
acc.svm.p

acc.rf.all= mean((predict(rf.best, frog.train))==frog.train$Family)

#train model on whole trainingsset on random forest model

#random forest with tuned mtry
acc.rf.all= mean((predict(tune.out.p$best.model,newdata = frog.train))== frog.train$Family)
acc.rf.all

best.all= which.max(mcc.rf.final)

set.seed(1)
rf.best.all= randomForest(factor(Family) ~ ., mtry=7, ntree=200, data= frog.train)
rf.pred.best.all= predict(rf.best.all, frog.train)
tab.rf.best.all= table(rf.pred.best.all , factor(frog.train$Family))


#------------------------------------
#b) best model on test data
#------------------------------------

pred.best = predict(rf.best, frog.test)

pred.best.all = predict(rf.best.all, frog.test)
final= table(pred.best.all)
final

#acc = mean(pred.best == pred.best.all)

write.table(pred.best.all, file = "dittschar_auckenthaler_test_response.txt", sep = "\t", col.names = FALSE, row.names = FALSE, quote= FALSE)

dist.frogfam.test <-table(pred.best.all)
barplot(dist.frogfam.test, xlab= "Family of frog", ylab="Number of observation predicted test set")

  
