library(tree)
library(rpart) # not needed for assignment, just additional illustration
library(rpart.plot) #not needed for assignment, just illustration
library(e1071)
library(randomForest)

#-------------------------------
# a) Learn a decision tree. 
#--------------------------------

obesity <- read.csv("./Obesity.csv")
obesity_train = obesity[0:1500,]
obesity_test = obesity[1500:2111,]

set.seed(42)
obesity_train[sapply(obesity_train, is.character)] <- lapply(obesity_train[sapply(obesity_train, is.character)], as.factor)
set.seed(42)
obesity_test[sapply(obesity_test, is.character)] <- lapply(obesity_test[sapply(obesity_test, is.character)], as.factor)

set.seed(42)
obesitytree2<-tree(ObesityLevel~.,data= obesity_train)
plot(obesitytree2)
text(obesitytree2)
summary(obesitytree2)



#Not needed but interesting with decision tree with the library (rpart und rpart.plot)
obesitree <- rpart(formula= ObesityLevel~., data = obesity_train)
rpart.plot(obesitree, box.palette=0)


#-------------------------------
# b) Predict the test samples

set.seed(42)
obesity_predict2 = predict(obesitytree2, obesity_test, type="class")
# accuracy of the fitted tree model on the test set
tree_acc3 = mean(obesity_predict2 == obesity_test$ObesityLevel)
tree_acc3

# confusion matrix on the test set
confusion_matrix = table(obesity_predict2, obesity_test$ObesityLevel)
confusion_matrix

#set.seed(42)
#obesity_predict = predict(obesitree, obesity_test, type="class")
#tree_acc = mean(obesity_predict == obesity_test$ObesityLevel)
#tree_acc
#confusion_matrix = table(obesity_predict, obesity_test$ObesityLevel)
#confusion_matrix

#-------------------------------
# c) Learn a Naive Bayes Classifier
#--------------------------------
set.seed(42)
# learn naive bayes classifier on th train set
bayes_obesity = naiveBayes(ObesityLevel ~., data=obesity_train)
bayes_obesity
# predict on test set
bayes_predict = predict(bayes_obesity, obesity_test, type="class")
# test accuracy on the test set
bayes_acc = mean(bayes_predict == obesity_test$ObesityLevel)
bayes_acc

#-------------------------------
# d) Learn a Random Forest and a Bagging Models
#--------------------------------
#Random Forest
set.seed(42)
obesity_train[sapply(obesity_test, is.factor)] <- lapply(obesity_train[sapply(obesity_train, is.factor)], as.character)
obesity_test[sapply(obesity_test, is.factor)] <- lapply(obesity_test[sapply(obesity_test, is.factor)], as.character)
obesity_train$ObesityLevel = factor(obesity_train$ObesityLevel)
obesity_test$ObesityLevel = factor(obesity_test$ObesityLevel)
# train model on full training data set
obesity_rf = randomForest(ObesityLevel ~ ., data= obesity_train)

summary(obesity_rf)
# predict on test set
yhat.rf<-predict(obesity_rf, newdata= obesity_test)
# test accuracy for random forest
test.acc.rf<- mean(yhat.rf == obesity_test$ObesityLevel)
test.acc.rf



#Bagging
set.seed(42)
# mtry is set to 16 because all parameters are evaluted for splitting
bag.obesity= randomForest(ObesityLevel ~., data= obesity_train, mtry=16, importance = TRUE)

summary(bag.obesity)
yhat.bag <- predict (bag.obesity , newdata = obesity_test)
# test accuracy 
test.acc.bag<-mean(yhat.bag == obesity_test$ObesityLevel)
test.acc.bag

#-------------------------------
# e)  mtry-parameter of the random forest 
#     by five-fold cross validation.
#--------------------------------

# cross-validation
mean_accuracies = c()
standard_errors = c()
for (j in 1:16){
  accuracies = c()
  m = j
  for (i in 1:5){
    # divide in steps of 300
    indices = ((i-1)*300+1):(i*300)
    fold = obesity_train[indices,]
    train_fold = obesity_train[-indices,]
    set.seed(42)
    # train on data excluding fold
    fold_rf = randomForest(ObesityLevel ~ ., mtry=m, data= train_fold)
    # predict on fold data
    yhat.fold <- predict(fold_rf , newdata = fold)
    fold.acc = mean(yhat.fold == obesity_train$ObesityLevel[indices])
    accuracies = append(accuracies, fold.acc)
  }
  # obtain accuracies and standard errors
  mean_accuracies = append(mean_accuracies, mean(accuracies))
  standard_errors = append(standard_errors, sqrt(sum((accuracies - mean(accuracies))^2)/length(accuracies)))
}

plot(mean_accuracies,col=ifelse(mean_accuracies == c(max(mean_accuracies)), 'red', 'black'),pch = 19,xlab="mtry", ylim=c(0,1))
arrows(x0=1:16, y0=mean_accuracies - standard_errors, x1=1:16, y1=mean_accuracies + standard_errors, code=3, lwd=1, angle=90)
title("Mean accuracies dependent on mtry")
axis(1, at=seq(1,16, by=1))
which.max(mean_accuracies)
mean_accuracies[which.max(mean_accuracies)]

#-------------------------------
# f)  random forest with the selected mtry ˆ f
#--------------------------------
set.seed(42)
# train model on best mtry
final_forest = randomForest(ObesityLevel ~ ., mtry=9, data= obesity_train, importance=TRUE)
rf_pred_final = predict(final_forest, obesity_test)
# predict optimal random forest accuracy
final.acc = mean(rf_pred_final == obesity_test$ObesityLevel)
final.acc
# confusion matrix for test set
confusion_matrix_rf = table(rf_pred_final, obesity_test$ObesityLevel)
confusion_matrix_rf
# mean decrease Gini plot
varImpPlot(final_forest, pch = 19)
final_forest$importance[,"MeanDecreaseGini"]
# confusion matrix for training set
final_forest$confusion
#-------------------------------
# g)  mtry according to the out-of-bag (OOB) error and compare 
#     to the CV-based tuning
#--------------------------------
oob.mean = c()
# tune model for each possible mtry
for (k in 1:16){
  oob.error = c()
  m = k
  set.seed(42)
  # train on full training set
  final_forest = randomForest(ObesityLevel ~ ., mtry=k, data= obesity_train)
  # predict on full test set
  rf_pred_final = predict(final_forest, obesity_test)
  oob.error = sum(rf_pred_final != obesity_test$ObesityLevel)/length(rf_pred_final)
  oob.mean = append(oob.mean, oob.error)
}
plot(oob.mean,col=ifelse(oob.mean == c(min(oob.mean)), 'red', 'black'),pch = 19,xlab="mtry", ylab="OOB error")
axis(1, at=seq(1,16, by=1))
title("OOB error dependent on mtry")
# mtry value of lowest oob-error
which.min(oob.mean)

# lowest oob error
oob.mean[which.min(oob.mean)]
plot(oob.mean, col="orange", ylim=c(0,1), xlab="mtry", ylab="error")
axis(1, at=seq(1,16, by=1))
legend(x="topright" , legend=c("OOB", "CV"), fill=c("orange", "blue"))
title("Cross-validation vs. OOB tuning")
par(new=TRUE)
points(1- mean_accuracies, col="blue")
# test accuracy for cv-tuned model
final.acc
# test accuracy for oob-tuned model
1- oob.mean[which.min(oob.mean)]
