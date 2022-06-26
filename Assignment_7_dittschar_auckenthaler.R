library(tree)
library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)

#-------------------------------
# a) Learn a decision tree. 
#--------------------------------

obesity <- read.csv("./Obesity.csv")
obesity_train = obesity[0:1500,]
obesity_test = obesity[1500:2111,]

#obesity_train$ObesityLevel = factor(obesity_train$ObesityLevel)
#obesity_test$ObesityLevel = factor(obesity_test$ObesityLevel)
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
tree_acc3 = mean(obesity_predict2 == obesity_test$ObesityLevel)
tree_acc3

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
bayes_obesity = naiveBayes(ObesityLevel ~., data=obesity_train)
bayes_obesity
bayes_predict = predict(bayes_obesity, obesity_test, type="class")
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

obesity_rf = randomForest(ObesityLevel ~ ., data= obesity_train)

summary(obesity_rf)
yhat.rf<-predict(obesity_rf, newdata= obesity_test)
test.acc.rf<- mean(yhat.rf == obesity_test$ObesityLevel)
test.acc.rf



#Bagging
set.seed(42)
bag.obesity= randomForest(ObesityLevel ~., data= obesity_train, mtry=16, importance = TRUE)

summary(bag.obesity)
yhat.bag <- predict (bag.obesity , newdata = obesity_test)
test.acc.bag<-mean(yhat.bag == obesity_test$ObesityLevel)
test.acc.bag

#-------------------------------
# e)  mtry-parameter of the random forest 
#     by five-fold cross validation.
#--------------------------------


mean_accuracies = c()
standard_errors = c()
for (j in 1:16){
  accuracies = c()
  m = j
  for (i in 1:5){
    indices = ((i-1)*300+1):(i*300)
    fold = obesity_train[indices,]
    train_fold = obesity_train[-indices,]
    set.seed(42)
    fold_rf = randomForest(ObesityLevel ~ ., mtry=m, data= train_fold)
    yhat.fold <- predict(fold_rf , newdata = fold)
    fold.acc = mean(yhat.fold == obesity_train$ObesityLevel[indices])
    accuracies = append(accuracies, fold.acc)
  }
  mean_accuracies = append(mean_accuracies, mean(accuracies))
  standard_errors = append(standard_errors, sqrt(sum((accuracies - mean(accuracies))^2)/length(accuracies)))
}
plot(mean_accuracies,col=ifelse(mean_accuracies == c(max(mean_accuracies)), 'red', 'black'),pch = 19,xlab="mtry")
arrows(x0=1:16, y0=mean_accuracies - standard_errors, x1=1:16, y1=mean_accuracies + standard_errors, code=3, lwd=2)
axis(1, at=seq(1,16, by=1))
which.max(mean_accuracies)
mean_accuracies[which.max(mean_accuracies)]

#-------------------------------
# f)  random forest with the selected mtry ˆ f
#--------------------------------
set.seed(42)
final_forest = randomForest(ObesityLevel ~ ., mtry=9, data= obesity_train, importance=TRUE)
rf_pred_final = predict(final_forest, obesity_test)
final.acc = mean(rf_pred_final == obesity_test$ObesityLevel)
final.acc
confusion_matrix_rf = table(rf_pred_final, obesity_test$ObesityLevel)
confusion_matrix_rf
varImpPlot(final_forest,pch = 19)
# oob.error = sum(rf_pred_final != obesity_test$ObesityLevel)/length(rf_pred_final)
# oob.error
final_forest$importance
final_forest$confusion
#-------------------------------
# g)  mtry according to the out-of-bag (OOB) error and compare 
#     to the CV-based tuning
#--------------------------------
oob.mean = c()
for (k in 1:16){
  oob.error = c()
  m = k
  set.seed(42)
  final_forest = randomForest(ObesityLevel ~ ., mtry=k, data= obesity_train)
  rf_pred_final = predict(final_forest, obesity_test)
  oob.error = sum(rf_pred_final != obesity_test$ObesityLevel)/length(rf_pred_final)
  oob.mean = append(oob.mean, oob.error)
}
plot(oob.mean,col=ifelse(oob.mean == c(min(oob.mean)), 'red', 'black'),pch = 19,xlab="mtry")
axis(1, at=seq(1,16, by=1))
which.min(oob.mean)
plot(final_forest)
oob.mean
 
plot(oob.mean, col="orange", ylim=c(0,1), xlab="mtry", ylab="error")
axis(1, at=seq(1,16, by=1))
legend(x="topright" , legend=c("OOB", "CV"), fill=c("orange", "blue"))
title("Cross-validation vs. OOB tuning")
par(new=TRUE)
points(1- mean_accuracies, col="blue")
final.acc
1- oob.mean[which.min(oob.mean)]
