library(tree)
library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)
library (gbm)

#-------------------------------
# a) Learn a decision tree. 
#--------------------------------
obesity <- read.csv("./Obesity.csv")
obesity_train = obesity[0:1500,]
obesity_test = obesity[1500:2111,]
set.seed(42)
obesitree <- rpart(formula= ObesityLevel~., data = obesity_train)
plot(obesitree)
rpart.plot(obesitree)
#-------------------------------
# b) Predict the test samples
#--------------------------------
set.seed(42)
obesity_predict = predict(obesitree, obesity_test, type="class")
tree_acc = mean(obesity_predict == obesity_test$ObesityLevel)
confusion_matrix = table(obesity_predict, obesity_test$ObesityLevel)
confusion_matrix
#-------------------------------
# c) Learn a Naive Bayes Classifier
#--------------------------------
set.seed(42)
bayes_obesity = naiveBayes(ObesityLevel ~., data=obesity_train)
bayes_predict = predict(bayes_obesity, obesity_test, type="class")
bayes_acc = mean(bayes_predict == obesity_test$ObesityLevel)
bayes_acc

#-------------------------------
# d) Learn a Random Forest and a Bagging Models
#--------------------------------
#Random Forest
set.seed(42)
obesity_train$ObesityLevel = factor(obesity_train$ObesityLevel)
obesity_test$ObesityLevel = factor(obesity_test$ObesityLevel)
obesity_rf = randomForest(ObesityLevel ~ ., data= obesity_train)
yhat.rf<-predict(obesity_rf, newdata= obesity_test)
test.MSE.rf<- mean(yhat.rf == obesity_test$ObesityLevel)
test.MSE.rf



#Bagging
set.seed(42)
bag.obesity= randomForest(ObesityLevel ~., data= obesity_train,mtry = 12, importance = TRUE)
yhat.bag <- predict (bag.obesity , newdata = obesity_test)
test.MSE.bag<-mean(yhat.bag == obesity_test$ObesityLevel)
test.MSE.bag

#-------------------------------
# e)  mtry-parameter of the random forest by five-fold cross validation.
#--------------------------------


#-------------------------------
# f)  random forest with the selected mtry ˆ f
#--------------------------------

#-------------------------------
# g)  mtry according to the out-of-bag (OOB) error and compare to the CV-based tuning
#--------------------------------
