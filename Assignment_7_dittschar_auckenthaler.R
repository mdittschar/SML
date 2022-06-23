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
obesitree <- rpart(formula= ObesityLevel~., data = obesity_train)
plot(obesitree)
rpart.plot(obesitree)
#-------------------------------
# b) Predict the test samples
#--------------------------------
obesity_predict = predict(tree, obesity_test, type="class")
tree_acc = mean(obesity_predict == obesity_test$ObesityLevel)
confusion_matrix = table(obesity_predict, obesity_test$ObesityLevel)
confusion_matrix
#-------------------------------
# c) Learn a Naive Bayes Classifier
#--------------------------------

bayes_obesity = naiveBayes(ObesityLevel ~., data=obesity_train)
bayes_predict = predict(bayes_obesity, obesity_test, type="class")
bayes_acc = mean(bayes_predict == obesity_test$ObesityLevel)
bayes_acc

#-------------------------------
# c) Learn a Random Forest and a Bagging Model
#--------------------------------
obesitylevel = obesity_train$ObesityLevel
#obesity_rf = randomForest(obesity_train[-obesitylevel], obesity_train$ObesityLevel)
