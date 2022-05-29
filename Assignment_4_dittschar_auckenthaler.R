#load data 
data <- read.csv("phoneme.csv")

#------------------
# Task 2.a)
#-------------------
#create df for test data
test.data <- data[grep('test', data$speaker),]
#return number of oversations in train data
nrow(test.data)
train.data <-data[grep('train', data$speaker),]
#return number of observations in train data
nrow(train.data)

#subset g = aa & ao
subset.aa <- data[grep ('aa', data$g),]
subset.ao <- data[grep ('ao', data$g),]
subset.aa.ao <- rbind(subset.aa,subset.ao)
subset.train <-subset.aa.ao[grep('train', subset.aa.ao$speaker),]
nrow(subset.train)
subset.test <-subset.aa.ao[grep('test', subset.aa.ao$speaker),]
nrow(subset.test)

#------------------
# Task 2.b)
#------------------
subset.train["g"][subset.train["g"] == "aa"] <- 0
subset.train["g"][subset.train["g"] == "ao"] <- 1

subset.test["g"][subset.test["g"] == "aa"] <- 0
subset.test["g"][subset.test["g"] == "ao"] <- 1
#delete row.name, speaker
train.fit <- subset(subset.train, select = -c(row.names,speaker))
test.fit <-subset(subset.test, select = -c(row.names,speaker))

#transform values to numeric values
train.fit$g <- as.numeric(train.fit$g)
test.fit$g <- as.numeric(test.fit$g)
#fit logistic regression model
glm.fits <- glm (g~. , data= train.fit, family = binomial)
#summary(glm.fits)

glm.probs <- predict (glm.fits, test.fit, type="response")
glm.pred <- rep ("0", 439)
glm.pred[glm.probs > .5] <- "1"
table(glm.pred)

#calculate train accuracy 
glm.probs.train <- predict (glm.fits, train.fit, type="response")
glm.pred.train <- rep ("0", 1278)
glm.pred.train[glm.probs.train > .5] <- "1"
mean (glm.pred.train == train.fit$g)

#calculate test accuracy
acc.glm= mean (glm.pred == test.fit$g)
print(acc.glm)
#------------------
# Task 2c)  LDA
#-------------------
library (MASS)
lda.fit <- lda (g ~., data= train.fit)
lda.pred <- predict (lda.fit , test.fit)
names (lda.pred)
lda.class <- lda.pred$class
# print number of values above treshold
sum (lda.pred$posterior[, 2] >= .5)
sum (lda.pred$posterior[, 1] >= .5)
table(lda.class)
#calculate train accuracy
lda.pred.train <- predict (lda.fit , train.fit)
names (lda.predtrain)
lda.class.train <- lda.pred.train$class
mean (lda.class.train == train.fit$g)
#calculate test accuracy
acc.lda= mean (lda.class == test.fit$g)
print(acc.lda)

#comparison with 
table(test.fit$g)

#------------------
# Task 2d)  Generate confusion matrices
#-------------------
table(glm.pred , lda.class)
table(glm.pred , test.fit$g)
table(lda.class , test.fit$g)

#------------------
# Task 2e)  plot ROC and PR curves
#-------------------
library(PRROC)
plot(roc.curve(scores.class0=lda.pred$posterior[,2], weights.class0=test.fit$g, 
               curve=TRUE))
plot(pr.curve(scores.class0=lda.pred$posterior[,2], weights.class0=test.fit$g, 
               curve=TRUE))
plot(roc.curve(scores.class0=glm.probs, weights.class0=test.fit$g, 
               curve=TRUE))
plot(pr.curve(scores.class0=glm.probs, weights.class0=test.fit$g, 
               curve=TRUE))

#------------------
# Task 2f) LDA on full training data set
#------------------
# get the train data without row names and speaker
all_train.fit <- subset(train.data, select = -c(row.names,speaker))
all_test.fit <- subset(test.data, select = -c(row.names,speaker))
# fit lda on train data
all_lda.fit <- lda (g ~., data= all_train.fit)
# get predictions for training and test data
all_train_pred <- predict(all_lda.fit, train.data, type="response")
all_test_pred <- predict(all_lda.fit, test.data, type="response")
# get train and test accuracy
mean(all_train_pred$class == train.data$g)
mean(all_test_pred$class == test.data$g)
# get confusion matrices
table(all_train_pred$class, train.data$g)
table(all_test_pred$class, test.data$g)
