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
test.fit$g <- as.numeric(train.fit$g)
#fit logistic regression model
glm.fits <- glm (g~. , data= train.fit, family = binomial)
#summary(glm.fits)
#-------- 
# NOT CORRECT!!!!
#------
glm.probs <- predict (glm.fits, test.fit, type="response")
glm.pred <- rep ("aa", 439)
glm.pred[glm.probs > .5] <- "ao"
#------------------
# Task 2c)  LDA
#-------------------
library (MASS)
lda.fit <- lda (g ~., data= train.fit)
lda.pred <- predict (lda.fit , test.fit)
names (lda.pred)
lda.class <- lda.pred$class
# print number of values above trashhold
sum (lda.pred$posterior[, 1] >= .5)
#MISSING COMPARE VALUE COUNTS!!

#------------------
# Task 2d)  Generate confusion matrices
#-------------------

#------------------
# Task 2e)  plot ROC and PR curves
#-------------------
library(PRROC)

#------------------
# Task 2f) LDA on full training data set
#------------------


