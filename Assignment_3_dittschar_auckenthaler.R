library('ISLR')
data("Auto")
#Task 2a plot of all columns exept name
pairs(~ mpg + cylinders + displacement + horsepower+ weight+ acceleration+ year+ origin, data = Auto)

#Task 2b Correlation matrix
cor(Auto[ ,-9])

#Task 2c Simple linear regression cylinders, displacement, horsepower and year 
model.mpg.cylinders <- lm(mpg~cylinders, data= Auto)
summary(model.mpg.cylinders)

model.mpg.displacement <- lm(mpg~displacement, data= Auto)
summary(model.mpg.displacement)

model.mpg.horsepower <- lm(mpg~horsepower, data= Auto)
summary(model.mpg.horsepower)

model.mpg.year <- lm(mpg~year, data= Auto)
summary(model.mpg.year)

#Task 2d multiple linear regression
model.MLR <- lm(formula = mpg ~ . - name, data = Auto)
summary(model.MLR)


#Task 2e

#studentized residual plots
par(mfrow=c(1,1))
plot ( predict (model.MLR), residuals (model.MLR))

#plot ( hatvalues (model.MLR))
#which.max(hatvalues (model.MLR))

#residual plots of multi regression model 
par(mfrow=c(2,2))
plot(lm(formula = mpg ~ . - name,data=Auto))

#Task 2f
#model 1
model.2f1 = lm(formula = mpg ~ cylinders*year + cylinders*weight+ weight*year+ log(displacement), data = Auto)
summary(model.2f1)

#model 2
model.2f2 = lm(formula = mpg ~ cylinders*year + cylinders*weight+ weight*year+ + I(displacement^2), data = Auto)
summary(model.2f2)

#model 3
model.2f3 = lm(formula = mpg ~ cylinders*year + cylinders*weight+ weight*year+ sqrt(displacement), data = Auto)
summary(model.2f3)
#plot model 1 2f
par(mfrow=c(2,2))
plot(model.2f1)

#plot model 2 2f
par(mfrow=c(2,2))
plot(model.2f2)

#plot model 3 2f
par(mfrow=c(2,2))
plot(model.2f3)

