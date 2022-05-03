# assignment 1
library(FNN)

mse=function(arr, arr_pred){
  weight = 1/nrow(arr)
  error = (arr - arr_pred)^2
  sum_err = sum(error)
  mse = weight*sum_err
  print(mse)
    }

load("ozone.RData")
summary(trainset)
range(trainset)
colnames(ozone)
length(trainset)
summary(testset)
length(testset)
summary(ozone)
pairs(ozone)
cor(ozone, ozone)
var(ozone)

linear_model = lm(ozone~radiation+temperature+wind, ozone[testset,])
ozone_predict = predict(linear_model, ozone[testset,])
mse(ozone[testset,]["ozone"],ozone_predict)
cor(ozone["ozone"][testset,],ozone_predict)
x_axis = ozone["ozone"][testset,]
plot(x_axis,ozone_predict, xlab="real ozone values", ylab="predicted ozone values")
ozone_train = ozone
ozone_train$ozone = NULL
mses = c()
mses_train = c()
for (val in seq(1,30)){
  
  knn_val = knn.reg(train=ozone_train[trainset,], test = ozone_train[testset,],
                    y=ozone["ozone"][trainset,], k=val)
  knn_val_train = knn.reg(train=ozone_train[trainset,], test = ozone_train[trainset,],
                    y=ozone["ozone"][trainset,], k=val)
  mses = c(mses, mse(ozone[testset,]["ozone"],knn_val$pred))
  mses_train = c(mses_train, mse(ozone[trainset,]["ozone"],knn_val_train$pred))
}

plot(seq(1,30), mses, xlab="k (k-nearest-neighbors)", ylab="MSE test set")
plot(seq(1,30), mses_train, xlab="k (k-nearest-neighbors)", ylab="MSE train set")
