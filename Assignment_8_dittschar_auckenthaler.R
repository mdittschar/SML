
wines = read.csv("winequality-white.csv", sep=";")
summary(wines)

trainIndex = sample(1:4898, 4000)
train.x = wines[trainIndex,]
train.y = wines$quality[trainIndex]

test.x = wines[-trainIndex,]
test.y = wines$quality[-trainIndex]
