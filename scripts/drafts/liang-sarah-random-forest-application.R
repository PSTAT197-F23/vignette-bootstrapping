library(dplyr)
#install.packages("randomForest")
library(randomForest)
# install.packages("tree")
library(gbm)
library(ISLR)
library(tree)

attach(Carseats)
Carseats = Carseats %>%
  mutate(High=as.factor(ifelse(Sales <= median(Sales), "No", "Yes"))) %>%
  select(-Sales)


# Sample 75% observations as training data
set.seed(3)
train = sample(nrow(Carseats), 0.75*nrow(Carseats))
train.carseats = Carseats[train,]
# The rest as test data
test.carseats = Carseats[-train,]

# build a bootstrapping + bagging random forest model
# bagging is random forest with m = p
bag.carseats = randomForest(High ~ ., data=train.carseats,
                            mtry=10, importance=TRUE)
bag.carseats

plot(bag.carseats)
legend("top", colnames(bag.carseats$err.rate),col=1:4,cex=0.8,fill=1:4)

yhat.bag = predict(bag.carseats, newdata = test.carseats, type = "response")
test.bag.err = mean(yhat.bag != test.carseats$High)
test.bag.err

prob.bag = predict(bag.carseats, newdata = test.carseats, type = "prob")
head(prob.bag)

all(yhat.bag == ifelse(prob.bag[, 2] > 0.5, "Yes", "No"))


bag.carseats = randomForest(High ~ .,
                            data=train.carseats,
                            mtry=10, ntree=700, importance=TRUE)
yhat.bag = predict(bag.carseats, newdata = test.carseats)
test.bag.err = mean(yhat.bag != test.carseats$High)
test.bag.err



# build a random forest model for classification problem
# use mtry = 3 (number of variables randomly sampled as candidates at each split)
rf.carseats = randomForest(High ~ ., data=train.carseats,
                           mtry=3, importance=TRUE)
rf.carseats

plot(rf.carseats)

# obtain predictions on testing data
yhat.rf = predict(rf.carseats, newdata = test.carseats)

# obtain error
test.rf.err = mean(yhat.rf != test.carseats$High)
test.rf.err

# compare normal random forest model error vs. 
# bootstrapped + bagged random forest error
test.bag.err
test.rf.err

# Our random forest model achieved a test set error of 0.2. If we compare to our
# model on bootstrapped and bagged data, then we can see that the random forest 
# model does not provide any improvement over bootstrapping and bagging.








