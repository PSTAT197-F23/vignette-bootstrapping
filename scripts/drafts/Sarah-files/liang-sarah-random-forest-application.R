## load necessary packages
library(dplyr)
#install.packages("randomForest")
library(randomForest)
# install.packages("tree")
library(gbm)
library(ISLR)
library(tree)
library(tidyverse)
library(tidymodels)
library(stringr)
library(rpart)

grade.raw <- read_csv('data/courseGrades.csv')

# attach(Carseats)
# Carseats = Carseats %>%
#   mutate(High=as.factor(ifelse(Sales <= median(Sales), "No", "Yes"))) %>%
#   select(-Sales)
head(grade.raw)

grade <- grade.raw %>% 
  filter(nLetterStudents != 0) %>% 
  select(course, instructor, quarter, year, nLetterStudents, dept, avgGPA) %>%
  mutate(course = str_replace_all(course, "\\s+", " "))

pstat <- grade %>% 
  filter(!(quarter == "summer") & dept == "PSTAT" & between(year, 2017, 2022))

pstat <- pstat %>% select(-dept)

## create binary classification problem
# binary response variable, high or low avgGPA

# pstat <- pstat %>%
#   mutate(GPAlevel = case_when('avgGPA' >= 2.5 ~ 'High',
#                              'avgGPA'< 2.5 ~ 'Low')) %>%
#   select(-'avgGPA')

head(pstat)

# Sample 80% observations as training data
set.seed(3)
partitions <- pstat %>%
  initial_split(prop = 0.8)

train <- training(partitions)

test <- testing(partitions)

dim(train)
dim(test)

# build a bagging random forest model
# bagging is random forest with m = p

bag.pstat <- randomForest(avgGPA ~ course + instructor + quarter + year + nLetterStudents,
                          data = train,
                          mtry=5, importance=TRUE, na.action = na.omit)
bag.pstat

plot(bag.pstat)
legend("top", colnames(bag.pstat$err.rate),col=1:4,cex=0.8,fill=1:4)

yhat.bag <- predict(bag.pstat, newdata = test, type = "response")
test.bag.err <- mean(yhat.bag - test$avgGPA)
test.bag.err

# prob.bag = predict(bag.pstat, newdata = test, type = "prob")
# head(prob.bag)

# all(yhat.bag == ifelse(prob.bag[, 2] > 0.5, "Yes", "No"))


bag.pstat2 <- randomForest(avgGPA ~ .,
                            data=train,
                            mtry=5, ntree=700, importance=TRUE)
yhat.bag2 <- predict(bag.pstat2, newdata = test)
test.bag.err2 <- mean(yhat.bag - test$avgGPA)
test.bag.err2



# build a random forest model
# use mtry = 3 (number of variables randomly sampled as candidates at each split)
rf.pstat = randomForest(avgGPA ~ ., data=train,
                           mtry=3, importance=TRUE)
rf.pstat

plot(rf.pstat)

# obtain predictions on testing data
yhat.rf = predict(rf.pstat, newdata = test)

# obtain error
test.rf.err = mean(yhat.rf - test$avgGPA)
test.rf.err

# compare normal random forest model error vs. 
# bootstrapped + bagged random forest error
test.bag.err
test.rf.err








