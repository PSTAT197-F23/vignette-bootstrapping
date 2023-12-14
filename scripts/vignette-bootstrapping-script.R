# final script for vignette code
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


library(imager)
bootstrap_diagram <- load.image("/Users/sharanya/Documents/GitHub/vignette-bootstrapping/images/bootstrap-diagram.png")
plot(bootstrap_diagram, axes=FALSE)

# store each bootstrap iteration in a list
sample_list <- list()

# n=1000 bootstrap samples
for(i in 1:10000){
  sample_list[[i]] <- sample(c(1:10000), size = 10000, replace = TRUE)
}

# head(sample_list)

# select a random number to test ratio of missing observations
random_num <- sample(c(1:10000), size = 1, replace = TRUE)

# calculating out-of-bag probability 
count <- 0
for(i in 1:10000){
  if(random_num %in% unique(sample_list[[i]])){
    count = count + 1
  }
}
probability <- 1-(count/10000.0)

probability


# load course grades data set
grade.raw <- read_csv('data/courseGrades.csv')
head(grade.raw)

# preprocess
grade <- grade.raw %>% 
  filter(nLetterStudents != 0) %>% 
  select(course, instructor, quarter, year, nLetterStudents, dept, avgGPA) %>%
  mutate(course = str_replace_all(course, "\\s+", " "))

pstat <- grade %>% 
  filter(!(quarter == "summer") & dept == "PSTAT" & between(year, 2017, 2022))

pstat <- pstat %>% select(-dept)
head(pstat)



# sample 80% observations as training data
set.seed(3)

partitions <- pstat %>%
  initial_split(prop = 0.8)

train <- training(partitions)

test <- testing(partitions)

dim(train)
dim(test)



# build a bagging random forest model
# bagging is random forest with m = p

bag.pstat <- randomForest(avgGPA ~ course + instructor + quarter + year +
                            nLetterStudents,
                          data = train,
                          ntree = 1000,
                          mtry=5, importance=TRUE, na.action = na.omit)
bag.pstat


# plot the errors
plot(bag.pstat)


# predict on testing split based on exact class
yhat.bag <- predict(bag.pstat, newdata = test, type = "response")

# retrieve test set MSE
test.bag.err <- mean((test$avgGPA- yhat.bag)^2)
test.bag.err


# increase ntree
bag.pstat2 <- randomForest(avgGPA ~ .,
                           data=train,
                           mtry=5, ntree=10000, importance=TRUE)

# retrieve test set MSE
yhat.bag2 <- predict(bag.pstat2, newdata = test)
test.bag.err2 <- mean((test$avgGPA-yhat.bag)^2)
test.bag.err2



# build a random forest model for classification problem
# use a m smaller than 5
# mtry = 3
rf.pstat = randomForest(avgGPA ~ ., data=train,
                        mtry=3, 
                        ntree = 10000,
                        importance=TRUE)
rf.pstat

plot(rf.pstat)

# obtain predictions on testing data
yhat.rf = predict(rf.pstat, newdata = test)

# obtain MSE
test.rf.err = mean((test$avgGPA - yhat.rf)^2)

# compare normal random forest model error vs. bagged random forest error
test.bag.err
test.rf.err




