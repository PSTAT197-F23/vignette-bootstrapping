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

# histogram of pstat gpa
ggplot(pstat, aes(x = avgGPA)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Average GPA of Students in PSTAT Classes") +
  xlab("Average GPA") +
  ylab("Count")

# calculate t test without bootstrapping
print(t.test(pstat$avgGPA)$conf.int)

# create function to calculate the mean of each bootstrap sample
calculate_mean <- function(x) {
  return(mean(x, na.rm = TRUE))
}

# function for bootstrap resampling
bootstrap_resample <- function(data, fun, B = 1000) {
  n <- length(data)
  resampled_means <- numeric(B)
  
  for (i in 1:B) {
    resample <- sample(data, size = n, replace = TRUE)
    resampled_means[i] <- fun(resample)
  }
  
  return(resampled_means)
}

# Extract gpa vector from the data frame
pstat_gpa <- pstat$avgGPA

# Number of bootstrap samples
B <- 1000

# Bootstrap resampling
bootstrap_means_pstat <- bootstrap_resample(pstat_gpa, calculate_mean, B)

# Calculate confidence intervals
confidence_interval_pstat <- quantile(bootstrap_means_pstat, c(0.025, 0.975))

# Print the results
print("Confidence Interval for Bootstrapped PSTAT GPA:")
print(confidence_interval_pstat)

# Number of bootstrap samples
B <- 10000

# Bootstrap resampling for 10,000 samples
bootstrap_means_pstat2 <- bootstrap_resample(pstat_gpa, calculate_mean, B)

par(mfrow = c(1, 2))
# histogram of average gpa calulated for each resample
hist(bootstrap_means_pstat, col = "blue", border = "black",
     main = "1,000 Bootstrap Samples",
     xlab = "Average GPA")

hist(bootstrap_means_pstat2, col = "blue", border = "black",
     main = "10,000 Bootstrap Samples",
     xlab = "Average GPA")

# histogram of pstat 171 gpa
ggplot(pstat171, aes(x = avgGPA)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Average GPA of Students in PSTAT 171") +
  xlab("Average GPA") +
  ylab("Count")

# calculate t statistic
calculate_t <- function(x) {
  return(t.test(x)$statistic)
}

# bootstrap function with standard error of each sample
bootstrap_resample_std <- function(data, fun, B = 10000) {
  n <- length(data)
  resampled_means <- numeric(B)
  standard_error <- numeric(B)
  
  for (i in 1:B) {
    resample <- sample(data, size = n, replace = TRUE)
    resampled_means[i] <- fun(resample)
    standard_error[i] <- sd(resample)/sqrt(length(resample))
  }
  return(list(resampled_means, standard_error))
}

# confidence interval without bootstrapping
print(t.test(pstat171$avgGPA)$conf.int)

# bootstrap percentile interval

# Bootstrap resampling
bootstrap_means_pstat171 <- bootstrap_resample(pstat171$avgGPA, calculate_mean, B)

# Calculate confidence intervals
confidence_interval_pstat171 <-quantile(bootstrap_means_pstat171, c(0.025, 0.975))

# Print the results
print("Bootstrapped Confidence Interval for PSTAT 171 GPA:")
print(confidence_interval_pstat171)

# bootstrap t interval

# scale the data first
scaled_pstat171_data <- scale(pstat171_data$avgGPA, scale = FALSE)

# Bootstrap resampling 
bootstrap_t_pstat171 <- bootstrap_resample_std(scaled_pstat171_data, calculate_t, B)

# Calculate confidence intervals
# first find t statistic at 0.025 and 0.975 quantile
lower_q <- unname(quantile(unlist(bootstrap_t_pstat171[1]), 0.025))
upper_q <- unname(quantile(unlist(bootstrap_t_pstat171[1]), 0.975))

# calculate bounds of interval
lower_bound <- mean(pstat171$avgGPA) - upper_q*as.numeric(mean(unlist(bootstrap_t_pstat171[2])))
upper_bound <- mean(pstat171$avgGPA) - lower_q*as.numeric(mean(unlist(bootstrap_t_pstat171[2])))

print("Confidence Interval for PSTAT 171:")
print(lower_bound)
print(upper_bound)


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

pstat <- pstat %>% select(-dept)

# Step 1: Create Recipe
recipe <- recipe(avgGPA ~ ., data = stat_classes) %>%
  step_dummy(all_nominal(), one_hot = TRUE)

# Step 2: Create Model Specification
model_spec <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Step 3: Create Workflow
workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(model_spec)

# Step 4: Split Data
data_split <- initial_split(stat_classes, prop = 0.8, strata = instructor)
train_data <- training(data_split)
test_data <- testing(data_split)

# Step 5: Train and Evaluate Model
model <- workflow %>%
  fit(train_data)

# Step 6: Predictions and Evaluation
predictions <- predict(model, test_data) %>%
  bind_cols(test_data)
mse <- mean((predictions$avgGPA - predictions$.pred)^2)
cat("Mean Squared Error:", mse, "\n")

mse <- mean((predictions$avgGPA - predictions$.pred)^2)
mae <- mean(abs(predictions$avgGPA - predictions$.pred))

cat("Mean Squared Error:", mse, "\n")
cat("Mean Absolute Error:", mae, "\n")

# Additional Step: Model Accuracy (Optional)
# For a regression model, accuracy might not be the most suitable metric,
# but you can calculate the proportion of predictions within a certain threshold
threshold <- 0.25  # Example threshold, adjust as needed
accuracy <- mean(abs(predictions$avgGPA - predictions$.pred) <= threshold)
cat("Accuracy (within threshold):", accuracy, "\n")

# Step 7: Model Interpretability
# Extract feature importance using the vip package
feature_importance <- vip(xgb_model)
print("Feature Importance:")
print(feature_importance)




