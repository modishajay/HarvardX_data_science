library(caret)
library(tidyverse)

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)

#Question 1_________________________________________________________________
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

#Question 2_________________________________________________________________
#names(train)
#head(train)
max_accuracy <- function(feature){
  min <- min(feature)
  max <- max(feature)
  cutoff <- seq(min, max, 0.1)
  accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(feature > x, "virginica", "versicolor")
    mean(y_hat == train$Species)
  })
  max(accuracy)
}

sepal_length <- max_accuracy(train$Sepal.Length)
sepal_length

sepal_width <- max_accuracy(train$Sepal.Width)
sepal_width

petal_length <- max_accuracy(train$Petal.Length)
petal_length

petal_width <- max_accuracy(train$Petal.Width)
petal_width

#solution by edX:
# foo <- function(x){
#   rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
#   sapply(rangedValues,function(i){
#     y_hat <- ifelse(x>i,'virginica','versicolor')
#     mean(y_hat==train$Species)
#   })
# }
# predictions <- apply(train[,-5],2,foo)
# sapply(predictions,max)	

#Question 3_________________________________________________________________
feature <- train$Petal.Length
min <- min(feature)
max <- max(feature)
cutoff <- seq(min, max, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(feature > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})
best_cutoff <- cutoff[which.max(accuracy)]

y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") 
mean(y_hat == test$Species)

#solution by edX:
# predictions <- foo(train[,3])
# rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
# cutoffs <-rangedValues[which(predictions==max(predictions))]
# 
# y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
# mean(y_hat==test$Species)

#Question 4_________________________________________________________________
sepal_length <- max_accuracy(test$Sepal.Length)
sepal_length

sepal_width <- max_accuracy(test$Sepal.Width)
sepal_width

petal_length <- max_accuracy(test$Petal.Length)
petal_length

petal_width <- max_accuracy(test$Petal.Width)
petal_width

#Question 5_________________________________________________________________
plot(iris,pch=21,bg=iris$Species)

get_best_cutoff <- function(feature){
  min <- min(feature)
  max <- max(feature)
  cutoff <- seq(min, max, 0.1)
  accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(feature > x, "virginica", "versicolor")
    mean(y_hat == train$Species)
  })
  cutoff[which.max(accuracy)]
}

best_cutoff_length <- get_best_cutoff(train$Petal.Length)
best_cutoff_width <- get_best_cutoff(train$Petal.Width)

y_hat <- ifelse(test$Petal.Length > best_cutoff_length | test$Petal.Width > best_cutoff_width, "virginica", "versicolor") 
mean(y_hat == test$Species)

#solution by edX:
# library(caret)
# data(iris)
# iris <- iris[-which(iris$Species=='setosa'),]
# y <- iris$Species
# 
# plot(iris,pch=21,bg=iris$Species)
# 
# set.seed(2)
# test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
# test <- iris[test_index,]
# train <- iris[-test_index,]
# 
# petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
# petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)
# 
# length_predictions <- sapply(petalLengthRange,function(i){
#   y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
#   mean(y_hat==train$Species)
# })
# length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7
# 
# width_predictions <- sapply(petalWidthRange,function(i){
#   y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
#   mean(y_hat==train$Species)
# })
# width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5
# 
# y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
# mean(y_hat==test$Species)
