#Question 1_____________________________________________________________

library(caret)
library(dslabs)

tissue_gene_expression("heights")

y <- heights$sex
x <- heights$height

set.seed(1)

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

ks <- seq(1, 100, 3)

F_1 <- sapply(ks, function(k){
  fit <- knn3(as.matrix(train_set$height), train_set$sex, k = k)
  y_hat <- predict(fit, test_set$height, type = "class") 
  #%>% factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(test_set$sex))
})

max(F_1)

output <- data.frame(ks, F_1) 
output[order(-F_1),]

#Question 2_____________________________________________________________

library(purrr)
library(dslabs)
data("tissue_gene_expression")

set.seed(1)

test_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
test_x <- tissue_gene_expression$x[-test_index, ]
train_x <- tissue_gene_expression$x[test_index, ]
test_y <- tissue_gene_expression$y[-test_index]
train_y <- tissue_gene_expression$y[test_index]

ks <- seq(1, 11, 2)

accuracy <- sapply(ks, function(k){
  set.seed(1)
  fit <- knn3(as.matrix(train_x), train_y, k = k)
  y_hat <- predict(fit, test_x, type = "class") 
  confusionMatrix(data = y_hat, reference = test_y)$overall["Accuracy"]
})

output <- data.frame(ks, accuracy) 
print(output)