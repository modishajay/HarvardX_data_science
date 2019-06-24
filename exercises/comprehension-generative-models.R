#Question 1______________________________________________

library(dslabs)
library(caret)
data("tissue_gene_expression")

#set.seed(1993, sample.kind = "Rounding") # use this line of code if you are using R 3.6 or later
set.seed(1993) # use this line of code if you are using R 3.5 or earlier
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
data = data.frame(x, y)  

train_lda <- train(y ~ ., method = "lda", data = data)
str(train_lda)
show(train_lda)

#Question 2______________________________________________

train_lda$finalModel

#Question 3______________________________________________

library(dslabs)
library(caret)
data("tissue_gene_expression")

#set.seed(1993, sample.kind = "Rounding") # use this line of code if you are using R 3.6 or later
set.seed(1993) # use this line of code if you are using R 3.5 or earlier
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
data = data.frame(x, y)  

train_qda <- train(y ~ ., method = "qda", data = data)
str(train_qda)
show(train_qda)

#Question 4______________________________________________

train_qda$finalModel

#Question 5______________________________________________

library(dslabs)
library(caret)
data("tissue_gene_expression")

#set.seed(1993, sample.kind = "Rounding") # use this line of code if you are using R 3.6 or later
set.seed(1993) # use this line of code if you are using R 3.5 or earlier
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
data = data.frame(x, y)  

train_lda <- train(y ~ ., method = "lda", data = data, preProcess = "center")
str(train_lda)
show(train_lda)
train_lda$finalModel

#Question 6______________________________________________

library(dslabs)      
library(caret)
data("tissue_gene_expression")

#set.seed(1993, sample.kind = "Rounding") # use this line of code if you are using R 3.6 or later
set.seed(1993) # use this line of code if you are using R 3.5 or earlier
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
data = data.frame(x, y)  

train_lda <- train(y ~ ., method = "lda", data = data, preProcess = "center")
str(train_lda)
show(train_lda)
