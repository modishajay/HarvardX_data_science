#Question 1_______________________________________

library(dplyr)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results

#Question 2_______________________________________

#library(devtools)
#devtools::install_bioc("genefilter")
BiocManager::install("genefilter",version = "3.8")
library(genefilter)
library(dplyr)
library(caret)

tt <- colttests(x, y)
head(tt)
pvals <- tt$p.value

#Question 3_______________________________________

ind <- (which(pvals <= 0.01))
length(ind)

#Question 4_______________________________________

x_subset <- x[ ,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

#Question 5_______________________________________

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

#Question 6_______________________________________

library(purrr)
library(dslabs)
library(caret)
data("tissue_gene_expression")

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))
fit$results[order(fit$results$Accuracy)]
