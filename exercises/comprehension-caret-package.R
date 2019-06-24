rm(list = ls())

#Question 1__________________________________________________________

library(randomForest)
library(caret)
library(rpart)
library(Rborist)

n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

set.seed(1)

fit <- train(y ~ x,
             method = "Rborist",
             tuneGrid = data.frame(predFixed = 1, minNode = seq(25, 100, 25)),
             data = dat)

results <- data.frame(fit$results$minNode, fit$results$RMSE)
results <- results[order(fit$results$RMSE),]
results

#Question 2__________________________________________________________

library(ggplot2)
library(caret)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

#Question 3__________________________________________________________

data("tissue_gene_expression")

dat <- tissue_gene_expression
y <- dat$y
x <- dat$x

set.seed(1991)

fit <- train(x, y,
             method = "rpart",
             tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))

results <- data.frame(fit$results$cp, fit$results$Accuracy)
results <- results[order(-fit$results$Accuracy),]
results

#Question 4__________________________________________________________

best_fit <- train(x, y,
             method = "rpart",
             tuneGrid = data.frame(cp = 0.00))

confusionMatrix(best_fit)

#Question 5__________________________________________________________

data("tissue_gene_expression")

dat <- tissue_gene_expression
y <- dat$y
x <- dat$x

set.seed(1991)

fit <- train(x, y,
             method = "rpart",
             tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
             control = rpart.control(minsplit = 0))

results <- data.frame(fit$results$cp, fit$results$Accuracy)
results <- results[order(-fit$results$Accuracy),]
results

best_fit <- train(x, y,
                  method = "rpart",
                  tuneGrid = data.frame(cp = 0.00),
                  control = rpart.control(minsplit = 0))

confusionMatrix(best_fit)

#Question 6__________________________________________________________

plot(best_fit$finalModel, margin = 0.1)
text(best_fit$finalModel, cex = 0.75)

#Question 7__________________________________________________________

data("tissue_gene_expression")

dat <- tissue_gene_expression
y <- dat$y
x <- dat$x

set.seed(1991)

fit <- train(x, y,
             method = "rf",
             tuneGrid = data.frame(mtry = seq(50, 200, 25)),
             nodesize = 1)

results <- data.frame(fit$results$mtry, fit$results$Accuracy)
results <- results[order(-fit$results$Accuracy),]
results

#Question 8__________________________________________________________

imp <- varImp(fit)
imp

#Question 9__________________________________________________________

sortImp(imp, 7)


