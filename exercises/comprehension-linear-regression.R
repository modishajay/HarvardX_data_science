library(caret)

#Question 1____________________________________________________________________

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)

rmse <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  test_set <- dat %>% slice(test_index)
  train_set <- dat %>% slice(-test_index)
  
  fit <- lm(y ~ x, data = train_set)
  
  y_hat <- fit$coef[1] + fit$coef[2] * test_set$x 

  sqrt(mean((y_hat - test_set$y) ^ 2))
})

mean(rmse)
sd(rmse)

#Question 2____________________________________________________________________

q2 <- function (n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    test_set <- dat %>% slice(test_index)
    train_set <- dat %>% slice(-test_index)
    
    fit <- lm(y ~ x, data = train_set)
    
    y_hat <- fit$coef[1] + fit$coef[2] * test_set$x 
    
    sqrt(mean((y_hat - test_set$y) ^ 2))
  })
  
  print(mean(rmse))
  print(sd(rmse))
}


n <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
sapply(n, q2)

#Question 4____________________________________________________________________

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)

rmse <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  test_set <- dat %>% slice(test_index)
  train_set <- dat %>% slice(-test_index)
  
  fit <- lm(y ~ x, data = train_set)
  
  y_hat <- fit$coef[1] + fit$coef[2] * test_set$x 
  
  sqrt(mean((y_hat - test_set$y) ^ 2))
})

mean(rmse)
sd(rmse)

#Question 6____________________________________________________________________

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test_set <- dat %>% slice(test_index)
train_set <- dat %>% slice(-test_index)

# fit_1 <- lm(y ~ x_1, data = train_set)
# y_hat <- fit_1$coef[1] + fit_1$coef[2] * test_set$x_1 

# fit_2 <- lm(y ~ x_2, data = train_set)
# y_hat <- fit_2$coef[1] + fit_2$coef[2] * test_set$x_2 

fit_12 <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- fit_12$coef[1] + fit_12$coef[2] * test_set$x_1 + fit_12$coef[3] * test_set$x_2 

rmse <- sqrt(mean((y_hat - test_set$y) ^ 2))
print(rmse)

#Question 8____________________________________________________________________

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test_set <- dat %>% slice(test_index)
train_set <- dat %>% slice(-test_index)

# fit_1 <- lm(y ~ x_1, data = train_set)
# y_hat <- fit_1$coef[1] + fit_1$coef[2] * test_set$x_1

# fit_2 <- lm(y ~ x_2, data = train_set)
# y_hat <- fit_2$coef[1] + fit_2$coef[2] * test_set$x_2

fit_12 <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- fit_12$coef[1] + fit_12$coef[2] * test_set$x_1 + fit_12$coef[3] * test_set$x_2

rmse <- sqrt(mean((y_hat - test_set$y) ^ 2))
print(rmse)