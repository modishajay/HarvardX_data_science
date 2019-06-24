#Question 1_________________________________________

library(dplyr)
library(dslabs)
library(caret)

data("mnist_27")

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
print(indexes)

sum(indexes$Resample01 == 7)

#Question 2_________________________________________

total <- 0
for(i in names(indexes)){
  total <- total + sum(indexes[[i]] == 3)
}
total

#Question 3_________________________________________

set.seed(1)

B <- 10000
t <- replicate(B, {
  y <- rnorm(100, 0, 1)
  sample_quantiles <- quantile(y, 0.75)
})

mean(t)
sd(t)

#Question 4_________________________________________

rm(list = ls())

set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
indexes <- createResample(y, 10)
samples <- Map(`[`, list(y), indexes)

sample_q = list()
for(i in 1:10){
  sample_q[i] <- quantile(samples[[i]], 0.75)
}

q_list <- unlist(sample_q)
mean(q_list)
sd(q_list)

#Question 5_________________________________________

rm(list = ls())

set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
indexes <- createResample(y, 10000)
samples <- Map(`[`, list(y), indexes)

sample_q = list()
for(i in 1:10000){
  sample_q[i] <- quantile(samples[[i]], 0.75)
}

q_list <- unlist(sample_q)
mean(q_list)
sd(q_list)


