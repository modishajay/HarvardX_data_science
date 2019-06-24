rm(list = ls())

library(dplyr)

set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:100),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1)
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#Question 1_________________________________________________

top_schools <- schools[order(-schools$score),]
top_10 <- top_schools[1:10,]

#Question 2_________________________________________________

median(schools$size)

median(top_10$size)

#Question 3_________________________________________________

bottom_schools <- schools[order(schools$score),]
bottom_10 <- bottom_schools[1:10,]
median(bottom_10$size)

#Question 4_________________________________________________

schools %>% ggplot(aes(size, score)) + scale_x_log10() + geom_line()
  
#Question 5_________________________________________________

overall <- mean(sapply(scores, mean))

alpha <- 25
score_reg <- sapply(scores, function(x){
  overall + sum(x-overall)/(length(x)+alpha)})
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Question 6_________________________________________________

library(Metrics)

alphas <- seq(10, 250)

rmses <- sapply(alphas, function(a){
  score_reg <- sapply(scores, function(x){
    overall + sum(x-overall)/(length(x)+a)})
  schools <- schools %>% mutate(score_reg = score_reg)
  return(rmse(schools$score_reg, schools$quality))
})

best_alpha <- alphas[which.min(rmses)]
best_alpha

#Question 7_________________________________________________

alpha <- best_alpha
score_reg <- sapply(scores, function(x){
  overall + sum(x-overall)/(length(x)+alpha)})
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Question 8_________________________________________________

alphas <- seq(10, 250)

rmses <- sapply(alphas, function(a){
  score_reg <- sapply(scores, function(x){
    sum(x)/(length(x)+a)})
  schools <- schools %>% mutate(score_reg = score_reg)
  return(rmse(schools$score_reg, schools$quality))
})

alphas[which.min(rmses)]
