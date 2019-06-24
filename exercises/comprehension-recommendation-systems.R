rm(list = ls())

library(dplyr)
library(dslabs)
data("movielens")

#Question 1______________________________________

head(movielens)

p <- movielens %>% count(year)

library(ggplot2)
p %>% ggplot(aes(x=year, y=sqrt(n))) + geom_point()

p[which(p$n == max(p$n)), ]

#Question 2______________________________________

shawshank <- movielens %>% 
  filter(title == grep('Shawshank Redemption', title, ignore.case = TRUE, value= TRUE))
mean(shawshank$rating)

forrest <- movielens %>% 
  filter(title == grep('Forrest Gump', title, ignore.case = TRUE, value= TRUE))
forrest_ratings <- forrest %>% count(year)
forrest_ratings

forrest_ratings$n/(length(unique(movielens$year[movielens$year >= 1994]))) 

#Question 3______________________________________

post_1993_movies_avg_rating <- movielens %>% 
  filter(year > 1993) %>% 
  group_by(movieId) %>%
  summarize(avg_rating = mean(rating))

post_1993_movies_pa_rating <- movielens %>% 
  filter(year > 1993) %>% 
  group_by(movieId) %>%
  count(year) %>%
  summarize(pa_rating = n/(length(unique(movielens$year[movielens$year >= 1994]))))
  
post_1993_movies <- merge(post_1993_movies_avg_rating, post_1993_movies_pa_rating)

post_1993_movies %>%
  ggplot(aes(avg_rating, pa_rating)) +
  geom_point()

#Question 5______________________________________

library(lubridate)

movielens <- mutate(movielens, date = as_datetime(timestamp))

#Question 6______________________________________

movielens %>% 
  mutate(week = round_date(date, unit = c("week"))) %>% 
  group_by(week) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(week, avg_rating)) +
  geom_point() +
  geom_smooth()

#Question 8______________________________________

genre_data <- movielens %>% 
  group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), sd = sd(rating)) %>%
  filter(n >= 1000) 

genre_data %>%
  ggplot(aes(x = genres, y = avg)) +
  geom_bar(stat="identity")

genre_data[which(genre_data$avg == min(genre_data$avg)), ]
