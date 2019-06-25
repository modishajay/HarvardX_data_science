###################################
# Create edx set and validation set
###################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

###################################
# Explore data
###################################

#Question 1: How many rows and columns are there in the edx dataset?

nrow(edx)
ncol(edx)

#Question 2: How many zeros were given as ratings in the edx dataset? How many threes were given as ratings in the edx dataset?

nrow(edx[edx$rating == 0,])
nrow(edx[edx$rating == 3,])

#Question 3: How many different movies are in the edx dataset?
#Question 4: How many different users are in the edx dataset?

apply(edx, 2, function(x) length(unique(x)))

#Question 5: How many movie ratings are in each of the following genres in the edx dataset? Drama, Comedy, Thriller, Romance

library(stringr)

sum(str_detect(edx[,6],"Drama"))
sum(str_detect(edx[,6],"Comedy"))
sum(str_detect(edx[,6],"Thriller"))
sum(str_detect(edx[,6],"Romance"))

#Question 6: Which movie has the greatest number of ratings?

rating_count <- edx %>% 
  group_by(movieId) %>% 
  summarize(num_rating = count(movieId))

id <- rating_count[which.max(rating_count$num_rating$freq),]

edx[(edx$movieId == id$x),]

#Question 7: What are the five most given ratings in order from most to least?

popular_rating <- edx %>%
  group_by(rating) %>%
  summarize(num_rating = count(rating))

popular_rating <- popular_rating[order(-popular_rating$num_rating$freq),]

popular_rating[1:5,1]

#Question 8: True or False: In general, half star ratings are less common than whole star ratings

popular_rating <- popular_rating[order(popular_rating$num_rating$x),]

odd_indexes<-seq(1,10,2)
even_indexes<-seq(2,10,2)

odd_stars <- sum(popular_rating[odd_indexes,])
even_stars <- sum(popular_rating[even_indexes,])

odd_stars < even_stars

##BELOW CODE WAS NOT REQUIRED
# #separate the distinct genres
# separated_genres <- strsplit(edx$genres, split='|', fixed=TRUE)
# 
# #convert the list to a data frame
# genre_count <- sapply(separated_genres, length)
# max_genre_count <- seq_len(max(genre_count))
# genre_table <- t(sapply(separated_genres, "[", i = max_genre_count))
# 
# #merge the genre_table with edx
# edx <- cbind(edx, genre_table)
# head(edx)
# 
# ###THIS CODE IS NOT WORKING!!!
# count_genre <- function(genre){
#   results <- nrow(edx[edx$`1`== genre,]) + nrow(edx[edx$`2` == genre,]) + nrow(edx[edx$`3` == genre,]) + nrow(edx[edx$`4` == genre,]) + nrow(edx[edx$`5` == genre,]) + nrow(edx[edx$`6` == genre,]) + nrow(edx[edx$`7` == genre,]) + nrow(edx[edx$`8` == genre,])
#   return(results)
# }
# count_genre("Drama")

###################################
# Recommendation system
###################################

#RMSE function
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings) ^ 2))
}

#Model 1: Movie Effects

mu <- mean(edx$rating)
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

movie_avgs %>%
  qplot(b_i, geom = "histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- data_frame(Method = "Movie Effects Model", RMSE = model_1_rmse)
rmse_results

#Model 2: Movie + User Effects

edx %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>%
  filter(n() >= 100) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")

user_avgs <- edx %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results, data_frame(Method = "Movie + User Effects Method", RMSE = model_2_rmse))
rmse_results

