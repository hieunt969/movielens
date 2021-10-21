#######################################
# Name:     Hieu Nguyen               #
# Course:   Data Science Capstone     #
# Project:  MovieLens                 #
# Date:     Oct 2021                  #
# Email:    hieunguyenthanh@gmail.com #
#######################################


##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Check and install packages if not exist 
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                          repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
                                          repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)



########################
# Dataset download  #
########################

# Download MovieLens 10M dataset from:
#     https://grouplens.org/datasets/movielens/10m/
#     http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


#######################################
# Data exploration and visualization  #
#######################################

# Extract information from current data and add back to dataset 
# to check relationship of these information with rating:
# - year: the year that movie was made: extracted from title
# - time that user give rating: extracted from timestamp
#     - week: week number: 1 - 52
#     - wday: Monday, Tuesday ... Sunday in form week-day number  1, ..7
#     - hour: 0, 1 ... 23 

movielens <- movielens %>% mutate(year = strtoi(substr(title, nchar(title) - 4, nchar(title) -1)),
                      week = week(as_datetime(timestamp)),
                      wday = wday(as_datetime(timestamp)),
                      hour = hour(as_datetime(timestamp)))

# Data structure of Movielens dataset
str(movielens)

movielens %>% as_tibble()

# Number of unique userId, movieId, genres
data.frame(userId = n_distinct(movielens$userId),
           movieId = n_distinct(movielens$movieId),
           genres = n_distinct(movielens$genres),
           row.names = "MovieLens: Number of Unique") %>% knitr::kable()

# Plot of distribution of ratings
movielens %>% group_by(rating) %>% summarize(count = n()) %>%
  ggplot(aes(rating, count)) + geom_col(fill = "blue") + 
  xlab("Ratings") + ylab("Count") + ggtitle("distribution of rating in edx dataset") +
  scale_x_continuous(breaks = seq(0,5,0.5)) + 
  scale_y_continuous(labels = scales::comma) + 
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = "blue", face = "bold"))


# Plot Number of Users by Number of Ratings
movielens %>% group_by(userId) %>% summarize(count = n()) %>%
  ggplot(aes(count)) + geom_histogram(color = "blue", fill="white") +
  xlab("# ratings") + ylab("# users") + ggtitle("Numbers of Users by Number of Ratings") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = "blue", face = "bold")) +
  scale_x_log10(labels = scales::comma)

# Plot Numbers of Movies by Number of Ratings
movielens %>% group_by(movieId) %>% summarize(count = n()) %>%
  ggplot(aes(count)) + geom_histogram(color = "blue", fill="white") +
  xlab("# ratings") + ylab("# movies") + ggtitle("Numbers of Movies by Number of Ratings") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = "blue", face = "bold")) +
  scale_x_log10(labels = scales::comma)

# Plot Number of Genres by Number of Ratings
movielens %>% group_by(genres) %>% summarize(count = n()) %>%
  ggplot(aes(count)) + geom_histogram(color = "blue", fill="white") +
  xlab("# ratings") + ylab("# genres") + ggtitle("Numbers of Genres by Number of Ratings") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = "blue", face = "bold")) +
  scale_x_log10(labels = scales::comma)

# Plot Number of Years of Production by Number of Ratings
movielens %>% group_by(year) %>% summarize(count = n()) %>%
  ggplot(aes(count)) + geom_histogram(color = "blue", fill="white") +
  xlab("# ratings") + ylab("# years") + ggtitle("Numbers of Years of Production by Number of Ratings") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = "blue", face = "bold")) +
  scale_x_log10(labels = scales::comma)

# Plot of Number of Users / Average Rating
movielens %>% group_by(userId) %>% 
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(color="blue", fill="white") +
  xlab("average rating") + ylab("Number of users") + 
  ggtitle("Number of user / average rating") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = "blue", face = "bold")) +
  scale_x_continuous(breaks = seq(0,5,0.5)) +
  scale_y_continuous(labels = scales::comma)

# Plot of Number of Movies / Average ratings
movielens %>% group_by(movieId) %>% 
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(color="blue", fill="white") +
  xlab("average rating") + ylab("Number of movies") + 
  ggtitle("Number of movies / average rating") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = "blue", face = "bold")) +
  scale_x_continuous(breaks = seq(0,5,0.5)) +
  scale_y_continuous(labels = scales::comma)

# Plot average ratings by genres
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg_rating = mean(rating)) %>%
  filter(n > 20000) %>%
  mutate(genres = reorder(genres, desc(avg_rating))) %>% 
  ggplot(aes(genres, avg_rating)) + 
  geom_point() + geom_smooth() + 
  ggtitle("Average rating by Genres") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = "blue", face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5)) +
  scale_y_continuous(breaks = seq(0,5,0.5))

# Plot average ratings by year of production
movielens %>% group_by(year) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(year, avg_rating)) + 
  geom_point() + geom_smooth() + 
  ggtitle("Average rating by Year of production") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = "blue", face = "bold")) +
  scale_y_continuous(breaks = seq(0,5,0.5))

# Plot average ratings by week timestamp
movielens %>% group_by(week) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(week, avg_rating)) + 
  geom_point() + geom_smooth() + 
  ggtitle("Average rating by Week timestamp") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = "blue", face = "bold")) +
  scale_y_continuous(breaks = seq(0,5,0.5)) +
  scale_x_continuous(breaks = seq(1,52,2))
  
# Plot average ratings by weekday timestamp
  movielens %>% group_by(wday) %>%
    summarize(avg_rating = mean(rating)) %>%
    ggplot(aes(wday, avg_rating)) + 
    geom_point() + geom_smooth() + 
    xlab("Weekday (1, 2 ... 7 = Sun, Mon ... Sat)") +
    ggtitle("Average rating by Weekday timestamp") +
    theme(plot.title = element_text(hjust = 0.5, size = 10, color = "blue", face = "bold")) +
    scale_y_continuous(breaks = seq(0,5,0.5)) + 
    scale_x_continuous(breaks = seq(1,7,1))
  
# Plot average ratings by hour timestamp
movielens %>% group_by(hour) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(hour, avg_rating)) + 
  geom_point() + geom_smooth() + 
  ggtitle("Average rating by Hour timestamp") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = "blue", face = "bold")) +
  scale_y_continuous(breaks = seq(0,5,0.5)) +
  scale_x_continuous(breaks = seq(0,23,1))


#####################
# Partition Dataset #
#####################

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% semi_join(edx, by = "movieId") %>% semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Create training and testing data from edx dataset to build model
set.seed(1, sample.kind="Rounding")

# Partition edx datase to training and testing data
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)

train_set <- edx[-test_index]
temp <- edx[test_index]

# Make sure userId and movieId in test_set are also in train_set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>% 
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# Remove unused variables
rm(dl, ratings, movies, test_index, temp, removed) 


# Number of instances in each dataset and its %
data.frame(movielens = c(nrow(movielens), 100),
           edx = c(nrow(edx), round(nrow(edx)/nrow(movielens) * 100)),
           validation = c(nrow(validation), round(nrow(validation) / nrow(movielens)*100)),
           row.names = c("Number of instances", "Percentage (%)")) %>% knitr::kable()


data.frame(edx = c(nrow(edx), 100),
           train_set = c(nrow(train_set), round(nrow(train_set)/nrow(edx) * 100)),
           test_set = c(nrow(test_set), round(nrow(test_set) / nrow(edx)*100)),
           row.names = c("Number of instances", "Percentage (%)")) %>% knitr::kable()


###########################################################################
# Evaluate RMSE of different algorithms on train_set and test_set dataset #
###########################################################################

# Define Root Mean Square Error (RMSE) function to evaluate prediction accuracy
RMSE <- function(real_rating, predicted_rating) {
  sqrt(mean((real_rating - predicted_rating)^2))
}

# 1. Average prediction model
predicted_ratings <- mean(train_set$rating)

rmses_score <- data.frame(method = "Average Rating", 
                          rmse_score= RMSE(test_set$rating, predicted_ratings))

rmses_score %>% knitr::kable()

# 2. Movie model
mu <- mean(train_set$rating)
b_i <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- test_set %>% 
  left_join(b_i, by = "movieId") %>% 
  mutate(pred = mu + b_i) %>% .$pred

rmses_score <- rbind(rmses_score, c("Movie", 
                                    RMSE(test_set$rating, predicted_ratings)))
rmses_score %>% knitr::kable()

# 3. Movie + User model
b_u <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>% .$pred

rmses_score <- rbind(rmses_score, c("Movie + User", 
                                    RMSE(test_set$rating, predicted_ratings)))
rmses_score %>% knitr::kable()

# 4. Movie + User + Genres model
b_g <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>% .$pred

rmses_score <- rbind(rmses_score, c("Movie + User + Genres", 
                                    RMSE(test_set$rating, predicted_ratings)))
rmses_score %>% knitr::kable()

# 5. Movie + User + Genres + Production Year model
b_y <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  group_by(year) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u - b_g))

predicted_ratings <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "year") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>% .$pred 

rmses_score <- rbind(rmses_score, c("Movie/User/Genres/Production Year", 
                                    RMSE(test_set$rating, predicted_ratings)))
rmses_score %>% knitr::kable()

# 6. Movie + User + Genres + Production Year + Week model
b_w <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "year") %>%
  group_by(week) %>%
  summarize(b_w = mean(rating - mu - b_i - b_u - b_g + b_y))

predicted_ratings <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "year") %>%
  left_join(b_w, by = "week") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y + b_w) %>% .$pred 

rmses_score <- rbind(rmses_score, c("Movie/User/Genres/Production Year/Week", 
                                    RMSE(test_set$rating, predicted_ratings)))
rmses_score %>% knitr::kable()

# 7. Movie + User + Genres + Production Year + Week + Weekday model
b_wd <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "year") %>%
  left_join(b_w, by = "week") %>%
  group_by(wday) %>%
  summarize(b_wd = mean(rating - mu - b_i - b_u - b_g + b_y + b_w))

predicted_ratings <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "year") %>%
  left_join(b_w, by = "week") %>%
  left_join(b_wd, by = "wday") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y + b_w + b_wd) %>% .$pred 

rmses_score <- rbind(rmses_score, c("Movie/User/Genres/Production Year/Week/Weekday", 
                                    RMSE(test_set$rating, predicted_ratings)))
rmses_score %>% knitr::kable()

# 8. Movie + User + Genres + Production Year + Week + Weekday + Hour model
b_h <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "year") %>%
  left_join(b_w, by = "week") %>%
  left_join(b_wd, by = "wday") %>%
  group_by(hour) %>%
  summarize(b_h = mean(rating - mu - b_i - b_u - b_g + b_y + b_w + b_wd))

predicted_ratings <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "year") %>%
  left_join(b_w, by = "week") %>%
  left_join(b_wd, by = "wday") %>%
  left_join(b_h, by = "hour") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y + b_w + b_wd + b_h) %>% .$pred 

rmses_score <- rbind(rmses_score, c("Movie/User/Genres/Production Year/Week/Weekday/Hour", 
                                    RMSE(test_set$rating, predicted_ratings)))
rmses_score %>% knitr::kable()


# 9. Regularized Movie 
lambdas <- seq(0,10,1)
mu <- mean(train_set$rating)

rmses <- sapply(lambdas, function(l) {
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  RMSE(test_set$rating, predicted_ratings)
})

rmses_score <- rbind(rmses_score, 
                     c(paste("Reg. Movie (lamda = ",
                             lambdas[which.min(rmses)], ")", sep = ""), min(rmses)))
rmses_score %>% knitr::kable()

qplot(lambdas, rmses)


# 10. Regularized Movie + User
rmses <- sapply(lambdas, function(l) {
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  RMSE(test_set$rating, predicted_ratings)
})

rmses_score <- rbind(rmses_score, 
                     c(paste("Reg. Movie + User (lamda = ",
                             lambdas[which.min(rmses)], ")", sep = ""), min(rmses)))
rmses_score %>% knitr::kable()

qplot(lambdas, rmses)

# 11. Regularized Movie + User + Genres
rmses <- sapply(lambdas, function(l) {
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + l))
  b_g <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + l)) 
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  RMSE(test_set$rating, predicted_ratings)
})

rmses_score <- rbind(rmses_score, 
                     c(paste("Reg. Movie + User + Genres (lamda = ",
                             lambdas[which.min(rmses)], ")", sep = ""),min(rmses)))
rmses_score %>% knitr::kable()

qplot(lambdas, rmses)

# 12. Regularized Movie + User + Genres + Production Year
rmses <- sapply(lambdas, function(l) {
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + l))
  b_g <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + l))
  b_y <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u + b_g)/(n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
    .$pred
  RMSE(test_set$rating, predicted_ratings)
})

rmses_score <- rbind(rmses_score, 
                     c(paste("Reg. Movie/User/Genres/Production Year (lamda=",
                             lambdas[which.min(rmses)], ")", sep = ""),min(rmses)))
rmses_score %>% knitr::kable()

qplot(lambdas, rmses)


# 13. Regularized Movie + User + Genres + Production Year + Week
rmses <- sapply(lambdas, function(l) {
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + l))
  b_g <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + l))
  b_y <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u + b_g)/(n() + l))
  b_w <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year") %>%
    group_by(week) %>%
    summarize(b_w = sum(rating - mu - b_i - b_u + b_g + b_y)/(n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year") %>%
    left_join(b_w, by = "week") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y + b_w) %>%
    .$pred
  RMSE(test_set$rating, predicted_ratings)
})

rmses_score <- rbind(rmses_score, 
                     c(paste("Reg. Movie/User/Genres/Production Year/Week (lamda=",
                             lambdas[which.min(rmses)], ")", sep = ""), min(rmses)))
rmses_score %>% knitr::kable()

qplot(lambdas, rmses)


# 14. Regularized Movie + User + Genres + Production Year + Week + Weekday
rmses <- sapply(lambdas, function(l) {
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + l))
  b_g <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + l))
  b_y <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u + b_g)/(n() + l))
  b_w <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year") %>%
    group_by(week) %>%
    summarize(b_w = sum(rating - mu - b_i - b_u + b_g + b_y)/(n() + l))
  b_wd <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year") %>%
    left_join(b_w, by = "week") %>%
    group_by(wday) %>%
    summarize(b_wd = sum(rating - mu - b_i - b_u + b_g + b_y + b_w)/(n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year") %>%
    left_join(b_w, by = "week") %>%
    left_join(b_wd, by = "wday") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y + b_w + b_wd) %>%
    .$pred
  RMSE(test_set$rating, predicted_ratings)
})

rmses_score <- rbind(rmses_score, 
                     c(paste("Reg. Movie/User/Genres/Production Year/Week/Weekday
                             (lamda=",lambdas[which.min(rmses)], ")", sep = ""),
                       min(rmses)))
rmses_score %>% knitr::kable()

qplot(lambdas, rmses)

# 15. Regularized Movie + User + Genres + Production Year + Week + Weekday + Hour
rmses <- sapply(lambdas, function(l) {
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + l))
  b_g <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + l))
  b_y <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u + b_g)/(n() + l))
  b_w <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year") %>%
    group_by(week) %>%
    summarize(b_w = sum(rating - mu - b_i - b_u + b_g + b_y)/(n() + l))
  b_wd <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year") %>%
    left_join(b_w, by = "week") %>%
    group_by(wday) %>%
    summarize(b_wd = sum(rating - mu - b_i - b_u + b_g + b_y + b_w)/(n() + l))
  b_h <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year") %>%
    left_join(b_w, by = "week") %>%
    left_join(b_wd, by = "wday") %>%
    group_by(hour) %>%
    summarize(b_h = sum(rating - mu - b_i - b_u + b_g + b_y + b_w + b_wd)/(n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year") %>%
    left_join(b_w, by = "week") %>%
    left_join(b_wd, by = "wday") %>%
    left_join(b_h, by = "hour") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y + b_w + b_wd + b_h) %>%
    .$pred
  RMSE(test_set$rating, predicted_ratings)
})

rmses_score <- rbind(rmses_score, 
                     c(paste("Reg. Movie/User/Genres/Production Year/Week/Weekday/Hour
                             (lamda=",lambdas[which.min(rmses)], ")", 
                             sep = ""), min(rmses)))
rmses_score %>% knitr::kable()

qplot(lambdas, rmses)


# Show the Model that return minimum RMSE
rmses_score[which.min(rmses_score$rmse_score),] %>% knitr::kable()
# the min RMSE model shown that lambda_min = 5


#######################################################################################
# Apply best algorithm that give min RMSE on edx data and evaluate on validation data #
#######################################################################################

mu <- mean(edx$rating)
lambda_min <- 5 

b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + lambda_min))
b_u <- edx %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + lambda_min))
b_g <- edx %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + lambda_min))
b_y <- edx %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u + b_g)/(n() + lambda_min))

predicted_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "year") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  .$pred

rmse <- RMSE(validation$rating, predicted_ratings)

print(paste("RMSE =", rmse, ", compare with target: RMSE < 0.86490 is", rmse < 0.86490))


## END OF SCRIPT ##