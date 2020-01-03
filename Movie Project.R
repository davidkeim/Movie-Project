if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

options(digits = 7)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
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

#Create another Test and Training
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

#Again remove any movies or users that don't appear in both sets
validation <-  validation %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Create our function for Residual Means Squared Error, our rating of effectiveness
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Overall Ratings Spread & Visualization
ratings_mean <- mean(train_set$rating)
ratings_buckets <- train_set %>% group_by(rating) %>% count(rating)
ratings_buckets %>% ggplot(aes(rating, n)) + 
  geom_point() + 
  geom_line(aes(ratings_mean)) + 
  ggtitle("Ratings Assigned & Average") +
  ylab("Total Ratings") +
  xlab("Ratings") +
  theme_economist() +
  ggsave(filename = "./figure01.png")

#User Effect Visualiztion
user_averages <- train_set %>% group_by(userId) %>% summarise(n = mean(rating))
user_mean <- mean(user_averages$n)
user_averages %>%
  ggplot(aes(n)) + 
  geom_histogram() +
  ggtitle("Average Rating by User") +
  ylab("Total Ratings") +
  xlab("Ratings") +
  theme_economist() +
  ggsave(filename = "./figure02.png")

#Average Reviews per User Visualization
user_review_count <- train_set %>% group_by(userId) %>% summarise(reviews = length(rating))
median(user_review_count$reviews)
user_review_count %>% ggplot(aes(reorder(userId, -reviews), reviews)) + geom_point() + scale_y_log10() +
  ggtitle("Average Number of Ratings by User") +
  ylab("Total Ratings") +
  xlab("User") +
  theme_economist() +
  ggsave(filename = "./figure04.png")

#Median Reviews per Movie Visualization
movie_review_count <- train_set %>% group_by(movieId) %>% summarise(reviews = length(rating))
median(movie_review_count$reviews)
movie_review_count %>% ggplot(aes(reorder(movieId, -reviews), reviews)) + geom_point() + scale_y_log10() +
  ggtitle("Average Number of Ratings by Movie") +
  ylab("Total Ratings") +
  xlab("Movie") +
  theme_economist() +
  ggsave(filename = "./figure05.png")

#Genre Effect Exploration
mean_rating_genre <- train_set %>% group_by(movieId) %>% group_by(genres) %>% summarise(avg = mean(rating)) %>% arrange(desc(avg))
train_set %>% count(genres) %>% arrange(desc()) %>% slice(1:10) # Most Popular Genres
train_set %>% count(genres) %>% arrange(n) %>% slice(1:10) # Least Popular Genres
mean_rating_genre %>%
  ggplot(aes(x = reorder(genres, -avg), avg)) +
  geom_point() +
  ggtitle("Total Genres by Avg Rating") +
  ylab("Avg Rating") +
  xlab("Genres") +
  theme_economist() +
  ggsave(filename = "./figure03.png")


#show sparse matrix for sampled 100 unique userId and moveiId
users <- sample(unique(train_set$userId), 100)
train_set %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

####
#First Attempt: Mean for every rating
#Find averge of all ratings
mean_hat <- mean(train_set$rating)
mean_hat
#Calculate RMSE
mean_rmse <- RMSE(test_set$rating, mean_hat)
mean_rmse
#Build Results Table
rmse_results <- data.frame(method = "Mean", RMSE = mean_rmse) 

#Baseline RMSE - 1.0600

#####
#Second Attempt: Movie Effect Model
#Estimate Movie bias for all movies
avg <- mean(train_set$rating)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(m = mean(rating - avg))
movie_avgs %>% ggplot(aes(m)) + 
  geom_histogram(bins = 30) +
  ggtitle("Bias by Movie") + theme_economist() + ggsave(filename = "./figure6.png")
#Prediction Model considering movie bias
predicted_ratings <- avg + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(m)
#Calculate RMSE
m_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="M", RMSE = m_rmse))

#RMSE - 0.9429

####
#Model 3: Adding user effect
#Plot average with for users with over 100 movies
train_set %>% group_by(userId) %>% 
  summarize(u = mean(rating)) %>% 
  filter(n() >= 100) %>% 
  ggplot(aes(u)) + geom_histogram(bins = 30) +
  ggtitle("Bias by User") + theme_economist() + ggsave(filename = "./figure7.png")
#estimate user bias for all users
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(u = mean(rating - avg - m))
#calculate predictions considering user effects in previous model
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = avg + m + u) %>%
  pull(pred)
#calculate rmse after modelling user specific effect in previous model
mu_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="M+U", RMSE = mu_rmse))

#RMSE - 0.8646

####
#Model 4: Regularizing Move and User Effect
#Calculate Lambda
lambdas <- seq(0, 10, 0.5)
rmses <- sapply(lambdas, function(l){
  avg <- mean(train_set$rating)
  m <- train_set %>% 
    group_by(movieId) %>%
    summarize(m = sum(rating - avg)/(n()+l))
  u <- train_set %>% 
    left_join(m, by="movieId") %>%
    group_by(userId) %>%
    summarize(u = sum(rating - m - avg)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(m, by = "movieId") %>%
    left_join(u, by = "userId") %>%
    mutate(pred = avg + m + u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]

#Calulate RMSE
rmse_results <- bind_rows(rmse_results, data_frame(method="RM+U", RMSE = min(rmses)))

#RMSE - 0.8638

####
#Model 5: Adding genre effect
#Plot average with for genres with over 100 movies
train_set %>% group_by(genres) %>% 
  summarize(g = mean(rating)) %>% 
  filter(n() >= 100) %>% 
  ggplot(aes(g)) + geom_histogram(bins = 30) +
  ggtitle("Bias by Genre") + theme_economist() + ggsave(filename = "./figure9.png")
#estimate Genre bias for all users
g <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(g = mean(rating - avg - m - u))
#Bring in Regularized Means
avg <- mean(train_set$rating)
m <- train_set %>% 
  group_by(movieId) %>%
  summarize(m = sum(rating - avg)/(n()+lambda))
u <- train_set %>% 
  left_join(m, by="movieId") %>%
  group_by(userId) %>%
  summarize(u = sum(rating - m - avg)/(n()+lambda))
#calculate predictions considering user effects in previous model
predicted_ratings <- test_set %>% 
  left_join(m, by='movieId') %>%
  left_join(u, by='userId') %>%
  left_join(g, by='genres') %>%
  mutate(pred = avg + m + u + g) %>%
  pull(pred)
#calculate rmse after modelling user specific effect in previous model
g_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="RM+U+G", RMSE = g_rmse))
rmse_results
#RMSE - 0.8638

####
#Model 6
#Regularized Genre Effect
lambdas <- seq(0, 10, 0.5)
rmses_2 <- sapply(lambdas, function(l_2){
  avg <- mean(train_set$rating)
  m <- train_set %>% 
    group_by(movieId) %>%
    summarize(m = sum(rating - avg)/(n()+lambda))
  u <- train_set %>% 
    left_join(m, by="movieId") %>%
    group_by(userId) %>%
    summarize(u = sum(rating - m - avg)/(n()+lambda))
  g <- train_set %>%
    left_join(movie_avgs, by = "movieId") %>%
    left_join(user_avgs, by = "userId") %>%
    group_by(genres) %>%
    summarize(g = mean(rating - avg - m - u)/(n()+l_2))
  predicted_ratings <- 
    test_set %>% 
    left_join(m, by = "movieId") %>%
    left_join(u, by = "userId") %>%
    left_join(g, by = "genres") %>%
    mutate(pred = avg + m + u + g) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses_2)
lambda_2 <- lambdas[which.min(rmses)]
#Add Results to Table
rmse_results <- bind_rows(rmse_results, data_frame(method="RM+U+RG", RMSE = min(rmses_2)))
#RMSE - 0.8641


####
#Model 7: Adding Time effect
#Wrangle Time
train_set <- mutate(train_set, year = year(as.POSIXlt(as.numeric(train_set$timestamp),origin="1970-01-01",tz="GMT")))
test_set <- mutate(test_set, year = year(as.POSIXlt(as.numeric(test_set$timestamp),origin="1970-01-01",tz="GMT")))
validation <- mutate(validation, year = year(as.POSIXlt(as.numeric(validation$timestamp),origin="1970-01-01",tz="GMT")))

#Plot average bias by Year
train_set %>% group_by(year) %>%
  summarize(t = mean(rating)) %>% 
  ggplot(aes(t)) + geom_histogram(bins = 30) +
  ggtitle("Bias by Time") + theme_economist() + ggsave(filename = "./figure10.png")
#Estimate Time bias for all users
t <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(g, by = "genres") %>%
  group_by(year) %>%
  summarize(t = mean(rating - avg - m - u - g))
#Bring in Regularized Means
avg <- mean(train_set$rating)
m <- train_set %>% 
  group_by(movieId) %>%
  summarize(m = sum(rating - avg)/(n()+lambda))
u <- train_set %>% 
  left_join(m, by="movieId") %>%
  group_by(userId) %>%
  summarize(u = sum(rating - m - avg)/(n()+lambda))
g <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(g = mean(rating - avg - m - u))
#calculate predictions considering user effects in previous model
predicted_ratings <- test_set %>% 
  left_join(m, by='movieId') %>%
  left_join(u, by='userId') %>%
  left_join(g, by='genres') %>%
  left_join(t, by='year') %>%
  mutate(pred = avg + m + u + g + t) %>%
  pull(pred)
#calculate rmse after modelling user specific effect in previous model
t_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="RM+U+G+T", RMSE = t_rmse))
rmse_results
#RMSE - 0.8637

####
#Run Final Model on Validation Set
#Bring in Regularized Means
avg <- mean(train_set$rating)
m <- train_set %>% 
  group_by(movieId) %>%
  summarize(m = sum(rating - avg)/(n()+lambda))
u <- train_set %>% 
  left_join(m, by="movieId") %>%
  group_by(userId) %>%
  summarize(u = sum(rating - m - avg)/(n()+lambda))
g <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(g = mean(rating - avg - m - u))
t <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(g, by = "genres") %>%
  group_by(year) %>%
  summarize(t = mean(rating - avg - m - u - g))
#calculate predictions considering user effects in previous model
final_predictions <- validation %>% 
  left_join(m, by='movieId') %>%
  left_join(u, by='userId') %>%
  left_join(g, by='genres') %>%
  left_join(t, by='year') %>%
  mutate(pred = avg + m + u + g + t) %>%
  pull(pred)
#Run RMSE
final_rmse <- RMSE(final_predictions, validation$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Final", RMSE = final_rmse))
rmse_results$method <- factor(rmse_results$method, levels = rmse_results$method)
rmse_results %>% ggplot(aes(method, RMSE, label = (round(RMSE, digits = 4)))) + geom_point() + 
  ggtitle("Accuracy by Method") + theme_economist() + geom_text(nudge_y = .01) + 
  theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8)) + 
  ggsave(filename = "./figure8.png")
rmse_results
#Final RMSE - 0.8648

