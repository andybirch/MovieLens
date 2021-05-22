# Set global options and formatting
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE,  fig.show = "hold")
knitr::knit_hooks$set(inline = function(x) { if(!is.numeric(x)){ x }else{ prettyNum(round(x,2), big.mark=",") } })
KableTidy = function(x) {
  knitr::kable(x, format.args = list(decimal.mark = '.', big.mark = ",",booktabs = TRUE), align = "c", digits = 2) %>% kable_styling(latex_options = c("striped", "HOLD_position"), font_size = 8)
}
chart_col_1 <- "#008B61"
chart_col_2 <- "#BA328B"
chart_back <- "#DBDBD3"

# Install and load packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(broom)
library(kableExtra)
library(lubridate)
library(recosystem)

# Download and prepare initial data

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Extend the timeout to ensure file download is completed
options(timeout = 300)

# Code provided in course materials to download the initial file
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

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

target <- 0.8649  # set the target RMSE

# Data structure
# Show data fields, types, number of distinct values and check for missing values
data.frame(Type = sapply(movielens, class), 
           Distinct_values = sapply(movielens, n_distinct), 
           Missing_values = sapply(movielens, function(x) sum(is.na(x)))) %>% KableTidy

#  Duplicate title check - list any movieId that has multiple titles
movielens %>% group_by(movieId) %>% summarise(titles = n_distinct(title)) %>% 
  filter(titles !=1)

# Duplicate movieID check - list any title that has multiple movieIds
(multi_movieId <- movielens %>% group_by(title) %>% 
    summarise(movies = n_distinct(movieId)) %>% filter(movies !=1) %>% 
    left_join(movielens, by = "title") %>% group_by(title, movieId) %>% 
    summarise(reviews = n())) %>% KableTidy

multi_movieId$reviews[2]*100/(multi_movieId$reviews[1]+multi_movieId$reviews[2]) 

# Unique record check - check if each row is a unique combination of userId and movieId
n_distinct(paste0(movielens$movieId, "-", movielens$userId)) == nrow(movielens)

# Mean number of reviews per movie 
round(nrow(edx) / n_distinct(edx$movieId),0)
  
#Chart distribution of number of reviews per movieId

# Mean number of reviews per movie to annotate chart
reviews_per_movie <- round(nrow(edx) / n_distinct(edx$movieId),0) 

# Calculate number of reviews for each movieId
movie_averages <- edx %>% group_by(movieId, title) %>% 
  summarise(n_reviews = n(), avg_rating = mean(rating)) %>% ungroup()

# Plot distribution of number of reviews per movie
movie_averages %>%  ggplot(aes(n_reviews)) + 
  geom_histogram(bins = 30, fill = chart_col_1, colour = "white") + 
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype =
                                          "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  labs(x = "Number of reviews", y = "Number of movies") +
  scale_y_continuous(breaks = seq(0,900,200), labels = scales::comma) +
  scale_x_log10() +
  geom_vline(xintercept = reviews_per_movie, colour = "red", size = 0.6, linetype = "dashed") +
  annotate("text", label = paste0("<<< Mean of ", round(reviews_per_movie,0)), x = reviews_per_movie +        4500, y = 600) 

as.numeric(quantile(movie_averages$n_reviews, 0.25)) 
median(movie_averages$n_reviews)
  
# movieId summary
# Summary statistics for number of reviews per movieId
tidy(summary(movie_averages$n_reviews)) %>% KableTidy

# Reviews for highly rated movies
movie_averages %>% filter(avg_rating > 4) %>% ggplot(aes(n_reviews)) + 
  geom_histogram(binwidth = 200,  fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_x_continuous(breaks = seq(0,35000,10000), labels = scales::comma) +  
  labs(x = "Number of reviews", y = "Number of movies")

# movies with a rating of four or higher
sum(movie_averages$avg_rating >=4)  

# movies rated 4 or higher, but 3 or less reviews
sum(movie_averages$avg_rating >=4 & movie_averages$n_reviews <= 3) 
  
# Show highly rated movies with few reviews
movie_averages %>% filter(avg_rating > 4) %>% arrange(n_reviews) %>% head(5) %>% KableTidy

# Show highly rated with lots of reviews
movie_averages %>% filter(avg_rating > 4) %>% arrange(desc(n_reviews)) %>% head(5) %>% KableTidy

# Reviews per userId
# Mean number of reviews per user to annotate chart
reviews_per_user <- round(nrow(edx) / n_distinct(edx$userId),0) 

# Number of reviews per user 
user_averages <- edx %>% group_by(userId) %>% summarise(n_reviews = n()) 

# Plot distribution of number of reviews per user
user_averages %>%  ggplot(aes(n_reviews)) + 
  geom_histogram(bins = 30, fill = chart_col_1, colour = "white") + 
  geom_vline(xintercept = reviews_per_user, colour = "red", size = 0.6, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0,8000,2000), labels = scales::comma) +
  scale_x_log10() +
  annotate("text", label = paste0("<<< Mean of ", round(reviews_per_user,0)), x = reviews_per_user + 250, y = 5000) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  labs(x = "Number of reviews", y = "Number of users")

# UserId summary
tidy(summary(user_averages$n_reviews)) %>%  KableTidy

# Add new time based fields
Sys.setenv(TZ="UTC")
edx <- edx %>% mutate(released = as.numeric(str_sub(title, str_length(title)-4, str_length(title)-1)),
                      review_year = year(round_date(as.POSIXct(timestamp, origin = "1970-01-01"),unit = "year")),
                      review_yearmon = round_date(as.POSIXct(timestamp, origin = "1970-01-01"),unit = "month"),
                      review_age = review_year - released)

# Create list of the unique genres, to be used below
genres <- edx %>% group_by(genres) %>% summarise(n_reviews = n(), n_movies = n_distinct(movieId)) %>% 
  arrange(desc(n_reviews))

# Number of genre options
genres %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>% 
  summarise(number = n()) %>% nrow()
  
# Split out the elements of the genre field and show in a simple table
genres %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>% summarise(number = n()) %>% 
  .$genres %>% matrix(nrow = 4)

# Most complex genres
max(str_count(genres$genres,"\\|")) + 1

genres %>% select(genres) %>% mutate(sub_genres = str_count(genres,"\\|") + 1) %>% 
  arrange(desc(sub_genres)) %>% head(5) %>% KableTidy

# Unique genres
nrow(genres)

#Most popular genres
genres %>% top_n(5) %>% KableTidy

#Genres for franchises
edx %>% filter(str_detect(title, "Terminator")) %>% group_by(title, genres, released) %>% 
  summarise(n_reviews = n()) %>% arrange(released) %>% select(-released) %>%  KableTidy

edx %>% filter(str_detect(title, "Star Trek")) %>% group_by(title, genres, released) %>% 
  summarise(n_reviews = n()) %>% arrange(released) %>% select(-released) %>%  KableTidy

# Earliest and most recent review dates 
format(as.POSIXct(min(edx$timestamp), origin = "1970-01-01"),format='%d-%b-%Y')
format(as.POSIXct(max(edx$timestamp), origin = "1970-01-01"),format='%d-%b-%Y')

# Review count over time
edx %>% group_by(review_yearmon) %>% summarise(reviews = n()) %>% ggplot(aes(review_yearmon, reviews)) +
  geom_line(colour = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
      axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,250000,50000),labels = scales::comma) +
  labs(x = "Review date", y = "Number of reviews")

# Chart the proportion of each rating level by year
review_per_year <- edx %>% group_by(review_year) %>% summarise(tot_reviews = n()) 

edx %>% group_by(rating, review_year) %>% summarise(reviews = n()) %>%
  left_join(review_per_year, by = "review_year") %>% 
  mutate(review_prop = reviews / tot_reviews) %>% 
  ggplot(aes(review_year,rating,fill = review_prop)) + geom_tile()+
  scale_fill_gradient2(low = chart_col_1, high = chart_col_2,  mid = "white", midpoint = 0.3, na.value = NA) +
  labs( x = "Year of review", y = "Rating", fill = "Proportion of \n annual total")

# Find the earliest timestamp that had a half point rating
first_half_rating <- edx %>% mutate(temp = rating%%1) %>% filter(temp == 0.5)%>% 
  summarise(start = min(timestamp)) %>% .$start

format(as.POSIXct(first_half_rating, origin = "1970-01-01"),format='%d-%b-%Y')

# Distribution of ratings for full time period
edx %>% group_by(rating) %>% summarise(n_reviews = n()) %>% 
  ggplot(aes(rating, n_reviews)) + geom_bar(stat = "identity", fill = chart_col_1) + 
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,2500000,500000), labels = scales::comma) +
  labs(x = "Rating", y = "Number of reviews")

# Distribution of ratings, split to before and after introduction of half point ratings
edx %>% filter(timestamp < first_half_rating) %>% group_by(rating) %>% summarise(n_reviews = n()) %>% 
  ggplot(aes(rating, n_reviews)) + geom_bar(stat = "identity", fill = chart_col_1) + 
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,2500000,400000), labels = scales::comma) +
  labs(x = "Rating", y = "Number of reviews")

edx %>% filter(timestamp >= first_half_rating) %>% group_by(rating) %>% summarise(n_reviews = n()) %>% 
  ggplot(aes(rating, n_reviews)) + geom_bar(stat = "identity", fill = chart_col_1) + 
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,1000000,200000), labels = scales::comma) +
  labs(x = "Rating", y = "Number of reviews")


# Average rating over time
edx %>% filter(review_year >= 1997) %>% group_by(review_yearmon) %>% summarise(avg_rat = mean(rating)) %>%
  ggplot(aes(review_yearmon, avg_rat)) + geom_point(colour = chart_col_1) + 
  geom_smooth(method = "loess", span = 0.1, method.args = list(degree=1), colour = chart_col_2) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  labs( x = "Date", y = "Average rating")

# Average rating over time for the five most frequently reviewed movies
edx %>% group_by(movieId, title) %>% summarise(n_reviews = n()) %>% 
  arrange(desc(n_reviews)) %>% head(5)  %>% left_join(edx, by = "movieId") %>% 
  group_by(review_age, title.x) %>% 
  summarise(avg_rating = mean(rating), reviews = n()) %>% ungroup() %>% 
  ggplot(aes(review_age, avg_rating))  + 
  geom_smooth(aes(colour = title.x),method = "loess", span = 0.4, method.args = list(degree=1), se = FALSE) + 
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)), legend.position = "bottom") +
  guides(colour=guide_legend(nrow=3)) +
  labs(x = "Age of movie", y = "Average rating", colour = "Movie title")

edx %>% group_by(movieId) %>% summarise(n_reviews = n()) %>%
  arrange(desc(n_reviews)) %>% head(5)  %>% summarise(total_reviews = sum(n_reviews)) %>% 
  .$total_reviews

# Create test and training sets
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
test_set_temp <- edx[test_index,]

# Ensure every record in the test data has info for user and movie in the training data
test_set <- test_set_temp %>% semi_join(train_set, by = "userId") %>% semi_join(train_set, by = "movieId")

# Add back into the training set
removed <- anti_join(test_set_temp, test_set)
train_set <- bind_rows(train_set, removed)

# Define the loss function as RMSE
loss <- function(pred_rating, act_rating){
  sqrt(mean((pred_rating - act_rating)^2))
}

# Result for naive model
global_avg <- mean(train_set$rating)
results <- data.frame(method = "Mean movie \n rating", RMSE = loss(global_avg, test_set$rating))

loss(global_avg, test_set$rating)

# Distribution of average movie ratings
train_set %>% group_by(movieId) %>% summarise(avg_rating = mean(rating)) %>% 
  group_by(avg_rating) %>% ggplot(aes(avg_rating)) +
  geom_histogram(binwidth = 0.5,  fill = chart_col_1, colour = "white") + 
  scale_y_continuous(breaks = seq(0,5000,1000), labels = scales::comma) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  labs(x = "Average rating", y = "Number of movies")

# Calculate the movieId bias
b_i <- train_set %>% mutate(resid = rating - global_avg) %>% group_by(movieId) %>% 
  summarise(b_i = mean(resid))

# Use to make new predictions and measure RMSE
test_set <- test_set %>% left_join(b_i, by = "movieId") %>% mutate(pred_rating_i = global_avg + b_i)
results <- rbind(results, 
          data.frame(method = "Movie bias", RMSE = loss(test_set$pred_rating_i, test_set$rating)))
results %>% KableTidy

# Calculate residuals
b_u <- train_set %>% left_join(b_i, by = "movieId") %>% mutate(resid = rating - global_avg - b_i) %>% 
  group_by(userId) %>% summarise(b_u = mean(resid))

# Plot distribution
b_u %>% ggplot(aes(b_u)) + 
  geom_histogram(binwidth = 0.5,  fill = chart_col_1, colour = "white") + 
  scale_y_continuous(breaks = seq(0,50000,10000), labels = scales::comma) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  labs(x = "Average rating", y = "Number of movies")

# Create new predictions by including the user bias, test RMSE
test_set <- test_set %>% left_join(b_u, by = "userId") %>% mutate(pred_rating_i_u = pred_rating_i + b_u)
results <- rbind(results, 
          data.frame(method = "User bias", RMSE = loss(test_set$pred_rating_i_u, test_set$rating)))
results %>% KableTidy

# Movie user interaction example, echo = FALSE}
tribble(
  ~" ",     ~"Movie 1",    ~"Movie 2", ~"Movie 3",   ~" ... ",        ~"Movie n",
  #--             |--   |-- |-- |--                    /--
  "User 1",   "??", "??", "0.4","...", "??",
  "User 2",   "??", "3", "??", "...","-0.5",
  "User 3",   "-1.2", "??", "??", "...","??",
  "...",   "...", "...", "...", "...","...",
  "User m",   "0.3", "-0.5", "??","...", "??",
) %>% KableTidy()

# Use recosystem

# add reisduals to the test and training sets
train_set <- train_set %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% 
  mutate(resids = rating - global_avg - b_i - b_u)
test_set <- test_set %>% mutate(resids = rating - pred_rating_i_u)

# Create the datasets required for recosystem, note test set doesn't require ratings
reco_train_set <- data_memory(user_index = train_set$userId, item_index = train_set$movieId,
                              rating = train_set$resids)
reco_test_set <- data_memory(user_index = test_set$userId, item_index = test_set$movieId)

# Create an empty model object
r = Reco()

# Tune the model to select the optimal parameters
opts = r$tune(reco_train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                          costp_l1 = 0, costq_l1 = 0,
                                          nthread = 4, niter = 10))

# Train the model using the optimal set of parameters as held in opts$min
r$train(reco_train_set, opts = c(opts$min, nthread = 4, niter = 10))

# Create the predictions for the test set residuals
reco_pred = r$predict(reco_test_set, out_memory())

# Add the residual predictions to the test set
test_set <- cbind(test_set, reco_pred) %>% 
  mutate(final_pred = global_avg + b_i + b_u + reco_pred, final_resids = rating - final_pred,
         final_pred_capped = ifelse(final_pred > 5, 5, ifelse(final_pred < 0.5, 0.5, final_pred)))

# Recosystem tuning

opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
            costp_l1 = 0, costq_l1 = 0,
            nthread = 4, niter = 10)

tribble(
  ~"Parameter",     ~"Description",    ~"Tuning options",
  #--             |--                     /--
  names(opts[1]),   "Number of latent factors",       opts[[1]],         
  names(opts[2]),   "Learning rate, which can be thought of as the step size in gradient descent",       opts[[2]],         
  names(opts[3]),   "L1 regularization cost for user factors",       opts[[3]],         
  names(opts[4]),   "L1 regularization cost for movie factors",       opts[[4]],         
  names(opts[5]),   "Number of threads for parallel computing",       opts[[5]],         
  names(opts[6]),   "Number of iterations",       opts[[6]]
) %>% KableTidy()

# RMSE for recosystem
results <- rbind(results, data.frame(method = "Recommender", 
          RMSE = loss(test_set$final_pred_capped, test_set$rating)))

results$method <- factor(results$method, levels = results$method)

results %>% KableTidy

results %>% ggplot(aes(method, RMSE)) + geom_col(fill = chart_col_1) + 
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  geom_hline(yintercept = target, colour = "red", size = 0.6, linetype = "dashed" ) + 
  annotate("text", label = paste0("Target RMSE is ", target), x = 4, y = target + 0.05, size = 4) +
  labs(x = "Model", y = "RMSE")

# Final RMSE

# Add movie and user bias to validation set 
validation <- validation %>% left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") 

# Create dataset for recosystem
reco_validation_set <- data_memory(user_index = validation$userId, 
                                   item_index = validation$movieId)

# Create the residual predictions 
reco_pred = r$predict(reco_validation_set, out_memory())

# Create the final predictions using the global average movie rating, movie bias, 
# user bias and the residual predictions from recosystem. Then cap to keep predictions
# within feasible limits
validation <- cbind(validation, reco_pred) %>% mutate(
  final_pred = global_avg + b_i + b_u + reco_pred, 
  final_pred_capped = ifelse(final_pred > 5, 5, 
                             ifelse(final_pred < 0.5, 0.5, final_pred)))

loss(validation$final_pred_capped, validation$rating)

