options(timeout = 300)

chart_col_1 <- "#008B61"
chart_back <- "#DBDBD3"

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(ggthemes)
library(broom)
library(kableExtra)
library(lubridate)
library(recosystem)

KableTidy = function(x) {
  knitr::kable(x, format.args = list(decimal.mark = '.', big.mark = ",",booktabs = TRUE)) %>% kable_styling(latex_options = c("striped", "hold_position"), font_size = 8)
}

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

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

# My code
target <- 0.8649  # set the target RMSE

nrow(movielens) #inline

ncol(movielens) #inline

# Data fields overview
data.frame(Type = sapply(movielens, class), 
           Distinct_values = sapply(movielens, n_distinct), 
           Missing_values = sapply(movielens, function(x) sum(is.na(x)))) %>% 
  kbl(booktabs = T) %>% 
  kable_styling(full_width = FALSE, position = "left", bootstrap_options = "bordered", font_size = 10)

movielens %>% mutate(dte = as.POSIXct(timestamp, origin = "1970-01-01")) %>% 
  group_by(timestamp, dte) %>% summarise(n_reviews = n()) %>% filter(n_reviews > 5) %>% 
  ggplot(aes(n_reviews)) + geom_histogram(binwidth = 5)

n_distinct(movielens$movieId) * n_distinct(movielens$userId) # number of combinations of users and movies
nrow(movielens) * 100 / (n_distinct(movielens$movieId) * n_distinct(movielens$userId)) # full matrix is very sparse

# List any movieId that has multiple titles
movielens %>% group_by(movieId) %>% summarise(ids = n_distinct(title)) %>% filter(ids !=1)

# List any title that has multiple movieIds
(dupes <- movielens %>% group_by(title) %>% summarise(ids = n_distinct(movieId)) %>% filter(ids !=1) %>% 
    left_join(movielens, by = "title") %>% group_by(title, movieId) %>% summarise(reviews = n()))

dupes$reviews[2]*100/(dupes$reviews[1]+dupes$reviews[2]) # for inline code

# Check if any user has rated a movie more than once
#movielens %>% group_by(userId, movieId) %>% summarise(n = n()) %>% filter(n != 1)
n_distinct(paste0(movielens$movieId, "-", movielens$userId)) == nrow(movielens)


##rm(dl, ratings, movies, test_index, temp, movielens, removed)
#rm(dl, test_index, temp, movielens, removed)


##### My code  ###


### 1 Introduction
# based on movielens not edx set

### 2 Data structure
## 

## Movies
n_distinct(edx$movieId)  # number of movies

# Mean number of reviews per movie
reviews_per_movie <- round(nrow(edx) / n_distinct(edx$movieId),0) 

# Number of reviews and average rating for each movie
movie_averages <- edx %>% group_by(movieId, title) %>% 
  summarise(n_reviews = n(), avg_rating = mean(rating)) %>% ungroup()

# Plot distribution of number of reviews per movie
movie_averages %>%  ggplot(aes(n_reviews)) + 
  geom_histogram(binwidth = 50,  fill = "#008B61") + 
  geom_vline(xintercept = reviews_per_movie, colour = "red", size = 0.3, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0,2500,500)) +
  scale_x_continuous(breaks = seq(0,35000,5000)) +
  annotate("text", label = paste0("<<< Mean of ", round(reviews_per_movie,0)), x = reviews_per_movie + 4500, y = 1900) +
  theme(panel.background = element_rect(fill = "#DBDBD3",
                                        colour = "black",
                                        size = 0.5, linetype = "solid")) +
  labs(title = "Distribution of number of reviews by movie", x = "Number of reviews", y = "Number of movies")

# Zoom in on movies with up to 1,000 ratings
#movie_averages %>%  filter(n_reviews <= 1000) %>% ggplot(aes(n_reviews)) + 
#  geom_histogram(binwidth = 20, colour = "#FFAB00", fill = "#FFAB00") + 
#  theme_grey() +
#  scale_y_continuous(breaks = seq(0,3000,500)) +
#  theme_economist() +
#  labs(title = "Distribution of number of reviews by movie", x = "Number of reviews", y = "Number of movies")

# Summary of number of reviews per movie
tidy(summary(movie_averages$n_reviews)) %>% kbl(booktabs = T) %>% 
  kable_styling(full_width = FALSE, position = "left", bootstrap_options = "bordered", font_size = 12) 

as.numeric(quantile(movie_averages$n_reviews, 0.25))
median(movie_averages$n_reviews)

# Check out the highest and lowest avg rating - do they have small number of reviews?
movie_averages %>% filter(avg_rating > 4) %>% ggplot(aes(n_reviews)) + 
  geom_histogram(binwidth = 200, colour = "#FFAB00", fill = "#FFAB00")

sum(movie_averages$avg_rating >=4 & movie_averages$n_reviews <= 3)

movie_averages %>% filter(avg_rating > 4) %>% arrange(n_reviews) %>% head(5)
movie_averages %>% filter(avg_rating > 4) %>% arrange(desc(n_reviews)) %>% head(5)

#movie_averages %>% filter(avg_rating > 1.5) %>% ggplot(aes(n_reviews)) + 
#  geom_histogram(binwidth = 100, colour = "#FFAB00", fill = "#FFAB00")

#movie_averages %>% filter(avg_rating < 1.25) %>% arrange(desc(n_reviews))


## Users
n_distinct(edx$userId) # number of users
# Mean number of reviews per user
reviews_per_user <- round(nrow(edx) / n_distinct(edx$userId),0) 

# Number of reviews per user 
user_averages <- edx %>% group_by(userId) %>% summarise(n_reviews = n()) 

# Plot distribution of number of reviews per user
user_averages %>%  ggplot(aes(n_reviews)) + 
  geom_histogram(binwidth = 50, colour = "#FFAB00", fill = "#FFAB00") + 
  theme_grey() +
  geom_vline(xintercept = reviews_per_user, colour = "red") +
  scale_y_continuous(breaks = seq(0,30000,5000)) +
  annotate("text", label = paste0("<<< Mean of ", round(reviews_per_user,0)), x = reviews_per_user + 1050, y = 25000) +
  theme_economist() +
  labs(title = "Distribution of number of reviews by user", x = "Number of reviews", y = "Number of users")

# Summary of number of reviews per user
tidy(summary(user_averages$n_reviews)) %>% kbl(booktabs = T) %>% 
  kable_styling(full_width = FALSE, position = "left", bootstrap_options = "bordered", font_size = 12) 

# Plot distibution of review SD by user, split by high and low number of reviews
#user_averages %>% filter(grp != "M") %>% ggplot(aes(sd_reviews, fill = grp)) + geom_density(alpha = 0.4)

## Genres

# Top genres by number of ratings
(genres <- edx %>% group_by(genres) %>% summarise(n_reviews = n(), n_movies = n_distinct(movieId)) %>% 
    arrange(desc(n_reviews)) %>% top_n(5))
genres <- edx %>% group_by(genres) %>% summarise(n_reviews = n()) %>% arrange(n_reviews)
nrow(genres) # number of genre combinations
# Examples of diferent genres within the same franchise 
#edx %>% filter(str_detect(title, "X-Men")) %>% group_by(title, genres) %>% summarise(n_reviews = n()) 
#edx %>% filter(str_detect(title, "Die Hard")) %>% group_by(title, genres) %>% summarise(n_reviews = n()) 
#edx %>% filter(str_detect(title, "Harry Potter")) %>% group_by(title, genres) %>% summarise(n_reviews = n()) 
edx %>% filter(str_detect(title, "Terminator")) %>% group_by(title, genres) %>% 
  summarise(n_reviews = n()) %>% arrange(genres)
edx %>% filter(str_detect(title, "Star Trek")) %>% group_by(title, genres) %>% 
  summarise(n_reviews = n()) %>% arrange(genres)

## Timestamp
# Timestamps represent seconds since midnight Coordinated Universal Time (UTC) of January 1, 1970.
# Add a few date related fields
Sys.setenv(TZ="UTC")
edx <- edx %>% mutate(released = as.numeric(str_sub(title, str_length(title)-4, str_length(title)-1)),
                      review_year = year(round_date(as.POSIXct(timestamp, origin = "1970-01-01"),unit = "year")),
                      review_yearmon = round_date(as.POSIXct(timestamp, origin = "1970-01-01"),unit = "month"),
                      review_age = review_year - released)

#temp = str_extract(title, regex(   "\\((\\d{4})\\)"   ))     #extract the year of release in brackets
#release_yr = str_extract(temp, regex(   "(\\d{4})"   ))      #remove the brackets and...
#release_yr = as.numeric(release_yr)                      #...convert to a number

# Check start and end date for ratings
min(edx$timestamp) %>% as.POSIXct( origin = "1970-01-01")
max(edx$timestamp) %>% as.POSIXct( origin = "1970-01-01")

# Check number of ratings by month, check for gaps, patterns
edx %>% group_by(review_yearmon) %>% summarise(reviews = n()) %>% ggplot(aes(review_yearmon, reviews)) +
  geom_line(colour = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,250000,50000)) 

#edx %>% ggplot(aes(timestamp)) + geom_histogram(bins = 50)

# Check proportion of different ratings over time - macro view
review_per_year <- edx %>% group_by(review_year) %>% summarise(tot_reviews = n()) 

edx %>% group_by(rating, review_year) %>% summarise(reviews = n()) %>%
  left_join(review_per_year, by = "review_year") %>% 
  mutate(review_prop = reviews / tot_reviews) %>% 
  ggplot(aes(review_year,rating,fill = review_prop)) + geom_tile()+
  scale_fill_gradient(low = "pink", high = "blue", na.value = NA)


# Distribution of ratings - whole time period
edx %>% group_by(rating) %>% summarise(n_reviews = n()) %>% 
  ggplot(aes(rating, n_reviews)) + geom_bar(stat = "identity")

# what date did the half star ratings start
first_half_rating <- edx %>% mutate(temp = rating%%1) %>% filter(temp == 0.5)%>% 
  summarise(start = min(timestamp)) %>% .$start

first_half_rating %>% as.POSIXct( origin = "1970-01-01")

# Distribution prior to when half stars are used
edx %>% filter(timestamp < first_half_rating) %>% group_by(rating) %>% summarise(n_reviews = n()) %>% 
  ggplot(aes(rating, n_reviews)) + geom_bar(stat = "identity")

# Distribution for when half stars are used
edx %>% filter(timestamp >= first_half_rating) %>% group_by(rating) %>% summarise(n_reviews = n()) %>% 
  ggplot(aes(rating, n_reviews)) + geom_bar(stat = "identity")

# ratign trend over time
edx %>% filter(review_year >= 1997) %>% group_by(review_yearmon) %>% summarise(avg_rat = mean(rating)) %>% 
  ggplot(aes(review_yearmon, avg_rat)) + geom_point() + 
  geom_smooth(method = "loess", span = 0.1, method.args = list(degree=1))


# Do ratings vary over time for an individual movie
#edx %>% group_by(movieId, title) %>% summarise(n_reviews = n()) %>% 
#  arrange(desc(n_reviews)) %>% head(10)  %>% left_join(edx, by = "movieId") %>% 
#  group_by(review_yearmon, title.x) %>% 
#  summarise(avg_rating = mean(rating), reviews = n()) %>% ungroup() %>% 
#  ggplot(aes(review_yearmon, avg_rating))  + geom_smooth(aes(colour = title.x))

# How ratings vary for a movie over time, using age when reviewed
edx %>% group_by(movieId, title) %>% summarise(n_reviews = n()) %>% 
  arrange(desc(n_reviews)) %>% head(5)  %>% left_join(edx, by = "movieId") %>% 
  group_by(review_age, title.x) %>% 
  summarise(avg_rating = mean(rating), reviews = n()) %>% ungroup() %>% 
  ggplot(aes(review_age, avg_rating))  + 
  geom_smooth(aes(colour = title.x),method = "loess", span = 0.4, method.args = list(degree=1), se = FALSE) 

edx %>% group_by(movieId) %>% summarise(n_reviews = n()) %>% 
  arrange(desc(n_reviews)) %>% head(5)  %>% summarise(total_reviews = sum(n_reviews))

### 3. Modelling

## Data preparation

# Create a test set
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
test_set_temp <- edx[test_index,]
# check split is ok
nrow(train_set) / nrow(test_set_temp)
# Ensure every record in the test data has info for user and movie in the training data
test_set <- test_set_temp %>% semi_join(train_set, by = "userId") %>% semi_join(train_set, by = "movieId")
# What was removed - can show that they are quite obscure films
removed <- anti_join(test_set_temp, test_set)
# Add back into the training set
train_set <- bind_rows(train_set, removed)

# Define the loss function - RMSE
loss <- function(pred_rating, act_rating){
  sqrt(mean((pred_rating - act_rating)^2))
}

# Naive model - basic mean average
global_avg <- mean(train_set$rating)
results <- data.frame(method = "Mean movie rating", RMSE = loss(global_avg, test_set$rating))

# Add a movie bias

# Plot distribution of number of reviews per movie
train_set %>% group_by(movieId) %>% summarise(avg_rating = mean(rating)) %>% 
  group_by(avg_rating) %>% ggplot(aes(avg_rating)) +
  geom_histogram(binwidth = 0.5, colour = "grey", fill = "#FFAB00") + 
  scale_y_continuous(breaks = seq(0,5000,1000)) +
  theme_economist() +
  labs(title = "Distribution of number of avg reviews", x = "Average rating", y = "Number of movies")

b_i <- train_set %>% mutate(resid = rating - global_avg) %>% group_by(movieId) %>% 
  summarise(b_i = mean(resid)) 
test_set <- test_set %>% left_join(b_i, by = "movieId") %>% mutate(pred_rating_i = global_avg + b_i)
results <- rbind(results, 
                 data.frame(method = "Movie bias", RMSE = loss(test_set$pred_rating_i, test_set$rating)))

# Add a user bias
b_u <- train_set %>% left_join(b_i, by = "movieId") %>% mutate(resid = rating - global_avg - b_i) %>% 
  group_by(userId) %>% summarise(b_u = mean(resid))
b_u %>% ggplot(aes(b_u)) + 
  geom_histogram(binwidth = 0.5, colour = "grey", fill = "#FFAB00") + 
  scale_y_continuous(breaks = seq(0,50000,10000)) +
  theme_economist() +
  labs(title = "Distribution of number of avg reviews", x = "Average rating", y = "Number of movies")

test_set <- test_set %>% left_join(b_u, by = "userId") %>% mutate(pred_rating_i_u = pred_rating_i + b_u)
(results <- rbind(results, 
                  data.frame(method = "User bias", RMSE = loss(test_set$pred_rating_i_u, test_set$rating))))

# Use the recosystem
train_set <- train_set %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% 
  mutate(resids = rating - global_avg - b_i - b_u)
test_set <- test_set %>% mutate(resids = rating - pred_rating_i_u)

reco_train_set <- data_memory(user_index = train_set$userId, item_index = train_set$movieId,
                              rating = train_set$resids)

reco_test_set <- data_memory(user_index = test_set$userId, item_index = test_set$movieId)

r = Reco()
opts = r$tune(reco_train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                          costp_l1 = 0, costq_l1 = 0,
                                          nthread = 4, niter = 10))
r$train(reco_train_set, opts = c(opts$min, nthread = 4, niter = 10))

pred_rvec = r$predict(reco_test_set, out_memory())

test_set <- cbind(test_set, pred_rvec) %>% 
  mutate(final_pred = global_avg + b_i + b_u + pred_rvec, final_resids = rating - final_pred)

loss(test_set$final_pred, test_set$rating)

results <- rbind(results, 
                 data.frame(method = "Recommender", RMSE = loss(test_set$final_pred, test_set$rating)))

test_set <- test_set %>% mutate(final_pred_capped = ifelse(final_pred > 5, 5, ifelse(final_pred < 0.5, 0.5, final_pred)))
loss(test_set$final_pred_capped, test_set$rating)

results <- rbind(results, 
                 data.frame(method = "Recommender capped", RMSE = loss(test_set$final_pred_capped, test_set$rating)))

results$method <- factor(results$method, levels = results$method)

results %>% ggplot(aes(method, RMSE)) + geom_col(fill = "#FFAB00") + theme_economist() +
  geom_hline(yintercept = target, colour = "red") + 
  annotate("text", label = paste0("Target RMSE is ", target), x = 4, y = target + 0.05, size = 3) +
  labs(title = "Accuracy of different methods", x = "Method", y = "RMSE")

# check where big improvements were made - is it movies / users with little data? No, helps all...
#test_set %>% group_by(userId) %>% summarise(reviews = round(n(),-2)) %>% right_join(test_set, by = "userId") %>% 
#  select(reviews, rating, pred_rating_i_u, final_pred) %>% group_by(reviews) %>% 
#  summarise(basic_error = sqrt(mean((pred_rating_i_u - rating)^2)), 
#            reco_error = sqrt(mean((final_pred - rating)^2))) %>% 
#  gather(key = "method", value = "RMSE", -reviews) %>% 
#  ggplot(aes(reviews, RMSE)) + geom_col(aes(fill = method), position = "dodge")

#### Results

validation <- validation %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% 
  mutate(predicted = global_avg + b_i + b_u, resids = rating - predicted)

reco_validation_set <- data_memory(user_index = validation$userId, item_index = validation$movieId)

pred_rvec = r$predict(reco_validation_set, out_memory())
validation <- cbind(validation, pred_rvec) %>% mutate(final_pred = global_avg + b_i + b_u + pred_rvec)
loss(validation$final_pred, validation$rating)
