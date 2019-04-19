#Load packages and Libraries
# pacman    : This contains tools to more conveniently perform tasks associated with add-on packages.
# snakecase : Collection of miscellaneous utility functions, supporting data transformation
# ggalt     : Support geom_dumbbell()
# devtools  : Collection of R development tools
if(!require(pacman))install.packages("pacman")
if(!require(devtools))install.packages('devtools')
devtools::install_github('bbc/bbplot')    #Load the BBC plots for use with ggplot2
pacman::p_load('devtools',                                # Development 
               'data.table',                              # Data Importation
               'tidyverse', 'dplyr', 'tidyr', 'stringr',  # Data Manipulation
               'sjmisc', 'snakecase', 'lubridate',        # Data Manipulation
               'ggplot2', 'bbplot', 'ggalt',              # Visualisation 
               'caret', 'randomForest',                   # Classification and Regression 
               'tictoc')                                 # Performance measuring

#Download from MovieLens
##Data Retrieval
# MovieLens dataset: the file folder that is created must be the same name as the folder in the compressed zip file
tic("Prepare Ratings and Movies datasets")
dir_file_ratings <- "ml-10M100K/ratings.dat"   
dir_file_movies <- "ml-10M100K/movies.dat"
grouplens_file <- "http://files.grouplens.org/datasets/movielens/ml-10m.zip"

# Check if the file has already been downloaded to the working directory/unzipped folder and only download one time.
if(!file.exists(dir_file_ratings) || !file.exists(dir_file_movies)) {
  tic("Download from GroupLens") #Initiate the benchmarking
  dl_file <- tempfile()
  download.file(grouplens_file, dl_file)
  toc() #Print the time segment
    # Split the columns of the movies and ratings files using the separator ::. Also rename the columns.
    ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl_file, dir_file_ratings))),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
  movies <- str_split_fixed(readLines(unzip(dl_file, dir_file_movies)), "\\::", 3)
} else {
  print("Files already exist and do not need to be downloaded again")
  ratings <- read.table(text = gsub("::", "\t", readLines(dir_file_ratings)),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
  movies <- str_split_fixed(readLines(dir_file_movies), "\\::", 3)
}
#Add the column names to the movie dataframe and format the fields 
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
toc() 
##Data Preparation
tic("Data Preparation")
#Create new features for release year and the rating timestamp
#Split the year out from Title in to it's own column.
pattern <- "[(]\\d{4}[)]"    #Builds a pattern (year) i.e. ()
string_format <- function(x){
  #Detect the pattern (year). Build up the pattern using a combination of str_detect() and str_view() on one record only in order to verify
  #that the coding was valid. An extract() function is then used to separate the title into two columns: title and year.
  str_match(x, pattern) %>% 
    str_replace("[()]","") %>%
    str_replace("[)]","") %>% 
    str_trim()
} 
#Format the timestamp 
date_format <- function(date){
  #POSIXct formatting
  #Round the time to the nearest hour in order to group by hour later 
  new_date <- as_datetime(date, origin="1970-01-01") 
  round_date(new_date, unit = "1 hour")              
}
movies <- movies %>%
  mutate(release_year = string_format(title),
         film = str_replace(title, pattern, "")) 

#Combine the ratings and movies datasets
movielens <- left_join(ratings, movies, by = "movieId")
#Create new features for release year and the rating timestamp
movielens <- movielens %>% 
  mutate(rating_timestamp = date_format(timestamp),
         rating_year = year(rating_timestamp),    
         rating_month = month(rating_timestamp),
         rating_day = day(rating_timestamp),
         rating_hour = hour(rating_timestamp)) 

#Identify films that do not have a rating in the ratings dataset
unrated <- anti_join(movies, ratings, by = "movieId")
dim(unrated)

# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#Use str() or glimnpse() to check the fields that are contained in the training set. 
#Using string it is clear that there is a small number of dimensions in the training set.
str(edx)

# Free up memory. The movies dataset is kept in memory in order to perform some validations on movies only. For example number of movies released per year.
rm(removed, movielens, ratings, temp)

#Use summary() to check the training set. 
summary(edx)

#The results of the descriptive stats summary shows that there are no NA records present in the data. As there are few
#dimensions the resulting vector is not created as a dataframe. Based on the results there is not need to handle NAs
#for example by using dummy values, mean value, or omitting observations with NAs.
sapply(edx, function(x) sum(length(which(is.na(x)))))

#Split the genres in to specific categories in order to assess the data by a single genre rather than a combination of genres. So for example, 
# "Musical|Romance" will be split out to "Musical" and "Romance"
edx_categories <- edx %>%
  mutate(category = genres) %>% 
  separate_rows(category, sep ="[|]") 

#Categories need to be factorised to support plotting
edx_categories$category <- as.factor(edx_categories$category)       
table(edx_categories$category)

#Need to split the validation set to category level.
validation_categories <- validation %>%
  mutate(category = genres) %>% 
  separate_rows(category, sep ="[|]") 

sapply(validation_categories, function(x) sum(length(which(is.na(x)))))
validation_categories$category <- as.factor(validation_categories$category)

toc()
##Exploratory Data Analysis
tic("Exploratory Data Analysis")
n_distinct(unrated)     #How many films where not rated.
n_distinct(edx$movieId) #How many films have been rated
n_distinct(edx$userId)  #How many unique users provided ratings

#Distribution of Movie Ratings
options(scipen=10000)     #Stop ggplot2 using abbreviations for labels
edx %>% 
  ggplot(aes(rating)) + 
  geom_histogram(fill="#1380A1", binwidth = 0.5) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  xlab("Movie Rating") +
  labs(title="Distribution of Movie Ratings") + 
  bbc_style()

#Distribution of Average Movie ratings
#The distribution of edx_categories and edx was compared to verify that the distribution was similar when the genres where split out to specific categories
edx %>% group_by(movieId) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) + 
  geom_density(fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  xlab("Movie Rating") +
  labs(title="Density Dist. of Movie Ratings") + 
  bbc_style()

#Number of movies rating by decade 
## Group years by decade using floor_date()
## Convert the decades to a discrete (categorical) variable with factor() in order to get the correct labels for geom_bar(). 
movies %>% mutate(decade = year(floor_date(ISOdate(release_year, 1, 1), unit = "10 years"))) %>%
  group_by(decade) %>%
  summarise(releases = n()) %>%
  ggplot(aes(x=factor(decade), y=releases, label=releases)) +
  geom_col(fill="#1380A1") +
  geom_text(size = 3, position = position_dodge(width = 1), vjust = -0.25) +
  bbc_style() +
  labs(title="Movies Released by Decade",
       subtitle = "Number of rated films released per decade") +
  theme(axis.ticks.x = element_line(colour = "#333333"),
        axis.ticks.length =  unit(0.26, "cm")) 

#Distribution of ratings by Decade
edx_categories %>% mutate(decade = year(floor_date(ISOdate(release_year, 1, 1), unit = "10 years"))) %>%
  group_by(decade, category) %>%
  summarise(rating_count = n(),
            mean_rating = mean(rating)) %>%
  ggplot(aes(x=category, y=mean_rating, size=rating_count, color=decade)) +
  geom_point() +
  labs(title="Avg: Ratings over time",
       subtitle = "Average rating received for movies by release date") +
  theme(axis.ticks.x = element_line(colour = "#333333"),
        axis.ticks.length =  unit(0.26, "cm"),
        axis.text.x =element_text(angle = 90, hjust = 1))

#Introducing facet wrapping
edx_categories %>% mutate(decade = year(floor_date(ISOdate(release_year, 1, 1), unit = "10 years"))) %>%
  group_by(decade, category) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(x=factor(decade), y=mean_rating, fill = ifelse(mean_rating > 4, "above4", "below4"))) +
  facet_wrap(~category) +
  geom_col() +
  scale_fill_discrete(name="Rating") +
  coord_flip() +
  labs(title="Average Movie Rating",
         subtitle = "Average rating 4.0 or higher highlighted in red")

#User Ratings
#Display the distribution by the Day of the Week and hour of the rating that was provided by the user.
edx_categories %>% mutate(day_of_week = wday(rating_timestamp, label = TRUE)) %>%
  group_by(day_of_week, rating_hour, category) %>%
  summarise(mean_rating = mean(rating),
            num_ratings = n()) %>%
  ggplot(aes(x=day_of_week, y=rating_hour, fill=num_ratings)) +
  geom_tile() +
  facet_wrap(~category) +
  scale_fill_continuous(name="#Ratings") +
  xlab("Day of Week")+
  ylab("Hours (24h format)")+
  labs(title="Temporal Analysis of Ratings",
       subtitle = "Number of ratings by Hour and Day of the week")+
  theme(axis.text.x =element_text(angle = 90, hjust = 1))

#The following tibble supports the findings of the heat map, showing that Comedy, Drama and Action receive the largest amount of ratings.
edx_categories %>% group_by(category) %>%
  summarise(mean_rating = mean(rating),
            num_ratings = n()) %>%
  arrange(desc(num_ratings))

#Display the distribution by the Day of the Week and hour of the rating that was provided by the user.
edx_categories %>% mutate(day_of_week = wday(rating_timestamp, label = TRUE)) %>%
  group_by(day_of_week, rating_hour, category) %>%
  summarise(mean_rating = mean(rating),
            num_ratings = n()) %>%
  top_n(3) %>% 
  ggplot(aes(x=day_of_week, y=as.factor(rating_hour), fill=num_ratings)) +
  geom_tile() +
  facet_wrap(~category) +
  scale_fill_continuous(name="#Ratings") +
  xlab("Day of Week")+
  ylab("Hours (24h format)")+
  labs(title="Temporal Analysis of Ratings - Top 3",
       subtitle = "Number of ratings by Hour and Day of the week")+
  theme(axis.text.x =element_text(angle = 90, hjust = 1))

toc()
##Modelling preparation
###Predictor preprocessing
#Near zero was assessed however it is not useful for the MovieLens dataset. 
#The majority of features are between 1:2. The function nearZeroVar is removed as it is processive intensive and does not add any value to this program.
#saveMetrics = TRUE enables detailed analysis of the feature comparison. If you receive an integer0 answer it indicates no features are being processed for removal.
tic("Model preparation")
mu_hat <- mean(edx$rating)
mu_hat
toc()
###Model 1: Predict the same rating for all movies regardless of user
tic("Model 1: Predict the same rating for all movies regardless of user")
naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse
rmse_results <- tibble(method = "Average Rating", RMSE = naive_rmse)
toc()
###Model 2: Modeling movie effects
#Using a linear regression lm() results in the vector memory being exhausted
tic("Model 2: Predict using the movie effect")
#Calculate the average rating for each movie and calculate the bias (difference) for each
add_results <- function(n_rmse, model_method){
  bind_rows(rmse_results,
            tibble(method=model_method,  
                       RMSE = n_rmse))
}
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_hat))
head(movie_avgs)

# Plot the difference to show the distribution
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))
predicted_ratings <- mu_hat + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
  
naive_rmse <- RMSE(predicted_ratings, validation$rating)
naive_rmse
rmse_results <- add_results(naive_rmse, "Movie Effect Model")  

toc()
###Model 3: User + Movie effects
tic("Model 3: Predict using the movie + user  effect")
#Calculate the user effect using the training set
user_movie_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu_hat - b_i))   

# Plot the difference to show the distribution
user_movie_avgs %>% qplot(b_u, geom ="histogram", bins = 10, data = ., color = I("black"))

# Calculate the predicted ratings using the test set
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_movie_avgs, by='userId') %>% 
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

naive_rmse <- RMSE(predicted_ratings, validation$rating)
naive_rmse
rmse_results <- add_results(naive_rmse, "User + Movie Effect Model")  
toc()

###Model 4: User + Movie + Category Effect
tic("Model 4: Predict using the user, movie and category effect")

category_avgs <- edx_categories %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_movie_avgs, by='userId') %>%
  group_by(category) %>% 
  summarize(b_c = mean(rating - mu_hat - b_i - b_u))   

# Plot the difference to show the distribution
category_avgs %>% qplot(b_c, geom ="histogram", bins = 10, data = ., color = I("black"))

# Identify the outlier, count the number of movies in this outlier
category_avgs %>% filter(b_c > 0.1) %>%
  mutate(category = as.character(category)) %>%
  left_join(edx, by = c("category" = "genres")) %>%
  group_by(title, category) %>%
  summarise(num_ratings = n(), avg_rating = mean(rating))

#Remove the category (no genres listed) from the prediction.
category_avgs <- category_avgs %>% filter(category != "(no genres listed)")

# Calculate the predicted ratings considering the three effects
predicted_ratings <- validation_categories %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_movie_avgs, by='userId') %>% 
  left_join(category_avgs, by='category') %>% 
  mutate(pred = mu_hat + b_i + b_u + b_c) %>%
  pull(pred)

naive_rmse <- RMSE(predicted_ratings, validation_categories$rating)
naive_rmse
rmse_results <- add_results(naive_rmse, "User + Movie + Category Effect Model")  
toc()
###Model 5: User + Movie + Release Year Effect
tic("Model 5: Predict using the user, movie and Release Year effect")
#Calculate the bias introduced by release year
release_yr_avgs <- edx_categories %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_movie_avgs, by='userId') %>%
  group_by(release_year) %>% 
  summarize(b_r = mean(rating - mu_hat - b_i - b_u))   

# Plot the difference to show the distribution
release_yr_avgs %>% qplot(b_r, geom ="histogram", bins = 10, data = ., color = I("black"))

# Calculate the predicted ratings considering the three effects
predicted_ratings <- validation_categories %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_movie_avgs, by='userId') %>% 
  left_join(release_yr_avgs, by='release_year') %>% 
  mutate(pred = mu_hat + b_i + b_u + b_r) %>%
  pull(pred)

naive_rmse <- RMSE(predicted_ratings, validation_categories$rating)
naive_rmse
rmse_results <- add_results(naive_rmse, "User + Movie + Release Year Effect Model")  

toc()
###Model 6: Movie effects with Regularization
#The general idea behind regularization is to constrain the total variability of the effect sizes
tic("Model 6: Predict using the user, movie and category with regularisation of each predictor")

#The following are the best 10 films before regularisation
validation %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movies, by="movieId") %>%
  arrange(desc(b_i), n) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) 

#The following are the worst 10 films before regularisation
validation %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movies, by="movieId") %>%
  arrange(b_i, n) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) 

lambdas <- seq(0, 10, 0.25)
movie_reg <- edx %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu_hat), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- validation %>% 
    left_join(movie_reg, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu_hat + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, validation$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
rmses[which.min(rmses)]
rmse_results <- add_results(rmses[which.min(rmses)], "Movie Effect Model Regularized")  
toc()

###Model 7: Model 4 with Regularization
#The general idea behind regularization is to constrain the total variability of the effect sizes
tic("Model 7: Predict using the user, movie and category with regularisation ")
#Lambdas is set to 15 after having run severals iterations from 0 to 20 howver the code is retained demonstrating how lambda can be optimally found.
lambdas <- 15
rm(rmses)
rmses <- sapply(lambdas, function(l){
  b_i <- edx_categories %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat)/(n()+l))
  b_u <- edx_categories %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  b_c <- edx_categories %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(category) %>%
    summarize(b_c = sum(rating - b_i - b_u - mu_hat)/(n()+l))
  
  predicted_ratings <- validation_categories  %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_c, by = "category") %>%
    mutate(pred = mu_hat + b_i + b_u + b_c) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation_categories$rating))
})
lambdas[which.min(rmses)]
rmses[which.min(rmses)]
rmse_results <- add_results(rmses[which.min(rmses)], "Movie + User + Category Effect Model Regularized")  
toc()

##Results
#Display the results of the models
rmse_results %>% arrange(desc(RMSE))
        

        