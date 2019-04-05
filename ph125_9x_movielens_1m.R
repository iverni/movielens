####Load packages and Libraries----
## Packages
# pacman    : This contains tools to more conveniently perform tasks associated with add-on packages.
# snakecase : Collection of miscellaneous utility functions, supporting data transformation
# devtools  : Collection of R development tools
if(!require(pacman))install.packages("pacman")
if(!require(snakecase))install.packages('snakecase')
if(!require(devtools))install.packages('devtools')
devtools::install_github('bbc/bbplot')    #Load the BBC plots for use with ggplot2
pacman::p_load('data.table',                              # Data Importation
               'tidyverse', 'dplyr', 'tidyr', 'stringr',  # Data Manipulation
               'sjmisc', 'snakecase', 'lubridate',        # Data Manipulation
               'ggplot2', 'bbplot',                       # Visualisation 
               'caret',                                   # Classification and Regression 
               'tictoc'                                   # Performance measuring
)

####Download from MovieLens----
# Check if the file has already been downloaded to the working directory and only download one time.
# MovieLens 100K dataset:
# http://files.grouplens.org/datasets/movielens/ml-latest-small.zip
#
dir_file_ratings <- "ml-1m/ratings.dat"   
dir_file_movies <- "ml-1m/movies.dat"
if(!file.exists(dir_file_ratings) || !file.exists(dir_file_movies)) {
  tic("Download from GroupLens")
  dl_file <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-1m.zip", dl_file)
  toc()
  tic("Prepare ratings")
  ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl_file, dir_file_ratings))),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
  toc()
  tic("Prepare movies")
  movies <- str_split_fixed(readLines(unzip(dl_file, dir_file_movies)), "\\::", 3)
} else {
  print("Files already exist and do not need to be downloaded again")
  tic("Prepare ratings")
  ratings <- read.table(text = gsub("::", "\t", readLines(dir_file_ratings)),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
  toc()
  tic("Prepare movies")
  movies <- str_split_fixed(readLines(dir_file_movies), "\\::", 3)
}

colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

#Split the year out from Title in to it's own column.
pattern <- "[(]\\d{4}[)]"    #Builds a pattern (year) i.e. ()
string_format <- function(x){
  #Detect the pattern (year). I built up the pattern using a combination of str_detect() and str_view() on one record only in order to verify
  #that the coding was valid. An extract() function is then used to separate the title into two columns: title and year.
  str_match(x, pattern) %>% 
    str_replace("[()]","") %>%
    str_replace("[)]","") %>% 
    str_trim()
}
#Create new features for release year and the rating timestamp
movies <- movies %>%
  mutate(release_year = string_format(title),
         film = str_replace(title, pattern, "")) 
toc()

####Data Preparation----
tic("Data Preparation")
movielens <- left_join(ratings, movies, by = "movieId")
#Identify films that do not have a rating in the ratings dataset
unrated <- anti_join(movies, ratings, by = "movieId")
dim(unrated)
# Validation set will be 10% of MovieLens data
set.seed(1)
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

#Use str() or glimnpse() to check the fields that are contained in the training set. Check for factors to then determine if there are any novel levels that could potentially impact the algorithm.
#Using string it is clear that there is a small number of dimensions in the training set.
str(edx)

#Format the timestamp 
date_format <- function(date){
#POSIXct formatting
#Round the time to the nearest hour in order to group by hour later 
  new_date <- as_datetime(date, origin="1970-01-01") 
  round_date(new_date, unit = "1 hour")              
}
#Create new features for release year and the rating timestamp
edx <- edx %>%
  mutate(rating_timestamp = date_format(timestamp),
         rating_year = year(rating_timestamp),    
         rating_month = month(rating_timestamp),
         rating_day = day(rating_timestamp),
         rating_hour = hour(rating_timestamp)) 

# Free up memory. The movies dataset is kept in memory in order to perform some validations on movies only. For example number of movies released per year.
rm(removed, movielens, ratings, temp)

#Use summary() to check the training set. desc() provides additional basic descriptive statistics on the dataset.
#It is however slower than summary() when checking the processing timings. It can be used in conjuction with tidyverse and 
#produces a more readable output. 
summary(edx)
#descr(edx)

#The results of the descriptive stats summary shows that there are no NA records present in the data. As there are few
#dimensions the resulting vector is not created as a dataframe. Based on the results there is not need to handle NAs
#for example by using dummy values, mean value, or omitting observations with NAs.
sapply(edx, function(x) sum(length(which(is.na(x)))))

#Check for duplication in the training set. This is performed using the duplicated() function to create a vector of logical values. This is run in combination with table() to verify if there are any duplicate entries. 
#The duplicate check takes place before the genre variable has been split to a specific film category. 
table(duplicated(edx))

#Split the genres in to specific categories in order to assess the data by a single genre rather than a combination of genres. So for example, 
# "Musical|Romance" will be split out to "Musical" and "Romance"
edx_categories <- edx %>%
  mutate(category = genres) %>% 
  separate_rows(category, sep ="[|]") 

sapply(edx_categories, function(x) sum(length(which(is.na(x)))))
summary(edx_categories$category)  #This demonstrates that it is necessary to factorize the category variable
edx_categories$category <- as.factor(edx_categories$category)

toc()
####Exploratory Data Analysis----
tic("Exploratory Data Analysis")
#How many films where not rated.
dim(unrated)

n_distinct(edx$movieId) #How many films have been rated
n_distinct(edx$userId)  #How many unique users provided ratings

#Number of movies released by year. Use geom_col() instead of geom_bar() as I wish to use the count created in the data with the summarize
movies %>% group_by(release_year) %>%
  summarise(releases = n()) %>%
  ggplot(aes(x=release_year, y=releases)) +
  geom_col() + 
  coord_flip() 

#Number of movies percentage by decade - 2 skills - redefine the scale, switch the bins by factoring the x axis. 
## Convert the decades to a discrete (categorical) variable with factor() in order to get the correct labels.
movies %>% mutate(decade = year(floor_date(ISOdate(release_year, 1, 1), unit = "10 years"))) %>%
  group_by(decade) %>%
  summarise(releases = n()) %>%
  ggplot(aes(x=factor(decade), y=releases, label=releases)) +
  geom_col(fill="#1380A1") +
  geom_text(size = 3, position = position_dodge(width = 1), vjust = -0.25) +
  bbc_style() +
  labs(title="Movie Rated",
       subtitle = "Number of movies released per decade that have been rated") +
  theme(axis.ticks.x = element_line(colour = "#333333"),
        axis.ticks.length =  unit(0.26, "cm")) 

#Distribution of ratings by movie
edx_categories %>% mutate(decade = year(floor_date(ISOdate(release_year, 1, 1), unit = "10 years"))) %>%
  group_by(decade) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(x=factor(decade), y=mean_rating)) +
  geom_col(fill="#1380A1") +
  bbc_style() +
  labs(title="Average Movie Rating",
       subtitle = "Average rating received for movies released per decade") +
  theme(axis.ticks.x = element_line(colour = "#333333"),
        axis.ticks.length =  unit(0.26, "cm")) +
   geom_label(aes(label = round(mean_rating,2)),
             hjust = 0.5, 
             vjust = 1, 
             colour = "white", 
             fill = NA, 
             label.size = NA, 
             family="Helvetica", 
             size = 6)

#Distribution of ratings by movie
edx_categories %>% mutate(decade = year(floor_date(ISOdate(release_year, 1, 1), unit = "10 years"))) %>%
  group_by(decade, category) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(x=factor(decade), y=mean_rating, fill = ifelse(mean_rating > 4, "above4", "below4"))) +
  facet_wrap(~category) +
  geom_col() +
  scale_fill_discrete(name="Rating") +
  coord_flip() +
  labs(title="Average Movie Rating",
         subtitle = "Average rating 4.0 or higher")

ANSEO - STOPPED HERE Reduce the size of the BBC X and Y axes

#Average rating by category
#Average rating by time of day
#Average rating by month
#Average rating by day
#Distribution of ratings by movie release year

#Can you identify remakes and compare the ratings and number of ratings?
ANSEO Mutate to identify the original and colour code it. So set the minimum year to original!!!!
ANSEO Code a text.....maybe the remake was a bad idea.......identify the difference for Get Carter......
ANSEO Use symbols to identify the original
ANSEO Use colour to identify the decade
ANSEO Introduce the BBC plot formatting for this output
edx_ext %>% group_by(film, release_year) %>%
  summarise(total_ratings = n(),
            mean_rating = mean(rating)) %>%
  group_by(film) %>%
  filter(n()>1) %>% 
  ggplot(aes(x = film, y = mean_rating)) +
  geom_point() +
  coord_flip()




edx_ext %>% ggplot(aes(x = category)) +
  geom_bar() +
  coord_flip()

edx_ext %>% ggplot(aes(x = year, fill = category)) +
  geom_bar() + 
  coord_flip()

edx_ext %>% ggplot(aes(x = category)) +
  geom_bar() +
  coord_flip()

#Need to scale the outputs as the count is that useful.
#Need to label the outputs
#Need to adjust the X scale

#Display the distribution by Month
edx_ext %>% ggplot(aes(x=rating_month)) +
  geom_bar()

#Display the distribution by weekday
edx_ext %>% mutate(weekday = wday(rating_timestamp, label = TRUE)) %>%
  ggplot(aes(x=weekday)) +
  geom_bar()

#Display the distribution by Time of day
edx_ext %>% ggplot(aes(x=rating_hour)) +
  geom_bar()




% of ratings at the weekend (Friday, Saturday, Sunday)
% of ratings by season
Could extend this further to compare if seasonality is a predictor. Identify the specific dates that are holiday periods and then an





# The category variable will then be converted to a factor so that it may be used in later visualisation.
ANSEO - STOPPED HERE. Need to check out what is happening with the factors here. See the GGPLOT THAT SHOWS THAT 
THERE IS SOME FILMS THAT HAVE NOT BEEN FACETED CORRECTLY. REFER TO THE "NOVEL LEVELS"



# The timestamp will be converted to the relevnt POSITcx format, although it is not yet certain if this information is really necessary. It may be interesting to 
# to consider the temperural data as a separate dimension. Does the weekday influence the recommendation? does the age of the film also influence whether is should be recommended or not?


# Do I need to scale?


#Eliminate zero variance features and low level variance features as desired






toc()
####Modelling preparation ---- 
feature_var_edx <- nearZeroVar(edx_ext, saveMetrics = TRUE)
feature_var_edx 


#Analyse Zero and near-zero variance features. The key fields to check are zeroVar and nzv field. 
#If either are TRUE then the feature would need to be checked further.  
#For MovieLens the nzv is not going to be that relevant except for Category.
#For percentUnique - the lower the percentage, the lower the number of unique values. E.g. Ratings and Year
ANSEO - check if there is a calculation for feature variance in the caret package
feature_variance <- nearZeroVar(edx, saveMetrics = TRUE)
feature_variance
Feature_variance_movies <- nearZeroVar(movies, saveMetrics = TRUE)
Feature_variance_movies

#Correlation and linearity
#Peason or Spearman? Explain
#Provide the calculation for the correlation and explain it
edx_cor <- cor(edx, method = "spearman")



####Model 1 ----
####Model 2 ----
####Model 3 ----
####Model 4 ----
####Model 5 ----
####Model Comparison----
####Findings----
