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
tic("Prepare Ratings and Movies datasets")
dir_file_ratings <- "ml-1m/ratings.dat"   
dir_file_movies <- "ml-1m/movies.dat"
if(!file.exists(dir_file_ratings) || !file.exists(dir_file_movies)) {
  tic("Download from GroupLens")
  dl_file <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-1m.zip", dl_file)
  toc()
  ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl_file, dir_file_ratings))),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
  movies <- str_split_fixed(readLines(unzip(dl_file, dir_file_movies)), "\\::", 3)
} else {
  print("Files already exist and do not need to be downloaded again")
  ratings <- read.table(text = gsub("::", "\t", readLines(dir_file_ratings)),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
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
#The duplicated() statement is process intensive so has been commented out to skip for subsequent repeat processing.
#table(duplicated(edx))

#Split the genres in to specific categories in order to assess the data by a single genre rather than a combination of genres. So for example, 
# "Musical|Romance" will be split out to "Musical" and "Romance"
edx_categories <- edx %>%
  mutate(category = genres) %>% 
  separate_rows(category, sep ="[|]") 

sapply(edx_categories, function(x) sum(length(which(is.na(x)))))
edx_categories$category <- as.factor(edx_categories$category)

toc()
####Exploratory Data Analysis----
tic("Exploratory Data Analysis")
n_distinct(unrated)     #How many films where not rated.
n_distinct(edx$movieId) #How many films have been rated
n_distinct(edx$userId)  #How many unique users provided ratings

#Number of movies released by year. Use geom_col() instead of geom_bar() as I wish to use the count created in the data with the summarize
movies %>% group_by(release_year) %>%
  summarise(releases = n()) %>%
  ggplot(aes(x=release_year, y=releases)) +
  geom_col() + 
  coord_flip() 

#Number of movies percentage by decade 
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
       subtitle = "Number of films released per decade that have been rated") +
  theme(axis.ticks.x = element_line(colour = "#333333"),
        axis.ticks.length =  unit(0.26, "cm")) 

#Distribution of ratings by Decade
edx_categories %>% mutate(decade = year(floor_date(ISOdate(release_year, 1, 1), unit = "10 years"))) %>%
  group_by(decade) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(x=factor(decade), y=mean_rating)) +
  geom_col(fill="#1380A1") +
  bbc_style() +
  labs(title="Avg: Ratings by Decade",
       subtitle = "Average rating received for movies by release date") +
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

#Distribution of ratings by Decade
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
       subtitle = "Number of ratings by Hour and Day of the week")

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
       subtitle = "Number of ratings by Hour and Day of the week")

#Display the distribution by the Day of the Week and Month of the rating that was provided by the user.
edx_categories %>% group_by(rating_month, rating_day) %>%
  summarise(mean_rating = mean(rating),
            num_ratings = n()) %>%
  ggplot(aes(x=as.factor(rating_month), y=as.factor(rating_day), fill=num_ratings)) +
  geom_tile() +
  scale_fill_continuous(name="#Ratings") +
  xlab("Month")+
  ylab("Calendar Day")+
  labs(title="Thanksgiving?",
       subtitle = "Number of ratings by Month and Day")

#Distribution of ratings by Movie
edx_categories %>% group_by(category, rating, movieId) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(x=mean_rating)) +
  geom_density(alpha = .3)

#Distribution of Ratings
options(scipen=10000)     #Stop ggplot2 using abbreviations for labels
edx_categories %>% 
  ggplot(aes(rating)) + 
  geom_histogram(fill="#1380A1", binwidth = 0.5) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  xlab("Movie Rating") +
  labs(title="Distribution of Ratings") + 
  bbc_style()

#Distribution of Average Movie ratings
#The distribution of edx_categories and edx was compared to verify that the distribution was similar when the genres where split out to specific categories
edx_categories %>% group_by(movieId) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) + 
  geom_histogram(fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  xlab("Movie Rating") +
  labs(title="Distribution of Average Movie Ratings") + 
  bbc_style()

#Distribution of Average Movie ratings
#The distribution of edx_categories and edx was compared to verify that the distribution was similar when the genres where split out to specific categories
edx_categories %>% group_by(movieId, category) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating, fill = category)) + 
  geom_density(alpha = .3) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  xlab("Movie Rating") +
  labs(title="Distribution of Average Movie Ratings") + 
  bbc_style()

#Average rating by category
#The distribution of edx_categories and edx was compared to verify that the distribution was similar when the genres where split out to specific categories
edx_categories %>% group_by(movieId, category) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating, fill = category)) + 
  geom_density(alpha = .3) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  xlab("Movie Rating") +
  labs(title="Distribution of Average Movie Ratings") + 
  bbc_style()


#ANSEO% of ratings at the weekend (Friday, Saturday, Sunday)

#Analyse films that have been remade and compare the ratings.
remakes <- edx_categories %>% group_by(film, release_year) %>%
  summarise(ratings_count = n(),
            mean_rating = mean(rating)) %>%
  group_by(film) %>%
  filter(n()>1) 

remakes %>% ggplot(aes(x = film, y = mean_rating, label = release_year, size=ratings_count)) +
  geom_point(col="#1380A1") +
  coord_flip() +
  geom_text(size = 2, position = position_dodge(width = 1), vjust = 2.2) +
  xlab("Average Movie Rating")+
  ylab("Remakes")+
  labs(title="Movie Remakes",
       subtitle = "How do the ratings of the remakes compare?")

#On average, does the remake tend to get better results?
#find the year with the minimum mean_rating and the year with the maximum
#Check which is better, the earlier or later version
#Flag the result and highlight in a dumbell. 
dubbell_edx <- remakes %>% 
  group_by(film) %>%
  mutate(version = ifelse(release_year == min(release_year), "original", ifelse(release_year == max(release_year), "latest", "skip"))) %>%
  filter(version %in% c("original","latest")) %>%
  select(film, mean_rating, version) %>%
  spread(version, mean_rating) %>%
  mutate(difference = latest-original) 


#Use a dumbbell_geom() to compare the ratings of original versions and the latest remake
dubbell_edx %>% 
  ggplot(aes(x = original, xend = latest, y = film)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 2,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1",
                dot_guide=TRUE, dot_guide_size=0.25,
                show.legend = TRUE) +
  geom_vline(xintercept = mean(remakes$mean_rating), color="blue") +
  annotate("text", x = 4.05, y = 37, vjust = 0.5, hjust = 0, label = "Original", size=4, colour="#FAAB18") +
  annotate("text", x = 4.05, y = 35, vjust = 0.5, hjust = 0, label = "Last Remake", size=4, colour="#1380A1") +
  xlab("Average Movie Rating")+
  bbc_style() +
  theme(panel.grid.major.x = element_line(color="#cbcbcb"), 
        panel.grid.major.y = element_line(FALSE),
        axis.line.x = element_line(colour = "#333333"),
        axis.line.y = element_line(colour = "#333333"),
        axis.text = element_text(size = 9),
        axis.ticks.x = element_line(colour = "#333333"),
        axis.ticks.length =  unit(0.26, "cm")) +
  labs(title="Rebooted",
       subtitle="How did the latest version compare to the original?")         

#Use a dumbbell_geom() to compare the remake successed
dubbell_edx %>% filter(latest > original) %>%
  ggplot(aes(x = original, xend = latest, y = film)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 2,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1",
                dot_guide=TRUE, dot_guide_size=0.25,
                show.legend = TRUE) +
  geom_vline(xintercept = mean(remakes$mean_rating), color="blue") +
  xlab("Average Movie Rating")+
  bbc_style() +
  theme(panel.grid.major.x = element_line(color="#cbcbcb"), 
        panel.grid.major.y = element_line(FALSE),
        axis.line.x = element_line(colour = "#333333"),
        axis.line.y = element_line(colour = "#333333"),
        axis.text = element_text(size = 9),
        axis.ticks.x = element_line(colour = "#333333"),
        axis.ticks.length =  unit(0.26, "cm")) +
  labs(title="Rebooted Successfully!",
       subtitle="The Remake has better ratings than the original")     

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
