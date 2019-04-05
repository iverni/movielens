####Load packages and Libraries----
#This line of code installs the pacman package. This contains tools to more conveniently perform
#tasks associated with add-on packages. 
if(!require(pacman))install.packages("pacman")
pacman::p_load('data.table',                              # Data Importation
               'tidyverse', 'dplyr', 'tidyr', 'stringr',  # Data Manipulation
               'ggplot2', 'bbplot',                       # Visualisation 
               'caret',                                   # Classification and Regression 
               'tictoc'                                   # Performance measuring
)

if(!require(devtools))install.packages('devtools')
devtools::install_github('bbc/bbplot')    #Load the BBC plots for use with ggplot2

####Download from MovieLens----
# Check if the file has already been downloaded to the working directory and only download one time.
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
#
dir_file_ratings <- "ml-10M100K/ratings.dat"   
dir_file_movies <- "ml-10M100K/movies.dat"
if(!file.exists(dir_file_ratings) || !file.exists(dir_file_movies)) {
  tic("Download from GroupLens")
  dl_file <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl_file)
  toc()
  tic("Prepare ratings")
  ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl_file, dir_file_ratings))),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
  toc()
  tic("Prepare movies")
  movies <- str_split_fixed(readLines(unzip(dl_file, dir_file_movies)), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                             title = as.character(title),
                                             genres = as.character(genres))
  toc()
} else {
  print("Files already exist and do not need to be downloaded again")
  tic("Prepare ratings")
  ratings <- read.table(text = gsub("::", "\t", readLines(dir_file_ratings)),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
  toc()
  tic("Prepare movies")
  movies <- str_split_fixed(readLines(dir_file_movies), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                             title = as.character(title),
                                             genres = as.character(genres))
  toc()
}
movielens <- left_join(ratings, movies, by = "movieId")

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

####Data Preparation----
summary(edx)
table(edx$rating)
hist(edx$rating)
n_distinct(edx$movieId)
n_distinct(edx$userId)

# Count for 4 specific categories
edx_by_genre <- edx %>%
  mutate(category = genres) %>% 
  separate_rows(category, sep ="[|]") %>% 
  filter(category %in% c("Drama", "Comedy", "Thriller", "Romance")) %>%
  group_by(category) %>%
  summarise(total = n())

#### Which movie has the greatest number of ratings?
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#What are the five most given ratings in order from most to least?
edx_ratings_count <- edx %>% group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#True or False: In general, half star ratings are less common than whole star ratings 
#(e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).
edx_ratings_count %>% mutate(mod = edx_ratings_count$rating %% 1) %>%
  group_by(mod) %>%
  summarise(total = sum(count))

