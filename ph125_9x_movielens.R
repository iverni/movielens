####Load packages and Libraries----
install.packages('devtools')
devtools::install_github('bbc/bbplot')    #Load the BBC 
#This line of code installs the pacman page if you do not have it installed - if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")
pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')
#Data Import
library('data.table')
#Visualization
library('ggplot2')  # visualization
#Data Manipulation
library('tidyverse')
library('dplyr')   # data manipulation
library('tidyr')   # data manipulation
library('stringr') # data manipulation
library('purrr')   # Map and nest functions
#Model
library('randomForest') # classification algorithm
library('broom')        # Tidies data from linear regression  
#Other Libraries
library(tictoc)       # Performance Analysis 
library(microbenchmark) # Performance Analysis  
library('mice')       # imputation
library(writexl)      #Export to Excel
library(openintro)
library(svDialogs)    # Enable text input through pop up table
#Output
library(shiny)


####Data Retrieval----
#The data files should be included in a subfolder /data
rm(movies)
rm(ratings)
get_files <- function(load_file){
  folder_file <- paste0(getwd(),"/data",load_file)
  filelines <- readLines(folder_file)
  filelines <- gsub("::", "\t", filelines)
  fread(paste(filelines, collapse="\n"), sep="\t") 
}
movies <- get_files("/movies.dat")
ratings <- get_files("/ratings.dat")
summary(ratings)

# next step - Add headings to the tables
# Write a function that checks each table for NA, incorrect variables, data inaccuracies
# 


