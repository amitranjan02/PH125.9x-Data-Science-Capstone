##########################################################################################################################
# HarvardX PH125.9x Data Science Capstone Project
#
##########################################################################################################################

##########################################################################################################################
# Install following packages if not installed on your IDE
# Developed using R 3.5.3
##########################################################################################################################

if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dslabs)) install.packages("dslabs")
if(!require(data.table)) install.packages("data.table")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(dslabs)
library(data.table)
library(ggrepel)
library(ggthemes)

######################################################################
# Create edx set and validation set
######################################################################

# Note: this process could take a couple of minutes

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

##########################################################################################################################
# Dataset Analysis
##########################################################################################################################

dim(edx)
# 9000055 X 6

anyNA(edx)
# [1] FALSE

head(edx)
# Data Summary
#userId movieId rating timestamp                         title                        genres
#1      1     122      5 838985046              Boomerang (1992)                Comedy|Romance
#2      1     185      5 838983525               Net, The (1995)         Action|Crime|Thriller
#4      1     292      5 838983421               Outbreak (1995)  Action|Drama|Sci-Fi|Thriller
#5      1     316      5 838983392               Stargate (1994)       Action|Adventure|Sci-Fi
#6      1     329      5 838983392 Star Trek: Generations (1994) Action|Adventure|Drama|Sci-Fi
#7      1     355      5 838984474       Flintstones, The (1994)       Children|Comedy|Fantasy
# 1- timestamp needs to be converted to date time
# 2 - Title / Year needs to be extracted into two column
# 3 - genres is a multivalue fiield with pipe seperated value. Needs transformation

summary(edx)

#How many unique users and unique movies are in dataset

edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))
# n_users: 69878 n_movies: 10677

unique(edx$rating)
# 5.0 3.0 2.0 4.0 4.5 3.5 1.0 1.5 2.5 0.5

unique(edx$genres)
# Returns 797 unique combinations. Needs refactoring

# Movies rating distribution visulization

edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 500, binwidth=0.1, color="green", show.legend = FALSE, aes(fill = cut(n, 50))) + 
  scale_x_log10() + 
  ggtitle("Rated Movies")

# Rating distribution for test segment
edx %>% 
  ggplot(aes(rating)) + 
  geom_histogram(binwidth=0.2, color="darkblue", fill="lightblue") + 
  ggtitle("Rating Distribution (Training")

# Rating distribution for validation segment

validation %>% 
  ggplot(aes(rating)) + 
  geom_histogram(binwidth=0.2, color="darkblue", fill="lightblue") +  
  ggtitle("Rating Distribution (Validation")

# Both test and validation segment have very similar rating distribution.

