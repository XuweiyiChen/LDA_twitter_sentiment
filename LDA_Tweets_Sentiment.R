library(dplyr)
library(tidytext)
library(tidyverse)
library(tm)
library(stringr)

# balance unfair 0 and 1
data <- read.csv("./train_tweets.csv")
dist <- hist(data$label)
preprocess_data <- select(data, label, tweet)
glimpse(preprocess_data)
preprocess_data_1 <- filter(preprocess_data, preprocess_data$label == 1)
dim(preprocess_data_1)
preprocess_data_0 <- filter(preprocess_data, preprocess_data$label == 0)
dim(preprocess_data_0)
preprocess_data_0 <- preprocess_data_0[1:2242, ]
dim(preprocess_data_0)

train_set <- rbind(preprocess_data_0, preprocess_data_1)
set.seed(499)
sample_rows <- sample(nrow(train_set))
shuffle_train_set <- train_set[sample_rows, ]

# pre-processing
shuffle_train_set$tweet <- gsub("#", "", shuffle_train_set$tweet)
shuffle_train_set$tweet <- gsub("@", "", shuffle_train_set$tweet)

shuffle_train_set$tweet <- gsub('[[:digit:]]+', '', shuffle_train_set$tweet)
shuffle_train_set$tweet <- gsub('[[:punct:]]+', '', shuffle_train_set$tweet)
shuffle_train_set$tweet = removeWords(shuffle_train_set$tweet, stopwords("english"))
shuffle_train_set$tweet = stripWhitespace(shuffle_train_set$tweet)

# model building
#create DTM
dtm <- CreateDtm(shuffle_train_set$tweet, 
                 doc_names = tokens$ID, 
                 ngram_window = c(1, 2))

