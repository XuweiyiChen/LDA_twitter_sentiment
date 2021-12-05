library(dplyr)
library(tidytext)
library(tidyverse)
library(tm)
library(stringr)
library(rstan)
library(shinystan)

# balance unfair 0 and 1
data <- read.csv("train_tweets.csv")
preprocess_data_1 <- filter(data, data$label == 1)
dim(preprocess_data_1)
preprocess_data_0 <- filter(data, data$label == 0)
dim(preprocess_data_0)
preprocess_data_0 <- preprocess_data_0[1:2242, ]
dim(preprocess_data_0)

train_set <- rbind(preprocess_data_0, preprocess_data_1)
set.seed(499)
sample_rows <- sample(nrow(train_set))
shuffle_train_set <- train_set[sample_rows, ]
train_set = shuffle_train_set[1: 4000, ]
test_set = shuffle_train_set[4001:4484, ]
train_set$id = 1:4000
id = train_set$id

# pre-processing
train_set$tweet <- gsub("#", "", train_set$tweet)
train_set$tweet <- gsub("@", "", train_set$tweet)

train_set$tweet <- gsub('[[:digit:]]+', '', train_set$tweet)
train_set$tweet <- gsub('[[:punct:]]+', '', train_set$tweet)
train_set$tweet = removeWords(train_set$tweet, stopwords("english"))
train_set$tweet = stripWhitespace(train_set$tweet)
text_cleaning_tokens <- train_set %>%
  tidytext::unnest_tokens(word, tweet)

hist(text_cleaning_tokens$label)
dim(text_cleaning_tokens)

# model
K = 2 # num of topics
V = 37282 # num of words
M = 4000 # num docs
N = length(unique(text_cleaning_tokens$word))
w = unique(text_cleaning_tokens$word)
doc = array()
alpha = rep(1, 2)
beta = rep(1, V)
model <- stan_model('LDA_Model.stan')
list_words = list()
words = array()

for (i in 1:N) {
  if (w[i] %in% list_words) {
    index = match(w[i], list_words)
    words[i] = index
    print(1)
    print(w[i])
  } else {
    print(2)
    length_word = length(list_words) + 1
    print(w[i])
    list_words[length_word] = w[i]
    words[i] = length_word
  }
}

for (i in 1:N) {
  index = match(list_words[i], text_cleaning_tokens$word)
  doc_id = text_cleaning_tokens[index, ]$id
  print(index)
  print(doc_id)
  doc[i] = doc_id
}

# fit
options(mc.cores=2)
fit <- sampling(model, list(K=K, V=V, M=M, N=N, w=words, doc=doc, alpha=alpha, beta=beta), iter=200, chains=4)
