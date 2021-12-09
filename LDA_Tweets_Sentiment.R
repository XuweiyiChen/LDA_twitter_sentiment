library(dplyr)
library(tidytext)
library(tidyverse)
library(tm)
library(stringr)
library(rstan)
library(shinystan)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)

# balance unfair 0 and 1
data <- read.csv("train_tweets.csv")
preprocess_data_1 <- filter(data, data$label == 1)
dim(preprocess_data_1)
preprocess_data_0 <- filter(data, data$label == 0)
dim(preprocess_data_0)
preprocess_data_0 <- preprocess_data_0[1:2242, ]
dim(preprocess_data_0)
preprocess_data_0 = preprocess_data_0[1:100, ]
preprocess_data_1 = preprocess_data_1[1:100, ]

train_set <- rbind(preprocess_data_0, preprocess_data_1)
set.seed(499)
sample_rows <- sample(nrow(train_set))
shuffle_train_set <- train_set[sample_rows, ]
train_set = shuffle_train_set
train_set$id = 1:200
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
V = 1930 # num of words
M = 200 # num docs
N = length(unique(text_cleaning_tokens$word))
w = unique(text_cleaning_tokens$word)
doc = array()
alpha = rep(1, 2)
beta = rep(1, V)
rstan_options(javascript = FALSE)
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
options(mc.cores=4)
fit <- sampling(model, list(K=K, V=V, M=M, N=N, w=words, doc=doc, alpha=alpha, beta=beta), iter=2000, chains=4)

# visualize
launch_shinystan(fit)

data = extract(fit, permuted = TRUE, inc_warmup = FALSE, include = TRUE)
Phi <- data$phi
dim(Phi)

# analysis
first_emo <- apply(Phi[,1,1:1132], 2, mean)
second_emo <- apply(Phi[,2,1:1132], 2, mean)
plot(first_emo)
plot(second_emo)

which.max(first_emo)
which.max(second_emo)

sort_first_emo <- sort(first_emo, index.return=TRUE, decreasing=TRUE)
top_five_first <- lapply(sort_first_emo, `[`, sort_first_emo$x %in% head(unique(sort_first_emo$x),6))
words_chosen_first = top_five_first$ix
words_chosen_first = words_chosen_first[-2]
words_first <- list_words[words_chosen_first]

sort_second_emo <- sort(second_emo, index.return=TRUE, decreasing=TRUE)
top_five_second <- lapply(sort_second_emo, `[`, sort_second_emo$x %in% head(unique(sort_second_emo$x),6))
words_chosen_second = top_five_second$ix
words_second <- list_words[words_chosen_second]
freq = 5:1

wordcloud(words = words_first, freq = freq, min.freq = 1, max.words=200,
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

wordcloud(words = words_second, freq = freq, min.freq = 1, max.words=200,
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

save(fit, file = "fit.Rdata")
