library(ggplot2)
library(quanteda)
library(dplyr)

#quick and dirty word count
sapply(gregexpr("\\W+", head(twitter_sample)), length) + 1


source("tokenization.R")

#Create unigram freq table for sample of ALL THREE corpora samples
total_sample <- c(blogs_sample, twitter_sample, news_sample)
unigrams <- makeTokens(total_sample)

#Create bigram freq table
bigrams_freq  <- bigrams(total_sample)

#Create trigram freq table
trigrams_freq <- trigrams(total_sample)


#Create quadgram freq table
quadgrams_freq <- quadgrams(total_sample)



#ngrams for BLOGS ONLY
#unigrams
blogs_unigrams <- makeTokens(blogs_sample)
#Create bigram freq table
blogs_bigrams <- bigrams(blogs_sample)
#Create trigram freq table
blogs_trigrams <- trigrams(blog_sample)


#ngrams for TWITTER ONLY
#unigrams
twitter_unigrams <- makeTokens(twitter_sample)
#Create bigram freq table
twitter_bigrams <- bigrams(twitter_sample)
#Create trigram freq table
twitter_trigrams <- trigrams(twitter_sample)


#ngrams for NEWS ONLY
#unigrams
news_unigrams <- makeTokens(news_sample)
#Create bigram freq table
news_bigrams <- bigrams(news_sample)
#Create trigram freq table
news_trigrams <- trigrams(news_sample)

library(dplyr)

#Create Cumulative Frequency Data Frame
unigrams_freq <- as.data.frame(unigrams) %>% 
    select(freq = 1) %>% 
    mutate(unigram = names(unigrams))

unigrams_freq$word_number <- 1:nrow(unigrams_freq)

unigrams_freq <- unigrams_freq %>% 
    mutate(cumsum = cumsum(freq)) %>%
    mutate(coverage = 100*cumsum/sum(freq))

#FUNCTION to Create Cumulative Frequency Data Frame
freq_df <- function(table) {
  a <- as.data.frame(table) %>% 
    select(freq = 1) %>% 
    mutate(word = names(table))
  
  a$word_number <- 1:nrow(a)
  
  a <- a %>% 
    mutate(cumsum = cumsum(freq)) %>%
    mutate(coverage = 100*cumsum/sum(freq))
  
  a
}

#TOTAL word and culumative frequency tables
unigrams_freq <- freq_df(unigrams)
bigrams_freq_df <- freq_df(bigrams_freq)
trigrams_freq_df <- freq_df(trigrams_freq)

#Plot of word number and cumulative percentage
gg <- ggplot(unigrams_freq, aes(x=word_number, y=coverage))
gg <- gg + geom_line(size=0.75)
gg <- gg + scale_x_continuous(limits=c(1, nrow(unigrams_freq)))
gg <- gg + theme_bw()
gg


#Blogs word coverage
blogs_unigrams_freq <- freq_df(blogs_unigrams)

gg <- ggplot(blogs_unigrams_freq, aes(x=word_number, y=coverage))
gg <- gg + geom_line(size=0.75)
gg <- gg + scale_x_continuous(limits=c(1, nrow(blogs_unigrams_freq)))
gg <- gg + theme_bw()
gg


#News word coverage
news_unigrams_freq <- freq_df(news_unigrams)

gg <- ggplot(news_unigrams_freq, aes(x=word_number, y=coverage))
gg <- gg + geom_line(size=0.75)
gg <- gg + scale_x_continuous(limits=c(1, nrow(news_unigrams_freq)))
gg <- gg + theme_bw()
gg


#Twitter word coverage
twitter_unigrams_freq <- freq_df(twitter_unigrams)

gg <- ggplot(twitter_unigrams_freq, aes(x=word_number, y=coverage))
gg <- gg + geom_line(size=0.75)
gg <- gg + scale_x_continuous(limits=c(1, nrow(twitter_unigrams_freq)))
gg <- gg + theme_bw()
gg

#Bigram coverage
gg <- ggplot(bigrams_freq_df, aes(x=word_number, y=coverage))
gg <- gg + geom_line(size=0.75)
gg <- gg + scale_x_continuous(limits=c(1, nrow(bigrams_freq_df)))
gg <- gg + theme_bw()
gg

#Trigram coverage
gg <- ggplot(trigrams_freq_df, aes(x=word_number, y=coverage))
gg <- gg + geom_line(size=0.75)
gg <- gg + scale_x_continuous(limits=c(1, nrow(trigrams_freq_df)))
gg <- gg + theme_bw()
gg

