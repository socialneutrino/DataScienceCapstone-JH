unitest <- sampleNgm$unigrams[1:50000]
unigramdf <- data.frame(unigrams = names(unitest), freq = as.numeric(unitest))

bitest <- sampleNgm$bigrams[1:50000]
bigramdf <- data.frame(bigrams = names(bitest), freq = as.numeric(bitest))

#bigramdf <- within(bigramdf, bigrams<-data.frame(do.call('rbind', strsplit(as.character(bigrams), '_', fixed=TRUE))))
library(tidyr)
bigramdf <- separate(bigramdf, bigrams, c("word1", "word2"), "_")

tritest <- sampleNgm$trigrams[1:50000]
trigramdf <- data.frame(trigrams = names(tritest), freq = as.numeric(tritest))
trigramdf <- separate(trigramdf, trigrams, c("word1", "word2", "word3"), "_")

quadtest <- sampleNgm$quadgrams[1:50000]
quadgramdf <- data.frame(quadgrams = names(quadtest), freq = as.numeric(quadtest))
quadgramdf <- separate(quadgramdf, quadgrams, c("word1", "word2", "word3", "word4"), "_")

pred1 <- rep(NA, length(unigramdf$unigrams))
pred2 <- rep(NA, length(unigramdf$unigrams))
pred3 <- rep(NA, length(unigramdf$unigrams))
pred4 <- rep(NA, length(unigramdf$unigrams))
pred5 <- rep(NA, length(unigramdf$unigrams))
pred1_prob <- rep(NA, length(unigramdf$unigrams))
pred2_prob <- rep(NA, length(unigramdf$unigrams))
pred3_prob <- rep(NA, length(unigramdf$unigrams))
pred4_prob <- rep(NA, length(unigramdf$unigrams))
pred5_prob <- rep(NA, length(unigramdf$unigrams))
unigramProbs <- cbind(unigramdf, pred1, pred1_prob, 
                      pred2, pred2_prob, 
                      pred3, pred4_prob, 
                      pred4, pred4_prob, 
                      pred5, pred5_prob)

findNextWord <- function(df, x){
    nextWords <- df[df$word1 == x, 2:3]
    n <- length(nextWords[,1])
    totalInstances <- sum(nextWords[,2])
    if(n < 5){
      a <- data.frame(word2 = rep(NA, 5 - n), freq = rep(NA, 5 - n))
      nextWords <- rbind(nextWords, a)
    }
    nextWords <- dplyr::mutate(nextWords, prob = freq / totalInstances)
    return(nextWords[1:5,])
  }

x <- data.frame()
for (i in seq_along(unigramdf$unigrams)) {
    possibleWords = findNextWord(bigramdf, unigramdf$unigrams[i])
    x[i, 1] = possibleWords[1,1]
    x[i, 2] <- possibleWords[1,3]
    x[i, 3] <- possibleWords[2,1]
    x[i, 4] <- possibleWords[2,3]
    x[i, 5] <- possibleWords[3,1]
    x[i, 6] <- possibleWords[3,3]
    x[i, 7] <- possibleWords[4,1]
    x[i, 8] <- possibleWords[4,3]
    x[i, 9] <- possibleWords[5,1]
    x[i, 10] <- possibleWords[5,3]
  }

unigramdf <- cbind(unigramdf, x)

predBigram <- function(df, x, y){
  nextWords <- df[df$word1 == x & df$word2 == y, 3:4]
  n <- length(nextWords[,1])
  totalInstances <- sum(nextWords[,2])
  if(n < 5){
    a <- data.frame(word3 = rep(NA, 5 - n), freq = rep(NA, 5 - n))
    nextWords <- rbind(nextWords, a)
  }
  nextWords <- dplyr::mutate(nextWords, prob = freq / totalInstances)
  return(nextWords[1:5,])
}

x <- data.frame()
for (i in seq_along(bigramdf$word1)) {
  possibleWords = predBigram(trigramdf, bigramdf$word1[i], bigramdf$word2[i])
  x[i, 1] = possibleWords[1,1]
  x[i, 2] <- possibleWords[1,3]
  x[i, 3] <- possibleWords[2,1]
  x[i, 4] <- possibleWords[2,3]
  x[i, 5] <- possibleWords[3,1]
  x[i, 6] <- possibleWords[3,3]
  x[i, 7] <- possibleWords[4,1]
  x[i, 8] <- possibleWords[4,3]
  x[i, 9] <- possibleWords[5,1]
  x[i, 10] <- possibleWords[5,3]
}

bigram_next_words <- cbind(bigramdf, x)

predTrigram <- function(df, x, y, z){
  nextWords <- df[df$word1 == x & df$word2 == y & df$word3 == z, 4:5]
  n <- length(nextWords[,1])
  totalInstances <- sum(nextWords[,2])
  if(n < 5){
    a <- data.frame(word4 = rep(NA, 5 - n), freq = rep(NA, 5 - n))
    nextWords <- rbind(nextWords, a)
  }
  nextWords <- dplyr::mutate(nextWords, prob = freq / totalInstances)
  return(nextWords[1:5,])
}

x <- data.frame()
for (i in seq_along(trigramdf$word1)) {
  possibleWords = predTrigram(quadgramdf, 
                             trigramdf$word1[i], 
                             trigramdf$word2[i], 
                             trigramdf$word3[i])
  x[i, 1] = possibleWords[1,1]
  x[i, 2] <- possibleWords[1,3]
  x[i, 3] <- possibleWords[2,1]
  x[i, 4] <- possibleWords[2,3]
  x[i, 5] <- possibleWords[3,1]
  x[i, 6] <- possibleWords[3,3]
  x[i, 7] <- possibleWords[4,1]
  x[i, 8] <- possibleWords[4,3]
  x[i, 9] <- possibleWords[5,1]
  x[i, 10] <- possibleWords[5,3]
}

trigram_next_words <- cbind(trigramdf, x)
