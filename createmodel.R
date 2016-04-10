library(quanteda)
total_sample <- c(blogs_sample, twitter_sample, news_sample)
sampleNgm <- createNgrams(total_sample)

unitest <- sampleNgm$unigrams[1:25000]
unigramdf <- data.frame(unigrams = names(unitest), freq = as.numeric(unitest))

bitest <- sampleNgm$bigrams[1:25000]
bigramdf <- data.frame(bigrams = names(bitest), freq = as.numeric(bitest))

#bigramdf <- within(bigramdf, bigrams<-data.frame(do.call('rbind', strsplit(as.character(bigrams), '_', fixed=TRUE))))
library(tidyr)
bigramdf <- separate(bigramdf, bigrams, c("word1", "word2"), "_")

tritest <- sampleNgm$trigrams[1:25000]
trigramdf <- data.frame(trigrams = names(tritest), freq = as.numeric(tritest))
trigramdf <- separate(trigramdf, trigrams, c("word1", "word2", "word3"), "_")

quadtest <- sampleNgm$quadgrams[1:25000]
quadgramdf <- data.frame(quadgrams = names(quadtest), freq = as.numeric(quadtest))
quadgramdf <- separate(quadgramdf, quadgrams, c("word1", "word2", "word3", "word4"), "_")

ngramList <- list(unitest,bitest,tritest, quadtest)


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


ptm <- proc.time()
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
proc.time() - ptm

#add ngram token at the end for search purpose
bigram_next_words <- dplyr::mutate(bigram_next_words, bigram = paste(word1, word2, sep="_"))
trigram_next_words <- dplyr::mutate(trigram_next_words, trigram = paste(word1, word2, word3, sep="_"))

#subset with predicted next words only by removing NAs for first word
unigramdf <- subset(unigramdf, !is.na(V1))
bigram_next_words <- subset(bigram_next_words, !is.na(V1))
trigram_next_words <- subset(trigram_next_words, !is.na(V1))


saveRDS(trigram_next_words, "trigram_next_words.rds")
saveRDS(bigram_next_words, "bigram_next_words.rds")
saveRDS(unigramdf, "unigram_next_words.rds")

##Grepl method
#tritest2 <- data.frame(trigrams = names(tritest), freq = as.numeric(tritest))
#quadtest2 <- data.frame(quadgrams = names(quadtest), freq = as.numeric(quadtest))
# predTrigram <- function (trigram){
#   trigram <- paste("^", trigram, "_", sep="")
#   results <- quadtest2[grepl(trigram, quadtest2$quadgrams),]
#   if (length(results$quadgrams) > 5){
#     results <- results[1:5,]
#   }
#   return(results)
# }
# 
# predBigram <- function (bigram){
#   bigram <- paste("^", bigram, "_", sep="")
#   results <- tritest2[grepl(bigram, tritest2$trigrams),]
#   if (length(results$trigrams) > 5){
#     results <- results[1:5,]
#   }
#   return(results)
# }
# 
# predUnigram <- function (unigram){
#   unigram <- paste("^", unigram, "_", sep="")
#   results <- bitest2[grepl(unigram, biitest2$bigrams),]
#   if (length(results$bigrams) > 5){
#     results <- results[1:5,]
#   }
#   return(results)
# }