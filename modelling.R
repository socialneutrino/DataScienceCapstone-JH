library(sqldf)
source("tokenization.R")

testunigrams <- makeTokens(testcorpus)

createNgrams <- function(c){
    uni <- makeTokens(c)
    bi <- bigrams(c)
    tri <- trigrams(c)
    quad <- quadgrams(c)
    ngm <- list(unigrams = uni, bigrams = bi, trigrams = tri, quadgrams = quad)
}

bigramMatrix <- function(table) {
    a <- names(table)
}

fnames <- gsub("(_.*)$", "", bigramtest)

pieces <- sapply(strsplit(bigramtest,"_"), "[", 1)

uni_mod <- unigrams[1:1000]
bi_mod <- bigrams_freq[1:6000]
tri_mod <- trigrams_freq[1:10000]

bigramProb <- function (word, prev1){
  bigram <- paste(word, prev1, sep="_")
  prob <- sampleNgm$bigrams[bigram] / sampleNgm$unigrams[word]
  return(prob)
}

trigramProb <- function (word, prev1, prev2){
  trigram <- paste(word, prev1, prev2, sep="_")
  bigram <- paste(word, prev1, sep = "_")
  prob <- sampleNgm$trigrams[trigram] / sampleNgm$bigrams[bigram]
  return(prob)
}

quadgramProb <- function (word, prev1, prev2, prev3){
  quadgram <- paste(word, prev1, prev2, prev3, sep="_")
  trigram <- paste(word, prev1, prev2, sep = "_")
  prob <- sampleNgm$quadgrams[quadgram] / sampleNgm$trigrams[trigram]
  return(prob)
}

nextWordProb <- function (word, ...){
  #find how many words there are previously
  print(c(...))
  previousPhrase <- c(...)
  n <- length(previousPhrase)
  print(n)
  if (n == 1){
    prob <- bigramProb(..., word)
  } else if (n ==2){
    prob <- trigramProb(..., word)
  } else{
    prob <- quadgramProb(..., word)
  }
  return(prob)
}

pastetest <- function (...){
  s <- paste(..., sep="_")
  return(s)
}

as.integer(greeneggsNgm$bigrams[grep("on_a", names(greeneggsNgm$bigrams))])

nextWordProb("be", "insane")
nextWordProb("be", "callous")
nextWordProb("be", "asleep")
nextWordProb("be", "insensitive")

wordPredBigram <- function (first, second){
  searchBigram <- paste(first, paste(second, "_", sep=""), sep = "_")
  x <- sum(grepl(searchBigram, names(sampleNgm$trigrams)))
  if (x == 0) {
    return(0)
    break
  }
  found <- sampleNgm$trigrams[grepl(searchBigram, names(sampleNgm$trigrams))]
  prediction <- found[found == max(found)]
  names(prediction[1])
  }

wordPredTrigram <- function (first, second, third){
  searchTrigram <- paste(first, second, paste(third, "_", sep=""), sep = "_")
  x <- sum(grepl(searchTrigram, names(sampleNgm$quadgrams)))
  if (x == 0) {
    return(0)
    break
  }
  found <- sampleNgm$quadgrams[grepl(searchTrigram, names(sampleNgm$quadgrams))]
  prediction <- found[found == max(found)]
  names(prediction[1])
}

wordPredUnigram <- function (word){
  searchUnigram <- paste(word, "_", sep="")
  x <- sum(grepl(searchUnigram, names(sampleNgm$bigrams)))
  if (x == 0) {
    return(0)
    break
    }
  found <- sampleNgm$bigrams[grepl(searchUnigram, names(sampleNgm$bigrams))]
  prediction <- found[found == max(found)]
  names(prediction[1])
}

nextWordProb <- function (...){
    #find how many words there are previously
    predictPhrase <- c(...)
    n <- length(predictPhrase)
    if (n == 1){
      prediction <- wordPredUnigram(predictPhrase)
      if (prediction == 0){
        prediction <- "No words found"
      }
    } else if (n ==2){
      prediction <- wordPredBigram(predictPhrase[1], predictPhrase[2])
      if (prediction == 0){
        prediction <- wordPredBigram(predictPhrase[2])
        if (prediction == 0){
          prediction <- "No words found"
        }
      }
    } else if (n ==3){
      prediction <- wordPredTrigram(predictPhrase[1], predictPhrase[2], predictPhrase[3])
      if (prediction == 0){
        prediction <- wordPredBigram(predictPhrase[2], predictPhrase[3])
        if(prediction == 0){
          prediction <- wordPredUnigram(predictPhrase[3])
          if(prediction == 0){
            prediction <- "No words found"
          }
        }
      }
    } else {
      print("not appropriate number of words")
      break
    }
    return(prediction)
  }



