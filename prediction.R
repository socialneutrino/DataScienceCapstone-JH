#textInput <- "one of the ways in which this does not work is"

trigram_next_words <- readRDS("trigram_next_words.rds")
bigram_next_words <- readRDS("bigram_next_words.rds")
unigramdf <- readRDS("unigram_next_words.rds")
top5 <- unigramdf[1:5,1:2]
top5$freq <- top5$freq / sum(unigramdf$freq)
names(top5)[names(top5)=="freq"] <- "prob"

# Find the number of words in the phrase
numberWords <- sapply(gregexpr("[[:alpha:]]+", textInput), function(x) sum(x > 0))


processPhrase <- function(textInput) {
  numberWords <- sapply(gregexpr("[[:alpha:]]+", textInput), function(x) sum(x > 0))
  processedInput <- strsplit(textInput, " ")[[1]]
  if (length(processedInput) <= 3) {
    phrase <- processedInput
  } else if (length(processedInput > 3)){
    phrase <- processedInput[(numberWords-2):numberWords]
  }
  return(phrase)
}

# Define prediction functions
wordPredBigram <- function (first, second){
  searchBigram <- paste(first, second, sep = "_")
  if (!searchBigram %in% bigram_next_words$bigram) {
    return(0)
    break
  }
  found <- subset(bigram_next_words, word1 == first & word2 == second)
  return(found)
}

wordPredTrigram <- function (first, second, third){
  searchTrigram <- paste(first, second, third, sep = "_")
  if (!searchTrigram %in% trigram_next_words$trigram) {
    return(0)
    break
  }
  found <- subset(trigram_next_words, word1 == first & word2 == second & word3 == third)
  return(found)
}

wordPredUnigram <- function (word){
  searchUnigram <- paste(word)
  if (!searchUnigram %in% unigramdf$unigrams) {
    return(0)
    break
  }
  found <- sampleNgm$bigrams[grepl(searchUnigram, names(sampleNgm$bigrams))]
  found <- subset(unigramdf, unigrams == word)
  return(found)
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
  } else if (n == 2){
    prediction <- wordPredBigram(predictPhrase[1], predictPhrase[2])
    if (prediction == 0){
      prediction <- wordPredUnigram(predictPhrase[2])
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

predictWord <- function(textInput){
  ngrams <- processPhrase(textInput)
  probs <- nextWordProb(ngrams)
  if (probs == "No words found"){
      return(top5)
  }
  probs <- matrix(probs[, grepl( "V", names(probs))])
  probs <- probs[!is.na(probs)]
  probs <- cbind(probs[!mapply(is.numeric, probs)], probs[mapply(is.numeric, probs)])
  return(probs)
}
