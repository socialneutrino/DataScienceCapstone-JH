##Tokenize with words DIY function
makeTokens <- function(s) {
  s <- to.plain(s)
  s <- unlist(strsplit(tolower(s), "[^A-Za-z]"))
  s <- sort(table(s), decreasing = T)
  s <- s[-grep("^$", names(s))]
}


##Using quanteda pakage to
#txt <- to.plain(blog_sample)
#tokenize(txt, removePunct = TRUE)
#removeFeatures(tokenize(txt, removePunct = TRUE), stopwords("english"))
#tokenize(txt, removePunct = TRUE, ngrams = 2)
#tokenize(txt, removePunct = TRUE, ngrams = 1:2)
#tokenize(txt, removePunct = TRUE, ngrams = 2, skip = 1, concatenator = " ")
#removeFeatures(tokenize(txt, removePunct = TRUE, ngrams = 1:2), stopwords("english"))


#bigrams function
bigrams <- function (s){
  s <- tolower(s)
  old2 <- c('â€œ', 'â€', 'â€™', 'â€˜', 'â€”', 'â€“', 'â€¢', 'â€¦')
  new2 <- c('“', '”', '’', '‘', '–', '—', '-', '…')
  for(i in seq_along(old2)) s <- gsub(old2[i], new2[i], s, fixed = TRUE)
  s <- unlist(tokenize(s, removePunct = TRUE, ngrams = 2))
  s <- dfm(s)
  s <- colSums(sort(s))
  s[grepl("[A-Za-z]+_[A-Za-z]+", names(s))]
}

#trigrams function VERSION 1
trigrams <- function (s){
  s <- tolower(s)
  old2 <- c('â€œ', 'â€', 'â€™', 'â€˜', 'â€”', 'â€“', 'â€¢', 'â€¦')
  new2 <- c('“', '”', '’', '‘', '–', '—', '-', '…')
  for(i in seq_along(old2)) s <- gsub(old2[i], new2[i], s, fixed = TRUE)
  s <- unlist(tokenize(s, removePunct = TRUE, ngrams = 3))
  s <- dfm(s)
  s <- colSums(sort(s))
  s[grepl("[A-Za-z]+_[A-Za-z]+_[A-Za-z]+", names(s))]
}
