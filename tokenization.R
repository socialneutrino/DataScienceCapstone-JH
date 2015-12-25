##Tokenize with words DIY function
makeTokens <- function(s) {
  s <- to.plain(s)
  s <- unlist(strsplit(tolower(s), "[^A-Za-z]"))
  s <- sort(table(s), decreasing = T)
  s <- s[-grep("^$", names(s))]
}

tokens <- makeTokens(blog_sample)
head(tokens)

##Using quanteda pakage to
txt <- to.plain(blog_sample)
tokenize(txt, removePunct = TRUE)
removeFeatures(tokenize(txt, removePunct = TRUE), stopwords("english"))
tokenize(txt, removePunct = TRUE, ngrams = 2)
tokenize(txt, removePunct = TRUE, ngrams = 1:2)
tokenize(txt, removePunct = TRUE, ngrams = 2, skip = 1, concatenator = " ")
removeFeatures(tokenize(txt, removePunct = TRUE, ngrams = 1:2), stopwords("english"))

head(blog_sample)

bigrams <- function (s){
  s <- tolower(s)
  old2 <- c('â€œ', 'â€', 'â€™', 'â€˜', 'â€”', 'â€“', 'â€¢', 'â€¦')
  new2 <- c('“', '”', '’', '‘', '–', '—', '-', '…')
  for(i in seq_along(old2)) s <- gsub(old2[i], new2[i], s, fixed = TRUE)
  s <- unlist(tokenize(s, removePunct = TRUE, ngrams = 2, concatenator = " "))
  s
}

trigrams <- function (s){
  s <- tolower(s)
  old2 <- c('â€œ', 'â€', 'â€™', 'â€˜', 'â€”', 'â€“', 'â€¢', 'â€¦')
  new2 <- c('“', '”', '’', '‘', '–', '—', '-', '…')
  for(i in seq_along(old2)) s <- gsub(old2[i], new2[i], s, fixed = TRUE)
  s <- unlist(tokenize(s, removePunct = TRUE, ngrams = 3))
  s
}

blog_bigrams <- bigrams(blog_sample)
dfm_blog <- dfm(blog_bigrams)

#head(dfm_blog@Dimnames$features)
blog_freq <- colSums(sort(dfm_blog))


blog_trigrams <- trigrams(blog_sample)
dfm_blog_tri <- dfm(blog_trigrams)

#head(dfm_blog@Dimnames$features)
blog_freq_tri <- colSums(sort(dfm_blog_tri))


##using tm package to tokenize
library("tm")
data("crude")

BigramTokenizer <- function(x) {
unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
  
tdm <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))
inspect(removeSparseTerms(tdm[, 1:10], 0.7))
