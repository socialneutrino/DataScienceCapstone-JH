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
txt <- to.plain(blog_sample[4:6])
tokenize(txt, removePunct = TRUE)
removeFeatures(tokenize(txt, removePunct = TRUE), stopwords("english"))
tokenize(txt, removePunct = TRUE, ngrams = 2)
tokenize(txt, removePunct = TRUE, ngrams = 1:2)
tokenize(txt, removePunct = TRUE, ngrams = 2, skip = 1, concatenator = " ")
removeFeatures(tokenize(txt, removePunct = TRUE, ngrams = 1:2), stopwords("english"))
