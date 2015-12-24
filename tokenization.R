b <- blog_sample[1]
b <- to.plain(b)
tokens <- unlist(strsplit(b, "[^A-Za-z]"))


makeTokens <- function(s) {
  s <- to.plain(s)
  s <- unlist(strsplit(s, "[^A-Za-z]"))
  sort(table(s), decreasing = T)
}

makeTokens()