#Function to remove "â€*"-like tokens and replace them with punctuation
to.plain <- function(s) {
  old2 <- c('â€œ', 'â€', 'â€™', 'â€˜', 'â€”', 'â€“', 'â€¢', 'â€¦')
  new2 <- c('“', '”', '’', '‘', '–', '—', '-', '…')
  for(i in seq_along(old2)) s <- gsub(old2[i], new2[i], s, fixed = TRUE)
  s
}