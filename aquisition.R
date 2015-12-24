con <- file("en_US.twitter.txt", "r") 
readLines(con, 1) 
## Read the first line of text 
readLines(con, 1) ## Read the next line of text 
readLines(con, 5) ## Read in the next 5 lines of text 
close(con) ## It's important to close the connection when you are done

con_blog <- file("Coursera-SwiftKey/final/en_US/en_US.blogs.txt", "r")
con_news <- file("Coursera-SwiftKey/final/en_US/en_US.news.txt", "r")
con_twitter <- file("Coursera-SwiftKey/final/en_US/en_US.twitter.txt", "r")

n_blogs <- sapply(blogs, nchar)
n_news <- sapply(news, nchar)
n_twitter <- sapply(news, nchar)

length(grep("love", twitter))/length(grep("hate", twitter))