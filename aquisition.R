con <- file("en_US.twitter.txt", "r") 
readLines(con, 1) 
## Read the first line of text 
readLines(con, 1) ## Read the next line of text 
readLines(con, 5) ## Read in the next 5 lines of text 
close(con) ## It's important to close the connection when you are done

#Open connections with main text samples
con_blog <- file("Coursera-SwiftKey/final/en_US/en_US.blogs.txt", "r")
con_news <- file("Coursera-SwiftKey/final/en_US/en_US.news.txt", "r")
con_twitter <- file("Coursera-SwiftKey/final/en_US/en_US.twitter.txt", "r")

#Load and open connections with smaller generated samples (see sample_create.pl)
con_blog_sample <- file("blogs_sample.txt", "r")
con_news_sample <- file("news_sample.txt", "r")
con_twitter_sample <- file("twitter_sample.txt", "r")
blogs_sample <- readLines(con_blog_sample) 
close(con_blog_sample)
news_sample <- readLines(con_news_sample) 
close(con_news_sample)
twitter_sample <- readLines(con_twitter_sample) 
close(con_twitter_sample)

#Find number of lines
n_blogs <- sapply(blogs, nchar)
n_news <- sapply(news, nchar)
n_twitter <- sapply(news, nchar)

length(grep("love", twitter))/length(grep("hate", twitter))