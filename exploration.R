#Create unigram freq table
total_sample <- c(blog_sample, twitter_sample, news_sample)
unigrams <- makeTokens(total_sample)

#Create bigram freq table
a2 <- bigrams(total_sample)
a2 <- dfm(a2)
a2 <- colSums(sort(a2))
bigrams_freq <- a2[grepl("[A-Za-z]+_[A-Za-z]+", names(a2))]

#Create trigram freq table
a3 <- trigrams(total_sample)
a3 <- dfm(a3)
a3 <- colSums(sort(a3))
trigrams_freq <- a3[grepl("[A-Za-z]+_[A-Za-z]+_[A-Za-z]+", names(a3))]
















































library(openNLP)
require("NLP")

## Some text
s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
             "nonexecutive director Nov. 29.\n",
             "Mr. Vinken is chairman of Elsevier N.V., ",
             "the Dutch publishing group."),
           collapse = "")
s <- as.String(s)


## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
pos_tag_annotator
a3 <- annotate(s, pos_tag_annotator, a2)
a3


## Variant with POS tag probabilities as (additional) features.
head(annotate(s, Maxent_POS_Tag_Annotator(probs = TRUE), a2))


## Determine the distribution of POS tags for word tokens.
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")
tags
table(tags)


## Extract token/POS pairs (all of them): easy.
sprintf("%s/%s", s[a3w], tags)

## Extract pairs of word tokens and POS tags for second sentence:
a3ws2 <- annotations_in_spans(subset(a3, type == "word"),
                              subset(a3, type == "sentence")[2L])[[1L]]

sprintf("%s/%s", s[a3ws2], sapply(a3ws2$features, `[[`, "POS"))

library(tm)
blog_corpus <- VCorpus(VectorSource(blog_sample))

