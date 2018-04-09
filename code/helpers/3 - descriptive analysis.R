
#' 
#' This function plots a wordcloud from a document term matrix.
#' 
#' @param dtm a document term matrix
#' @param max.words the maximal number of words to display in the wordcloud
#' 
plotWordcloud <- function(dtm, max.words = 200) {
  words_freq <- as.matrix(dtm)
  words_freq <- colSums(words_freq)
  words_freq <- data.table(word = names(words_freq), freq = words_freq)
  words_freq <- words_freq[order(-freq)]
  wordcloud(
    words = words_freq$word, 
    freq = words_freq$freq, 
    min.freq = 1,
    max.words = 200, 
    random.order = FALSE, 
    rot.per = 0.35, 
    colors = brewer.pal(8, "Dark2")
  )
}