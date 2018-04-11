
#'
#' This function lemmatizes a text using treetagger.
#' 
#' @param text a raw character vector of length one.
#' @param id the id of the text in the corpus
#' @param filter boolean, should some pos be filtered out
#' 
#' @return a table with id, lemma and count
#' 
lemmatize <- function(text, id, filter = TRUE) {
  path_to_treetag <- 'C:/TreeTagger'
  writeLines(text = text, con = file.path(path_to_treetag, 'article_to_tag.txt'), useBytes = TRUE)
  shell('cd /d C:/TreeTagger & tag-french article_to_tag.txt tag_results.txt')
  tag_results <- fread(input = file.path(path_to_treetag, 'tag_results.txt'), header = FALSE, encoding = 'UTF-8', quote = "")
  names(tag_results) <- c('token', 'pos', 'lemma')
  file.remove(file.path(path_to_treetag, 'article_to_tag.txt'), file.path(path_to_treetag, 'tag_results.txt'))
  if(filter) {
    tag_results <- tag_results[pos %in% c('ADJ', 'ADV', 'NOM', 'NAM') | stringi::stri_detect_regex(str = pos, pattern = '^VER')]
  }
  tag_results <- cbind(id = id, tag_results)
  return(tag_results)
}


#' 
#' This function plots a wordcloud from a word count data.
#' 
#' @param article_word word count data
#' @param max.words the maximal number of words to display in the wordcloud
#' 
plotWordcloud <- function(article_word, max.words = 200) {
  if(ncol(article_word) != 3) {
    stop('"article_word" does not have 3 columns')
  }
  if(any(names(article_word) != c('article_id', 'word', 'count'))) {
    stop('"article_word" does not have the expected colnames')
  }
  if(!is.data.table(article_word)) {
    setDT(article_word)
    warning('"article_word" was coerced to data.table')
  }
  wordcloud_data <- article_word[, .(count = sum(count)), by = word][order(-count)]
  wordcloud(
    words = wordcloud_data$word, 
    freq = wordcloud_data$count,
    min.freq = 1,
    max.words = 200, 
    random.order = FALSE, 
    rot.per = 0.35, 
    scale = c(3, 0.5),
    colors = brewer.pal(8, "Dark2")
  )
}

#' 
#' This function plots the top most occuring words by category
#' 
#' @param article_word word count data
#' @param category a table witht the articcle id and their corresponding category
#' @param top_n the number of words to display
#' 
plotTopCount <- function(article_word, category, top_n = 10) {
  if(ncol(article_word) != 3) {
    stop('"article_word" does not have 3 columns')
  }
  if(any(names(article_word) != c('article_id', 'word', 'count'))) {
    stop('"article_word" does not have the expected colnames')
  }
  if(!is.data.table(article_word)) {
    setDT(article_word)
    warning('"article_word" was coerced to data.table')
  }
  if(ncol(category) != 2) {
    stop('"category" must have 2 columns')
  }
  if(any(names(category) != c('id', 'category'))) {
    stop('"category" does not have the expected colnames')
  }
  article_word <- merge(x = article_word, 
                        y = category, 
                        by.x = 'article_id',
                        by.y = 'id', 
                        allow.cartesian = TRUE)
  # top 10 words by category
  article_word_top10 <- article_word[, .(count = .N), by = list(category, word)]
  article_word_top10 <- article_word_top10[order(-count), head(.SD, 10), by = category]
  article_word_top10[, word := reorder(word, count)]
  # plot
  ggplot(data = article_word_top10, mapping = aes(x = word, y = count)) +
    geom_point(show.legend = FALSE) + 
    geom_segment(mapping = aes(xend = word, yend = 0), show.legend = FALSE) + 
    facet_wrap(~ category, scales = "free") + 
    coord_flip()
}

#'
#' This function plots the top words based on their probability difference between corpus
#' and category
#' 
#' @param article_word word count data
#' @param category a table witht the articcle id and their corresponding category
#' @param top_n the number of words to display
#' 
plotTopProb <- function(article_word, category, top_n = 10) {
  if(ncol(article_word) != 3) {
    stop('"article_word" does not have 3 columns')
  }
  if(any(names(article_word) != c('article_id', 'word', 'count'))) {
    stop('"article_word" does not have the expected colnames')
  }
  if(!is.data.table(article_word)) {
    setDT(article_word)
    warning('"article_word" was coerced to data.table')
  }
  if(ncol(category) != 2) {
    stop('"category" must have 2 columns')
  }
  if(any(names(category) != c('id', 'category'))) {
    stop('"category" does not have the expected colnames')
  }
  # add category information to compute category probability
  article_word <- merge(x = article_word,
                        y = categories,
                        by.x = 'article_id',
                        by.y = 'id',
                        allow.cartesian = TRUE)
  # category probability
  article_word <- article_word[, .(count_cat = sum(count)), 
                               by = list(category, word)]
  article_word <- article_word[, .(word, 
                                   prob_cat = count_cat / sum(count_cat)), 
                               by = category]
  # compare probabilities to find top differences
  article_word[, prob := prob_cat - mean(prob_cat), by = word]
  # plot
  article_word_top10 <- article_word[order(-prob), head(.SD, 10), by = category]
  ggplot(data = article_word_top10, mapping = aes(x = word, y = prob)) +
    geom_point(show.legend = FALSE) + 
    geom_segment(mapping = aes(xend = word, yend = 0), show.legend = FALSE) + 
    facet_wrap(~ category, scales = "free") + 
    coord_flip()
}

