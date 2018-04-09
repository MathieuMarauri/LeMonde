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