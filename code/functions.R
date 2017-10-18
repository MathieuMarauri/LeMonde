
#'
#' This function extracts information on the articles on a given topic on the
#' website of LeMonde
#'
#' @param topic a valid topic name found on the website
#' @param maxPage maximum page number
#' @param minPage minimum page number
#'
#' @return a table with the infos of the articles (title, url, date, time,
#'   topic)
#'
#' @details urls for videos, live and blog posts are removed because of their
#'   structure
#'   
getArticleInfo <- function(topic, maxPage = 5, minPage = 1) {
  baseUrl <- 'http://www.lemonde.fr'
  infos <- lapply(X = minPage:maxPage,
                  FUN = function(i){
                    tryCatch({
                      url_path <- paste0(baseUrl, '/', topic, '/', i, '.html')
                      url_path <- read_html(url_path)
                      url <- html_nodes(url_path, ".grid_12.alpha.enrichi .grid_11.conteneur_fleuve.omega h3 a") %>% 
                        html_attr(name = 'href')
                      date <- stri_replace_all_regex(str = url, pattern = '.*([0-9]{4}/[0-9]{2}/[0-9]{2}).*', replacement = '$1')
                      date <- as.Date(date, format = '%Y/%m/%d')
                      title <- html_nodes(url_path, ".grid_12.alpha.enrichi .grid_11.conteneur_fleuve.omega h3 a") %>% 
                        html_text()
                      time <- html_nodes(url_path, ".grid_12.alpha.enrichi time") %>% 
                        html_text()
                      return(data.table(title = title, url = paste0(baseUrl, url), date = date, time = time))
                    }, error = function(message){
                      print(paste0('the page ', i, ' could not be parsed due to the following error: ', message))
                    })
                  })
  infos <- do.call(rbind, infos)
  infos[, topic := topic]
  infos <- infos[!(stri_detect_regex(str = url, pattern = '(/video/|/videos/|/portfolio/|/live/|/grands-formats/)') | 
                     stri_detect_fixed(str = url, pattern = '.blog.') | 
                     stri_detect_fixed(str = url, pattern = '/live/'))]
  return(infos)
}

#'
#' This function extract the text of an article.
#'
#' @param articleUrl the url of an article retrieved with \code{getArticleInfo}
#'
#' @return the raw text of the article
#'   
getArticleText <- function(articleUrl) {
  text <- tryCatch({
    articleUrl <- read_html(articleUrl)
    first_par <- html_nodes(articleUrl, "div[itemprop=articleBody] .taille_courante") %>% html_text() %>% paste(collapse = ' ')
    other_par <- html_nodes(articleUrl, "div[itemprop=articleBody] p:not(.lire)") %>% html_text() %>% paste(collapse = ' ')
    text <- paste(first_par, other_par, sep = '\n')
  }, error = function(message) {
    print(paste0('the url ', articleUrl, ' could not be parsed due to the following error: ', message))
    return(NA)
  })
  return(text)
}

#'
#' This function extract the author of an article.
#'
#' @param articleUrl the url of an article retrieved with \code{getArticleInfo}
#'
#' @return the author of the article
#' 
getArticleAuthor <- function(articleUrl) {
  author <- tryCatch({
    articleUrl <- read_html(articleUrl)
    author <- html_nodes(articleUrl, ".signature_article span") %>% html_text() %>% paste(collapse = ' ')
    author
  }, error = function(message) {
    print(paste0('the url ', articleUrl, ' could not be parsed due to the following error: ', message))
    return(NA)
  })
  return(author)
}

#'
#' This function cretes a table containing inforamtion on article from a given
#' topic
#'
#' @param topic the name of the topic, must be a valid topic name
#' @param maxPage maximum page number
#' @param minPage minimum page number
#'
#' @return a table with title, text, topic, subtopic, date, time, author,
#'   and url
#'   
createTopicTable <- function(topic, maxPage = 5, minPage = 1) {
  articles_topic <- getArticleInfo(topic = topic, maxPage = maxPage, minPage = minPage)
  article_text <- lapply(X = articles_topic$url,
                         FUN = function(url) getArticleText(articleUrl = url))
  article_author <- lapply(X = articles_topic$url,
                           FUN = function(url) getArticleAuthor(articleUrl = url))
  subtopic <- stri_replace_all_regex(str = articles_topic$url[1],
                                     pattern = '.*fr/(.*)/article/.*',
                                     replacement = '$1')
  articles_topic <- articles_topic[, .(title, text = unlist(article_text), topic,
                                       subtopic = subtopic, date, time,
                                       author = unlist(article_author),
                                       url, date_creation = Sys.time())]
  return(articles_topic)
}

#' This function lemmatizes a text using treetagger.
#' 
#' @param text a raw character vectr of length one.
#' @param id the id of the text in the corpus
#' @param filter booena, should some pos be filtered out
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

