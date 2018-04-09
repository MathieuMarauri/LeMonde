
#'
#' This function extracts information on the articles on a given category on the
#' website of LeMonde
#'
#' @param category a valid category name found on the website
#' @param maxPage maximum page number
#' @param minPage minimum page number
#'
#' @return a table with the infos of the articles (title, url, date, time,
#'   category)
#'
#' @details urls for videos, live and blog posts are removed because of their
#'   structure
#'   
getArticleInfo <- function(category, maxPage = 5, minPage = 1) {
  baseUrl <- 'http://www.lemonde.fr'
  infos <- lapply(X = minPage:maxPage,
                  FUN = function(i){
                    tryCatch({
                      url_path <- paste0(baseUrl, '/', category, '/', i, '.html')
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
  infos[, category := category]
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
#' category
#'
#' @param category the name of the category, must be a valid category name
#' @param maxPage maximum page number
#' @param minPage minimum page number
#'
#' @return a table with title, text, category, subcategory, date, time, author,
#'   and url
#'   
createcategoryTable <- function(category, maxPage = 5, minPage = 1) {
  articles_category <- getArticleInfo(category = category, maxPage = maxPage, minPage = minPage)
  article_text <- lapply(X = articles_category$url,
                         FUN = function(url) getArticleText(articleUrl = url))
  article_author <- lapply(X = articles_category$url,
                           FUN = function(url) getArticleAuthor(articleUrl = url))
  subcategory <- stri_replace_all_regex(str = articles_category$url,
                                     pattern = '.*fr/(.*)/article/.*',
                                     replacement = '$1')
  articles_category <- articles_category[, .(title, text = unlist(article_text), category,
                                       subcategory = subcategory, date, time,
                                       author = unlist(article_author),
                                       url, date_creation = Sys.time())]
  return(articles_category)
}