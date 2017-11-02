
# News article are scrapped from LeMonde. Scrapping is done by category so that
# articles are pre-classified. Every articles from every categories are
# extracted. The title, the author, the text, the timestamp, the category, the
# subcategory and the url are retrieved for each article.

# Using the rvest package and going through the source code of the articles
# webpages, helper functions are created to extract the different features
# mentionned above.

# Packages ----------------------------------------------------------------

library('rvest')
library('stringi')
library('data.table')
source('code/functions/1 - scrapping functions.R')


# category names -------------------------------------------------------------

category_names <- read_html('http://www.lemonde.fr/') %>% 
  html_nodes("#navigation-generale li") %>% 
  html_attr("class") %>% 
  unique() %>% 
  stri_replace_all_fixed(pattern = ' ', replacement = '')
# only relevant categorys are kept: category where the structure is the one used in
# getArticleInfo (http://www.lemonde.fr/category/1.html)
category_names <- category_names[2:10] 


# Articles ----------------------------------------------------------------

articles <- lapply(X = category_names, 
                   FUN = function(category) createcategoryTable(category = category, maxPage = 20))
articles <- do.call(rbind, articles)
articles <- articles[!is.na(text)]

# some articles are duplicated
articles <- articles[text != '\n', 
                     .(text = text[1], 
                       category = paste(category, collapse = '/'),
                       subcategory = paste(subcategory, collapse = '/'),
                       date = date[1],
                       time = time[1],
                       author = author[1],
                       url = url[1], 
                       date_creation = date_creation[1]),
                     by = title]
articles <- cbind(id = 1:nrow(articles), articles)

# some articles have several categories that are the same, these categories are
# cleaned
articles$category <- strsplit(articles$category, split = '/')
articles$category <- sapply(X = articles$category, 
                            FUN = function(x) paste(unique(x), 
                                                    collapse = '/'))

saveRDS(articles, 'data/articles.rds')

# cleaning session
rm(list = ls())
