
# News article are scrapped from LeMonde. Scrapping is done by topic so that
# articles are pre-classified. Every articles from every categories are
# extracted. The title, the author, the text, the timestamp, the topic, the
# subtopic and the url are retrieved for each article.

# Using the rvest package and going through the source code of the articles
# webpages, helper functions are created to extract the different features
# mentionned above.

# Packages ----------------------------------------------------------------

library('rvest')
library('stringi')
library('data.table')
source('code/functions.R')


# Topic names -------------------------------------------------------------

topic_names <- read_html('http://www.lemonde.fr/') %>% 
  html_nodes("#navigation-generale li") %>% 
  html_attr("class") %>% 
  unique() %>% 
  stri_replace_all_fixed(pattern = ' ', replacement = '')
# only relevant topics are kept: topic where the structure is the one used in
# getArticleInfo (http://www.lemonde.fr/topic/1.html)
topic_names <- topic_names[2:10] 


# Articles ----------------------------------------------------------------

articles <- lapply(X = topic_names, 
                   FUN = function(topic) createTopicTable(topic = topic, maxPage = 20))
articles <- do.call(rbind, articles)
articles <- articles[!is.na(text)]

# some articles are duplicated
articles <- articles[text != '\n', 
                     .(text = text[1], 
                       topic = paste(topic, collapse = '/'),
                       subtopic = paste(subtopic, collapse = '/'),
                       date = date[1],
                       time = time[1],
                       author = author[1],
                       url = url[1], 
                       date_creation = date_creation[1]),
                     by = title]
articles <- cbind(id = 1:nrow(articles), articles)
saveRDS(articles, 'data/articles.rds')

# cleaning session
rm(list = ls())