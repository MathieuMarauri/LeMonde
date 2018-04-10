
# A description analysis is performed on the corpus. The goal is to have a better
# understanding of the data that was extracted. The category and the subcategory are the
# two features that can be used as a starting point to decide the a priori number of
# topics in the LDA method. Extracting the top words will be done on the pre-processed
# corpora (for the baseline and the mix).

# Packages ----------------------------------------------------------------

library('data.table') # datast manipulation
library('stringi') # string manipulation
library('ggplot2') # data visualisation
library("wordcloud") # wordcloud visualisation
library('tidytext') # tidy document term matrix
source('code/helpers/3 - descriptive analysis.R')


# Ggplot theme ----------------------------------------------------------------------

# default theme
theme_set(theme_bw(base_size = 15))


# Corpus ------------------------------------------------------------------

# import the corpus
articles <- readRDS('data/articles.rds')

# duplicate articles that are in several categories
articles_duplicated <- articles[, cbind(category_simple = strsplit(category, '/'), .SD), 
                                by = 'id']

# table with article id and associated category to compute counts by category
categories <- articles_duplicated[, .(id = id, category = category_simple)]


# Category ----------------------------------------------------------------

# How articles are distributed on the categories? How many by category(ies)?

# keep only id and category
articles_category <- articles[, .(id, category)]

# the number of articles by category
category <- articles_category[, .(count = .N), by = category]

# number of articles by category (duplicating articles with several categories)
category_simple <- articles_duplicated$category
category_simple <- as.data.table(category_simple)[, .(count = .N), by = category_simple]

# plot
ggplot(data = category_simple, mapping = aes(x = reorder(category_simple, count), y = count)) + 
  geom_segment(mapping = aes(xend = category_simple, yend = 0)) + 
  geom_point() + 
  labs(x = '', y = '', 
       title = 'Number of articles by category on lemonde.fr', 
       subtitle = 'Articles in several categories are duplicated') +
  coord_flip()

# How many articles are in more than one category? LDA is a fuzzy clustering method so we
# expect these articles to have high probabilities for several topics.

# count number of / in the category field and add 1 to have the number of categories in
# which the article was found while scrapping the data
articles_category[, nb_category := stri_count_fixed(category, '/') + 1]
articles_category <- articles_category[, .(count = .N, 
                                           percent = .N / nrow(articles_category)), 
                                       by = nb_category][order(-count)]

# plot
ggplot(data = articles_category, 
       mapping = aes(x = nb_category, y = percent, label = count)) + 
  geom_bar(stat = 'identity') + 
  geom_text() + 
  labs(x = 'Number of categories by article', 
       y = 'Percentage of articles', 
       title = 'Percentage of articles classified in several categories.')
# About 20% of all articles have been classified into more than one category on
# the website. 
  
# What is the category that is the most often associated with other categories?

# flag category if it is present in multiple categories combination
categories_names <- c('sport', 'culture', 'planete', 'international', 
                      'sciences', 'societe', 'politique', 'economie', 'idees')
category[, (categories_names) := lapply(X = categories_names, 
                                        FUN = function(x) stri_detect_fixed(category, 
                                                                            pattern = x))]

# number of articles associated with at least two categories by category
articles_category <- colSums(category[stri_detect_fixed(category, '/'), 
                                      lapply(.SD, function(x) x * count), 
                                      .SDcols = categories_names])

# construct proper data structure for plotting
articles_category <- data.table(category = names(articles_category),
                                count = articles_category)
articles_category$category <- factor(articles_category$category, 
                                     levels = articles_category$category[order(articles_category$count)])

# plot 
ggplot(data = articles_category, mapping = aes(x = category, y = count)) + 
  geom_point() + 
  geom_segment(mapping = aes(xend = category, yend = 0)) + 
  labs(x = '', y = '', 
       title = 'Number of articles associated with at least two categories by category') +
  coord_flip()
# Articles with the sport category are the ones that are the less often associated with
# another category. The categories economie, politique and idees are the ones that are
# most often associated with another category. International and societe are also often
# used with another category. The frequent associations can be found in the category
# table.

# clean environnement
rm(categories_names, category, articles_category, category_simple)


# Subcategory -------------------------------------------------------------

# How articles are distributed on the subcategories? How are they linked to the
# categories?

# keep only id, category and subcategory
articles_subcategory <- articles[, .(id, category, subcategory)]

# repartition of articles by category/subcategory 
subcategory <- articles_duplicated[, .(count = .N, 
                                        total_percent = .N / nrow(articles)), 
                                    by = list(category, subcategory)]

# subcat_percent and subcat_nb are the percentage of articles in the given subcategory by
# category and the number of different subcategories by category
subcategory[, c('subcat_percent', 'subcat_nb') := list(count / sum(count), uniqueN(subcategory)),
            by = category]

# cat_percent and cat_nb are the percentage of articles in the given category by
# subcategory and the number of different categories by subcategory
subcategory[, c('cat_percent', 'cat_nb', 'subcat_count') := list(count / sum(count), 
                                                                 uniqueN(category),
                                                                 sum(count)),
            by = subcategory]

# plot
ggplot(data = subcategory, 
       mapping = aes(x = reorder(category, -subcat_nb), 
                     y = reorder(subcategory, -cat_nb), 
                     fill = count)) + 
  geom_tile() + 
  labs(x = '', y = '', 
       title = 'Number of articles by couple category/subcategory.',
       subtitle = 'Articles in several categories are duplicated')

# The economie category is the one that have the greatest number of subcategories (42)
# followed by sport (37). The economy category only has 4 sub-categories that are unique
# to it. The other sub-categories can be found under at least one other category. Sport on
# the other hand has 18 sub-categories that are only related to sport (mainly specific
# sports). The categories with the least number of sub-categories are planete and idea
# with respectively 22 and 24 sub-categories.

# clean environnement
rm(subcategory, articles_subcategory)


