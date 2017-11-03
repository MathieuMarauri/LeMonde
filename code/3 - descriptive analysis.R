
# A description analysis is performed on the corpus. The goal is to have a
# better understanding of the data that was extracted. The category and the
# subcategory are the two features that can be used as a strating point to
# decide the apriori number of topics in the LDA method. Extracting the top
# words will be done on the pre-processed corpora.

# Packages ----------------------------------------------------------------

library('data.table')
library('stringi')
library('ggplot2')


# Corpus ------------------------------------------------------------------

# import the corpus
articles <- readRDS('data/articles.rds')


# Category ----------------------------------------------------------------

# How articles are distributed on the categories? How many by category(ies)?

# keep only id and category
articles_category <- articles[, .(id, category)]

# The number of articles by category
category <- articles_category[, .(count = .N), by = category]

# How many articles are in more than one category? LDA is a fuzzy clustering
# method so we expect these articles to have high probabilities for several
# topics.
articles_category[, nb_category := stri_count_fixed(category, '/') + 1]
nb_category <- articles_category[, .(count = .N, percent = .N / nrow(articles_category)), by = nb_category][order(-count)]
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
category_comb <- colSums(category[stri_detect_fixed(category, '/'), 
                                  lapply(.SD, function(x) x * count), 
                                  .SDcols = categories_names])
# articles with the sport category are the ones that are the less often
# associated with another category. The categories economie, politique and idees
# are the ones that are most often associated with another category.
# International and societe are also often used with another category.
# The frequent associations can be found in the category table. 

# clean environnement
rm(categories_names, category_comb, nb_category, category, articles_category)


# Subcategory -------------------------------------------------------------

# How articles are distributed on the subcategories? How are they linked to the
# categories?

# keep only id, category and subcategory
articles_subcategory <- articles[, .(id, category, subcategory)]

# Indicators of the repartition of articles by category/subcategory 
subcategory <- articles_subcategory[, .(count = .N, 
                                        total_percent = .N / nrow(articles)), 
                                    by = list(category, subcategory)]
# subcat_percent and subcat_nb are the percentage of articles in the given
# subcategory by category and the number of different subcategories by category
subcategory[, c('subcat_percent', 'subcat_nb') := list(count / sum(count), uniqueN(subcategory)),
            by = category]
# cat_percent and cat_nb are the percentage of articles in the given
# category by subcategory and the number of different categories by subcategory
subcategory[, c('cat_percent', 'cat_nb', 'subcat_count') := list(count / sum(count), 
                                                                 uniqueN(category),
                                                                 sum(count)),
            by = subcategory]

# the sport category is the one that have the greatest number of subcategories
# (34) but besides the first (with the highest number of categories) 12 that are
# not only related to sport, the others are just a particular sport. On the
# other side, idees is the category with the lowest number of different
# subcateories, not taking into account multiple categories such as
# politique/culture for example. The planete category is associated with
# subcategories that are found in many different categories. The first
# subcategories in terms of articles count that are not identical to any
# category are europe, police-justice and afrique. les-decodeurs, societe and
# football are equal at the 4th place.

# clean environnement
rm(subcategory, articles_subcategory)


# Text --------------------------------------------------------------------

# Now lets look at the top words by category/subcategory. To do so the cleaned
# corpora are used. For each stopwords removal methods the top words will be
# extracted.


