
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
source('code/helpers/3 - descriptive analysis.R')


# Corpus ------------------------------------------------------------------

# import the corpus
articles <- readRDS('data/articles.rds')


# Category ----------------------------------------------------------------

# How articles are distributed on the categories? How many by category(ies)?

# keep only id and category
articles_category <- articles[, .(id, category)]

# the number of articles by category
category <- articles_category[, .(count = .N), by = category]

# number of articles by category (duplicating articles with several categories)
category_simple <- articles[, cbind(category = strsplit(category, '/'), .SD), by = 'id']$category
category_simple <- as.data.table(category_simple)[, .(count = .N), by = category_simple]

# plot
ggplot(data = category_simple, mapping = aes(x = reorder(category_simple, count), y = count)) + 
  geom_segment(mapping = aes(xend = category_simple, yend = 0)) + 
  geom_point() + 
  labs(x = '', y = '', 
       title = 'Number of articles by category on lemonde.fr', 
       subtitle = 'Articles in several categories are duplicated') +
  coord_flip() +
  theme_bw()

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
       title = 'Percentage of articles classified in several categories.') +
  theme_bw()
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
  coord_flip() +
  theme_bw()
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

# indicators of the repartition of articles by category/subcategory 
subcategory <- articles_subcategory[, .(count = .N, 
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

# the sport category is the one that have the greatest number of subcategories (34) but
# besides the first (with the highest number of categories) 12 that are not only related
# to sport, the others are just a particular sport. On the other side, idees is the
# category with the lowest number of different subcateories, not taking into account
# multiple categories such as politique/culture for example. The planete category is
# associated with subcategories that are found in many different categories. The first
# subcategories in terms of articles count that are not identical to any category are
# europe, police-justice and afrique. les-decodeurs, societe and football are equal at the
# 4th place.

# clean environnement
rm(subcategory, articles_subcategory)


# Text --------------------------------------------------------------------

# Now lets look at the top words of the corpus and then by category/subcategory. To do so
# the cleaned corpora are used. For each stopwords removal methods the top words will be
# extracted and a wordcloud with the top 100 words is done.

# Baseline

# import baseline dtm
dtm_baseline <- readRDS('data/dtm/dtm_baseline.rds')

# wordcloud 
plotWordcloud(dtm = dtm_baseline)
# as expected the words that appear the most are non informative words (le, de, cardinals,
# avoir, un , ...)

# Plot top words by category

# add category information to the document term matrix
baseline_dt <- as.matrix(dtm_baseline)
baseline_dt <- data.table(id = as.integer(rownames(baseline_dt)), baseline_dt)
baseline_dt <- merge(
  x = baseline_dt,
  y = articles[, .(id, category)],
  by = 'id'
)
baseline_dt[, id := NULL]

# sum number of words by category
baseline_dt <- baseline_dt[, lapply(X = .SD, FUN = sum), by = category]
baseline_dt <- melt(
  data = baseline_dt, 
  id.vars = 'category', 
  variable.name = 'word', 
  value.name = 'frequency'
)

baseline_dt <- baseline_dt[order(-frequency), head(.SD, 10), by = category]
baseline_dt[, term := reorder(word, frequency)]

ggplot(data = baseline_dt, mapping = aes(x = word, y = frequency)) +
  geom_point(show.legend = FALSE) + 
  geom_segment(mapping = aes(xend = word, yend = 0), show.legend = FALSE) + 
  facet_wrap(~ category, scales = "free") + 
  coord_flip()


