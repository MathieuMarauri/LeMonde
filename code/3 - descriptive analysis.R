
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


# Ggplot theme ----------------------------------------------------------------------

# default theme
theme_set(theme_bw(base_size = 15))


# Corpus ------------------------------------------------------------------

# import the corpus
articles <- readRDS('data/articles.rds')

# duplicate articles that are in several categories
articles_duplicated <- articles[, cbind(category_simple = strsplit(category, '/'), .SD), 
                                by = 'id']


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
rm(subcategory, articles_subcategory, articles_duplicated)


# Text --------------------------------------------------------------------

# Now lets look at the top words of the corpus and then by category/subcategory. To do so
# the cleaned corpora are used. For each stopwords removal methods the top words will be
# extracted and a wordcloud with the top 100 words is done.

# Corpus

# import word count by article (baseline)
article_word <- readRDS('data/article_word.rds')

# wordcloud 
wordcloud_data <- article_word[, .(count = .N), by = word][order(-count)]
wordcloud(
  words = wordcloud_data$word, 
  freq = wordcloud_data$count,
  min.freq = 1,
  max.words = 200, 
  random.order = FALSE, 
  rot.per = 0.35, 
  colors = brewer.pal(8, "Dark2")
)
# As it is expected without removing stopwords the words that appear the most are non
# informative words (le, de, cardinals, avoir, un , ...)

# Plot top words by category

# add category to word count
article_word <- merge(x = article_word[, article_id := as.numeric(article_id)], 
                      y = articles_duplicated, 
                      by.x = 'article_id',
                      by.y = 'id', 
                      allow.cartesian = TRUE)

# top 10 words by category
article_word_top10 <- article_word[, .(count = .N), by = list(category_simple, word)]
article_word_top10 <- article_word_top10[order(-count), head(.SD, 10), by = category_simple]
article_word_top10[, word := reorder(word, count)]

# plot
ggplot(data = article_word_top10, mapping = aes(x = word, y = count)) +
  geom_point(show.legend = FALSE) + 
  geom_segment(mapping = aes(xend = word, yend = 0), show.legend = FALSE) + 
  facet_wrap(~ category_simple, scales = "free") + 
  coord_flip()

# wordcloud 
wordcloud_data <- article_word[, .(count = .N), by = list(category, word)][order(-count)]
par(mfrow=c(1, 2))
wordcloud(
  words = wordcloud_data$word, 
  freq = wordcloud_data$count,
  min.freq = 1,
  max.words = 200, 
  random.order = FALSE, 
  rot.per = 0.35, 
  colors = brewer.pal(8, "Dark2")
)

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


