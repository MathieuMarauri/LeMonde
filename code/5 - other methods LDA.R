
# LDA model is now performed on the different document term matrices constructed
# in the pre-processing step. The same process as the one used for the baseline
# is used. Helper functions are created to ease the anaysis. 

# Packages ----------------------------------------------------------------

library('data.table') # dataset manipulation
library('stringi') # string manipuation
library('ggplot2') # data visualisation
library('topicmodels') # LDA modeling
library('tidytext') # tidy LDA objects
source('code/helpers/LDA analysis.R')


# Corpus ------------------------------------------------------------------

# import article data
articles <- readRDS('data/articles.rds')

# keep only relevant variables
articles <- articles[, .(id = as.character(id), text, category, subcategory)]

# duplicate articles that are in several categories
categories <- articles[, cbind(category_simple = strsplit(category, '/'), .SD), 
                       by = 'id']

# table with article id and associated category to compute counts by category
categories <- categories[, .(id = id, 
                             category = category_simple, 
                             subcategory = subcategory)]


# Bag of words ------------------------------------------------------------

# The LDA model is performed on the dtm obtained with the bag words stopwords
# method.

# Construct the model
# dtm_bow <- readRDS('data/dtm/dtm_bow.rds')
# articles_lda <- LDA(x = dtm_bow, 
#                     k = 8, 
#                     method = 'VEM', 
#                     control = list(seed = 1234, 
#                                    verbose = 5))
# rm(dtm_bow)
# saveRDS(articles_lda, 'data/lda/lda_bow.rds')

# import model results
articles_lda <- readRDS('data/lda/lda_bow.rds')

# What are the words most associated with the topics?
plotTopWords(model = articles_lda, use_diff = FALSE, n = 10)
# Topic 1 seems to be about culture
# Topic 2 seems to be about sport
# Topic 3 is unclear
# Topic 4 is unclear
# Topic 5 seems to be about Irma 
# Topic 6 is unclear
# Topic 7 seems to be about politics
# Topic 8 seems to be about olympics

# What are the words most associated with only one topic?
plotTopWords(model = articles_lda, use_diff = TRUE, n = 10)
# Categories defined previously are more valid

# How are the articles distributed on the topics?
plotGammaDistribution(model = articles_lda, categories = categories)
# Intuition from top words is confirmed

# How are the articles distributed on the topics over subcategories?
plotGammaDistribution(model = articles_lda, categories = categories, subcategory = TRUE)
# Intuition from top words is confirmed

confusionMatrix(model = articles_lda, gamma_threshold = NULL, sankey = FALSE)
