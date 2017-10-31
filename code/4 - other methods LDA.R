
# LDa model is now performed on the different document term matrices constructed
# in the pre-processing step. The same process as the one used for the baseline
# is used. Helper functions are created to ease the anaysis. 

# Packages ----------------------------------------------------------------

library('data.table')
library('stringi')
library('ggplot2')
source('code/functions/LDA analysis.R')


# Corpus ------------------------------------------------------------------

articles <- readRDS('data/articles.rds')

# keep only relevant variables
articles <- articles[, .(id = as.character(id), text, category, subcategory)]

# transform category and subcategory to keep only the last item
articles[, subcategory_main := stri_replace_all_regex(subcategory, '.*/', '')]
articles[, category_main := stri_replace_all_regex(category, '/.*', '')]


# Bag of words ------------------------------------------------------------

# The LDA model is performed on the dtm obtained with the bag words stopwords
# method.

# Construct the model
dtm_bow <- readRDS('data/dtm/dtm_bow.rds')
articles_lda <- topicmodels::LDA(x = dtm_bow, 
                                 k = 8, 
                                 method = 'Gibbs', 
                                 control = list(seed = 1234, 
                                                verbose = 5))
rm(dtm_bow)
saveRDS(articles_lda, 'data/lda/lda_bow.rds')

# import model results
articles_lda <- readRDS('data/lda/lda_bow.rds')

# What are the words most associated with the topics?
getTopWords(model = articles_lda, use_diff = FALSE, n = 10)

# Some words are added to the stopwords list after looking at the most frequent words.

# Topic 1 seems to be about medecine
# Topic 2 seems to be about politics
# Topic 3 seems to be about Irma (with n = 15 'Irma' appears)
# Topic 4 seems to be about economics
# Words from topic 5 are too general
# Topic 6 seems to be about justice
# Topic 7 seems to be about sport
# Topic 8 seems to be about culture

# What are the words most associated with only one topic?
getTopWords(model = articles_lda, use_diff = TRUE, n = 10)

# Words from topic 5 are still not informative enough, Irma now appears in th
# top 10 words in topic 3

# How are the articles distributed on the topics?
confusionMatrix(model = articles_lda, gamma_threshold = NULL, sankey = FALSE)
