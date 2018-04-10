
# The corpus of articles obtained from ww.lemonde.fr are cleaned and stopwords are removed
# using different methods.

# The pre_processing of the corpus consists of tokenization, lemmatization and
# part-of-speech tagging. Treetagger is used to perform this pre-processing.

# Stopwords are removed using 5 different methods (the last one being a combination of the
# others). The baseline will be no removal at all. These methods are compared regarding
# the data sparsity of the document term matrix and the number of words removed by the
# method. Besides a wordcloud of the corpus is obtained and top words by category. The top
# words are defined using a raw occurence count and the difference in probability between
# the category and the mean on all categories. The probability is just the number of time
# the word occurs in the text divided by the total number of words.

# The methods are: bag of words, part-of-speech, TF1, IDF, TF-IDF and a mix of
# all the methods.

# Packages ----------------------------------------------------------------

library('data.table')
library('stringi')
library('ggplot2')
library('tidytext')
source('code/helpers/2 - pre-processing functions.R')


# Corpus ------------------------------------------------------------------

# Import the artices raw data that will be cleaned. Articles are duplicated and a table
# with the artile id and its corresponding category is created to then analyse the top
# words by category

# import articles data
articles <- readRDS('data/articles.rds')

# duplicate articles that are in several categories
categories <- articles[, cbind(category_simple = strsplit(category, '/'), .SD), 
                       by = 'id']

# table with article id and associated category to compute counts by category
categories <- categories[, .(id = as.character(id), category = category_simple)]

# keep only id and text
articles <- articles[, .(id = as.character(id), text)]


# Tokenization/lemmatization/pos tgging -----------------------------------

# replace apostrophe by proper character symbol so treetagger can properly
# tokenize words
apostrophe <- substr(articles$text[21], start = 4, 4)
articles$text <- stri_replace_all_fixed(str = articles$text, 
                                        pattern = apostrophe, 
                                        replacement = "'")

# tokenization and lemmatization are done using treetagger. 
article_word <- lapply(X = 1:nrow(articles),
                       FUN = function(i) {
                         lemmatize(text = articles$text[i], 
                                   id = articles$id[i], 
                                   filter = FALSE)
                       })
article_word <- do.call(rbind, article_word)
article_word <- article_word[, .(article_id = id, word = lemma, pos)]

# punctuation marks, symbols and sentence marks are removed
article_word <- article_word[!pos %in% c('PUN', 'PUN:cit', 'SENT', 'SYM')]

# save result and clean session
saveRDS(article_word, 'data/article_word.rds')
rm(apostrophe, article_word, articles)


# Baseline ----------------------------------------------------------------

# As a baseline no stopwords removal method is applied.

# import tagged data 
article_word <- readRDS('data/article_word.rds')

# word count by article
article_word_count <- article_word[, .(count = .N), by = list(article_id, word)]

# number of words
nb_words_baseline <- uniqueN(article_word_count$word)

# cast to document-term-matrix
(dtm_baseline <- cast_dtm(data = article_word_count, 
                          document = article_id, 
                          term = word, 
                          value = count))

# wordcloud 
plotWordcloud(article_word_count)
# Words are meaningful

# Plot top words by category for count and probability

# count
plotTopCount(article_word_count, categories)
# The top words are meaningful for the categories

# probability difference
plotTopProb(article_word_count, categories)
# The top words are meaningful for the categories

saveRDS(dtm_baseline, 'data/dtm/dtm_baseline.rds')
rm(article_word_count, dtm_baseline)


# Bag of words ------------------------------------------------------------

# stopwords are removed using a pre-defined bag of words

# import tagged data 
article_word <- readRDS('data/article_word.rds')

# word count by article
article_word_count <- article_word[, .(count = .N), by = list(article_id, word)]

# load the stopwords list and anti-join it with the corpus
stopwords <- readLines('data/dtm/stopwords_fr.txt', encoding = 'UTF-8')
stopwords <- data.table(word = stopwords)
article_word_count <- article_word_count[!stopwords, on = 'word']

# percentage of words removed
(1 - uniqueN(article_word_count$word) / nb_words_baseline) * 100

# cast to document-term-matrix
(dtm_bow <- cast_dtm(data = article_word_count, 
                     document = article_id, 
                     term = word, 
                     value = count))

# wordcloud 
plotWordcloud(article_word_count)
# Words are meaningful

# Plot top words by category for count and probability

# count
plotTopCount(article_word_count, categories)
# There are some differences between categories even if some words (otobre, france, ...)
# are in every categories.

# probability difference
plotTopProb(article_word_count, categories)
# Words are much more meaningful and well associated with their category

# save result and clean session
saveRDS(dtm_bow, 'data/dtm/dtm_bow.rds')
rm(article_word_count, stopwords, dtm_bow)

# < 1% removed, sparsity = 100%, remaining words are meaningful and the categories are
# well defined by the words (by count and probability difference).


# POS tags ----------------------------------------------------------------

# stopwords are removed based on their part-of-speech, only ADJ, ADV, NOM, NAM,
# and VER: tags are kept

# import tagged data 
article_word <- readRDS('data/article_word.rds')

# word count by article, filter out stopwords tags
article_word_count <- article_word[pos %in% c('ADJ', 'ADV', 'NOM', 'NAM') | 
                                     stringi::stri_detect_regex(str = pos, pattern = '^VER'), 
                                   .(count = .N), 
                                   by = list(article_id, word)]

# percentage of words removed
(1 - uniqueN(article_word_count$word) / nb_words_baseline) * 100

# cast to document-term-matrix
(dtm_pos <- cast_dtm(data = article_word_count, 
                     document = article_id, 
                     term = word, 
                     value = count))

# wordcloud 
plotWordcloud(article_word_count)
# Words are not more meaningful than the baseline even if words like le, de, un are removed

# Plot top words by category for count and probability

# count
plotTopCount(article_word_count, categories)
# The top words are the same for all categories 

# probability difference
plotTopProb(article_word_count, categories)
# Words are much more meaningful and well associated with their category

# save result and clean session
saveRDS(dtm_pos, 'data/dtm/dtm_pos.rds')
rm(article_word_count, dtm_pos)

# 1% removed, sparsity = 100%, non informative words remain in the corpus, categories are
# not well defined with the top words using raw count. Top words with probabilities on the
# other hand define well the categories.


# TF1 ---------------------------------------------------------------------

# Words appearing only once in the corpus are considered stopwords

# import tagged data 
article_word <- readRDS('data/article_word.rds')

# remove words that appear once
word_count <- article_word[, .(tf = .N), by = word]

ggplot(data = word_count[, .(count = .N), by = tf][tf <= 25][order(-count)], mapping = aes(x = tf, y = count)) + 
  geom_point(size = 2) +
  geom_segment(mapping = aes(xend = tf, yend = 0), show.legend = FALSE) + 
  scale_x_continuous(breaks = seq(1, 25, by = 1)) + 
  labs(x = 'Term-frequency', y = '', title = 'Number of words by term-frequency lower than 25') + 
  theme_bw()

word_count <- word_count[tf == 1, .(word)]
article_word_count <- article_word[, .(tf = .N), by = list(article_id, word)]
article_word_count <- article_word_count[!word_count, on = 'word']

# percentage of words removed
(1 - uniqueN(article_word_count$word) / nb_words_baseline) * 100

# cast to document-term-matrix
(dtm_tf1 <- cast_dtm(data = article_word_count, 
                     document = article_id, 
                     term = word, 
                     value = tf))

# rename columns so functions can be applied
names(article_word_count)[3] <- 'count'

# wordcloud 
plotWordcloud(article_word_count)
# Words are not more meaningful than the baseline as expected

# Plot top words by category for count and probability

# count
plotTopCount(article_word_count, categories)
# The top words are the same for all categories 

# probability difference
plotTopProb(article_word_count, categories)
# There are meaningful words but still lots of non informative words

# save result and clean session
saveRDS(dtm_tf1, 'data/dtm/dtm_tf1.rds')
rm(article_word_count, word_count, dtm_tf1)

# 45% removed, sparsity = 99%, remaining words are not so meaningful and categories are
# not well defined using count. Probabilities give better results. 


# IDF ---------------------------------------------------------------------

# inverse-document-frequency is used to detect words that will not help
# differentiate the articles: words that are only in few articles or in every
# article. Thresholds will be inferred manually based on the data. 

# import tagged data 
article_word <- readRDS('data/article_word.rds')

# word count by article
article_word_count <- article_word[, .(count = .N), by = list(article_id, word)]

# idf
word_idf <- article_word_count[, .(idf = log(nrow(articles) / uniqueN(article_id))), by = word]

# IDF0 and IDF max are used to remove stopwords
word_idf <- word_idf[idf == max(idf) | idf == 0]

# remove words with IDF0 or IDFmax
article_word_count <- article_word_count[!word_idf, on = 'word']

# percentage of words removed
(1 - uniqueN(article_word_count$word) / nb_words_baseline) * 100

# cast to document-term-matrix
(dtm_idf <- cast_dtm(data = article_word_count, 
                     document = article_id, 
                     term = word, 
                     value = count))

# rename columns so functions can be applied
names(article_word_count)[3] <- 'count'

# wordcloud 
plotWordcloud(article_word_count)
# Words are not more meaningful than the baseline even if le and de are removed

# Plot top words by category for count and probability

# count
plotTopCount(article_word_count, categories)
# The top words are the same for all categories 

# probability difference
plotTopProb(article_word_count, categories)
# There are meaningful words but still lots of non informative words

# save result and clean session
saveRDS(dtm_idf, 'data/dtm/dtm_idf.rds')
rm(article_word_count, word_idf, dtm_idf)

# 51% removed, sparsity = 99%, not meaningful words still remain, categories are not well
# defined using count, probabilities give better results but there are still non
# informative words.


# TF-IDF ------------------------------------------------------------------

# words with low TF-IDF score are removed. TF-IDF reflects the importance of a
# word to a document in a corpus. Words with low tf-idf on all documents are
# removed.

# import tagged data 
article_word <- readRDS('data/article_word.rds')

# term-frequency
article_word_count <- article_word[, .(count = .N), by = list(article_id, word)]
article_word_count[, tf := count / sum(count), by = article_id]
# inverse-document-frequency
article_word_count[, idf := log(nrow(articles) / uniqueN(article_id)), by = word]
# tf-idf
article_word_count[, tfidf := count * idf]
# mean tf-idf by word
word_tfidf <- article_word_count[, .(tfidf = mean(tfidf)), by = word][order(tfidf)]

# The first words are removed (this number is obtained looking at the table
# and considering a threshold under which words carry inforamtion)
word_tfidf <- head(word_tfidf, 100)

# remove words from corpus
article_word_count <- article_word_count[!word_tfidf, on = 'word']

# percentage of words removed
(1 - uniqueN(article_word_count$word) / nb_words_baseline) * 100

# cast to document-term-matrix
(dtm_tfidf <- cast_dtm(data = article_word_count, 
                       document = article_id, 
                       term = word, 
                       value = count))

# select columns so functions can be applied
article_word_count <- article_word_count[, .(article_id, word, count)]

# wordcloud 
plotWordcloud(article_word_count)
# Words are meaningful

# Plot top words by category for count and probability

# count
plotTopCount(article_word_count, categories)
# The top words are meaningful for the categories

# probability difference
plotTopProb(article_word_count, categories)
# The top words are meaningful for the categories

# save result and clean session
saveRDS(dtm_tfidf, 'data/dtm/dtm_tfidf.rds')
rm(article_word_count, word_tfidf, dtm_tfidf)

# < 1% removed, sparsity = 100%, words are meaningful, categories are well defined. 


# Mix ---------------------------------------------------------------------

# Words are removed using the bag of words method, the pos tags method, the idf method and
# the tf-idf method

# import tagged data 
article_word <- readRDS('data/article_word.rds')

# pos tag method
article_word_count <- article_word[pos %in% c('ADJ', 'ADV', 'NOM', 'NAM') | 
                                     stringi::stri_detect_regex(str = pos, pattern = '^VER'),
                                   .(count = .N), by = list(article_id, word)]

# load the stopwords list and anti-join it with the corpus
stopwords <- readLines('data/dtm/stopwords_fr.txt', encoding = 'UTF-8')
stopwords <- data.table(word = stopwords)
article_word_count <- article_word_count[!stopwords, on = 'word']

# idf method
word_idf <- article_word_count[, .(idf = log(nrow(articles) / uniqueN(article_id))), by = word]
word_idf <- word_idf[idf == max(idf) | idf == 0]
article_word_count <- article_word_count[!word_idf, on = 'word']

# tf-idf method
article_word_count_tfidf <- article_word[, .(count = .N), by = list(article_id, word)]
article_word_count_tfidf[, tf := count / sum(count), by = article_id]
article_word_count_tfidf[, idf := log(nrow(articles) / uniqueN(article_id)), by = word]
article_word_count_tfidf[, tfidf := count * idf]
word_tfidf <- article_word_count_tfidf[, .(tfidf = mean(tfidf)), by = word][order(tfidf)]
word_tfidf <- head(word_tfidf, 100)
article_word_count <- article_word_count[!word_tfidf, on = 'word']

# percentage of words removed
(1 - uniqueN(article_word_count$word) / nb_words_baseline) * 100

# cast to document-term-matrix
(dtm_mix <- cast_dtm(data = article_word_count, 
                     document = article_id, 
                     term = word, 
                     value = count))

# select columns so functions can be applied
article_word_count <- article_word_count[, .(article_id, word, count)]

# wordcloud 
plotWordcloud(article_word_count)
# Words are meaningful

# Plot top words by category for count and probability

# count
plotTopCount(article_word_count, categories)
# The top words are meaningful for the categories

# probability difference
plotTopProb(article_word_count, categories)
# The top words are meaningful for the categories

# save result and clean session
saveRDS(dtm_mix, 'data/dtm/dtm_mix.rds')
rm(article_word_count, word_idf, dtm_mix, stopwords, article_word_count_tfidf)

# 52% removed, sparsity = 99%, words are meaningful and categories are well defined. 

