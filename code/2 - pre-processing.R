
# The corpus of articles obtained from ww.lemonde.fr are cleaned and stopwords
# are removed using different methods. 

# The pre_processing of the corpus consists of tokenization, lemmatization and
# part-of-speech tagging. Treetagger is used to perform this pre-processing.

# Stopwords are removed using 5 diferent methods (the last one being a
# combination of the others). These methods are compared regarding the data
# sparsity of the document term matrix and the number of words removed by the
# method. The baseline will be no removal at all.

# The methods are: bag of words, part-of-speech, TF1, IDF, TF-IDF and a mix of
# all the methods.

# Packages ----------------------------------------------------------------

library('data.table')
library('stringi')
library('ggplot2')
source('code/functions.R')


# Corpus ------------------------------------------------------------------

articles <- readRDS('data/articles.rds')

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

article_word <- readRDS('data/article_word.rds')


# Baseline ----------------------------------------------------------------

# As a baseline no stopwords removal method is applied.

# word count by article
article_word_count <- article_word[, .(count = .N), by = list(article_id, word)]

# number of words
nb_words_baseline <- uniqueN(article_word_count$word)

# cast to document-term-matrix
(dtm_baseline <- tidytext::cast_dtm(data = article_word_count, 
                                   document = article_id, 
                                   term = word, 
                                   value = count))

saveRDS(dtm_baseline, 'data/dtm/dtm_baseline.rds')
rm(article_word_count, dtm_baseline)


# Bag of words ------------------------------------------------------------

# stopwordsa re removed using a pre-defined bag of words

# word count by article
article_word_count <- article_word[, .(count = .N), by = list(article_id, word)]

# load the stopwords list and anti-join it with the corpus
stopwords <- readLines('data/dtm/stopwords_fr.txt')
stopwords <- data.table(word = stopwords)
article_word_count <- article_word_count[!stopwords, on = 'word']

# percentage of words removed
(1 - uniqueN(article_word_count$word) / nb_words_baseline) * 100

# cast to document-term-matrix
(dtm_bow <- tidytext::cast_dtm(data = article_word_count, 
                              document = article_id, 
                              term = word, 
                              value = count))

# save result and clean session
saveRDS(dtm_bow, 'data/dtm/dtm_bow.rds')
rm(article_word_count, stopwords, dtm_bow)

# < 1% removed, sparsity = 100%


# POS tags ----------------------------------------------------------------

# stopwords are removed based on their part-of-speech, only ADJ, ADV, NOM, NAM,
# and VER: tags are kept

# word count by article, filter out stopwords tags
article_word_count <- article_word[pos %in% c('ADJ', 'ADV', 'NOM', 'NAM') | 
                                     stringi::stri_detect_regex(str = pos, pattern = '^VER'), 
                                   .(count = .N), 
                                   by = list(article_id, word)]

# percentage of words removed
(1 - uniqueN(article_word_count$word) / nb_words_baseline) * 100

# cast to document-term-matrix
(dtm_pos <- tidytext::cast_dtm(data = article_word_count, 
                                       document = article_id, 
                                       term = word, 
                                       value = count))

# save result and clean session
saveRDS(dtm_pos, 'data/dtm/dtm_pos.rds')
rm(article_word_count, dtm_pos)

# 1% removed, sparsity = 100%


# TF1 ---------------------------------------------------------------------

# words appearing only once in the corpus are considered stopwords

# remove words that appear once
word_count <- article_word[, .(count = .N), by = word]
word_count <- word_count[count == 1, .(word)]
article_word_count <- article_word[, .(count = .N), by = list(article_id, word)]
article_word_count <- article_word_count[!word_count, on = 'word']

# percentage of words removed
(1 - uniqueN(article_word_count$word) / nb_words_baseline) * 100

# cast to document-term-matrix
(dtm_tf1 <- tidytext::cast_dtm(data = article_word_count, 
                              document = article_id, 
                              term = word, 
                              value = count))

# save result and clean session
saveRDS(dtm_tf1, 'data/dtm/dtm_tf1.rds')
rm(article_word_count, word_count, dtm_tf1)

# 45% removed, sparsity = 99%


# IDF ---------------------------------------------------------------------

# inverse-document-frequency is used to detect words that will not help
# differentiate the articles: words that are only in few articles or in every
# article. Thresholds will be inferred manually based on the data. 

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
(dtm_idf <- tidytext::cast_dtm(data = article_word_count, 
                               document = article_id, 
                               term = word, 
                               value = count))

# save result and clean session
saveRDS(dtm_idf, 'data/dtm/dtm_idf.rds')
rm(article_word_count, word_idf, dtm_idf)

# 51% removed, sparsity = 99%


# TF-IDF ------------------------------------------------------------------

# words with low TF-IDF score are removed. TF-IDF reflects the importance of a
# word to a document in a corpus. Words with low tf-idf on all documents are
# removed.

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
# and considering a threshold above which words carry inforamtion)
word_tfidf <- head(word_tfidf, 100)

# remove words from corpus
article_word_count <- article_word_count[!word_tfidf, on = 'word']

# percentage of words removed
(1 - uniqueN(article_word_count$word) / nb_words_baseline) * 100

# cast to document-term-matrix
(dtm_tfidf <- tidytext::cast_dtm(data = article_word_count, 
                              document = article_id, 
                              term = word, 
                              value = count))

# save result and clean session
saveRDS(dtm_tfidf, 'data/dtm/dtm_tfidf.rds')
rm(article_word_count, word_tfidf, dtm_tfidf)

# < 1% removed, sparsity = 100%


# Mix ---------------------------------------------------------------------

# words are removed using the pos tags method and the idf method 

# pos tag method
article_word_count <- article_word[pos %in% c('ADJ', 'ADV', 'NOM', 'NAM') | 
                                     stringi::stri_detect_regex(str = pos, pattern = '^VER'),
                                   .(count = .N), by = list(article_id, word)]

# idf method
word_idf <- article_word_count[, .(idf = log(nrow(articles) / uniqueN(article_id))), by = word]
word_idf <- word_idf[idf == max(idf) | idf == 0]
article_word_count <- article_word_count[!word_idf, on = 'word']

# percentage of words removed
(1 - uniqueN(article_word_count$word) / nb_words_baseline) * 100

# cast to document-term-matrix
(dtm_mix <- tidytext::cast_dtm(data = article_word_count, 
                                 document = article_id, 
                                 term = word, 
                                 value = count))

# save result and clean session
saveRDS(dtm_mix, 'data/dtm/dtm_mix.rds')
rm(article_word_count, word_idf, dtm_mix)

# 52% removed, sparsity = 100%

