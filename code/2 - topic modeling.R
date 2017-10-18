
# Topic modeling is performed on the articles from www.lemonde.fr The goal is
# to know if the LDA method can properly classify the articles based on their
# topics on the website

# The corpus is first tagged and lemmatized. 
# Then stopwords are removed.
# Finally the LDA model is applied on the corpus.
# Step 2 and 3 are repeated and several models are obtained

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
articles$text <- stri_replace_all_fixed(str = articles$text, pattern = apostrophe, replacement = "'")

# tokenization and lemmatization are done using treetagger. 
article_word <- lapply(X = 1:nrow(articles),
                             FUN = function(i) lemmatize(text = articles$text[i], id = articles$id[i], filter = FALSE))
article_word <- do.call(rbind, article_word)
article_word <- article_word[, .(article_id = id, word = lemma, pos)]

# punctuation marks, symbols and sentence marks are removed
article_word <- article_word[!pos %in% c('PUN', 'PUN:cit', 'SENT', 'SYM')]

# save result and clean session
saveRDS(article_word, 'data/article_word.rds')
rm(apostrophe, article_word, articles)


# Stopwords removal -------------------------------------------------------

# several stopwords removal methods will be applied on the corpus. The results
# are saved and the LDA is then performed on these new corpus.

# The methods are : bag of words, pos tags, TF1, IDF, mix of all

# tagged corpus 
article_word <- readRDS('data/article_word.rds')


# Bag of words ------------------------------------------------------------

# words are grouped by article 
article_word_count <- article_word[, .(count = .N), by = list(article_id, word)]

# load the stopwords list and anti-join it with the corpus
stopwords <- readLines('data/dtm stopwords/stopwords_fr.txt')
stopwords <- data.table(word = stopwords)
article_word_count <- article_word_count[!stopwords, on = 'word']

# cast to document-term-matrix
articles_dtm_bow <- tidytext::cast_dtm(data = article_word_count, document = article_id, term = word, value = count)

# save result and clean session
saveRDS(articles_dtm_bow, 'data/dtm stopwords/articles_dtm_bow.rds')
rm(article_word_count, stopwords, articles_dtm_bow)


# POS tags ----------------------------------------------------------------

# only the tags ADJ, ADV, NOM, NAM, and VER: are kept
article_word_count <- article_word[pos %in% c('ADJ', 'ADV', 'NOM', 'NAM') | 
                                     stringi::stri_detect_regex(str = pos, pattern = '^VER'), 
                                   .(count = .N), 
                                   by = list(article_id, word)]

# cast to document-term-matrix
articles_dtm_pos <- tidytext::cast_dtm(data = article_word_count, document = article_id, term = word, value = count)

# save result and clean session
saveRDS(articles_dtm_pos, 'data/dtm stopwords/articles_dtm_pos.rds')
rm(article_word_count, articles_dtm_pos)


# TF1 ---------------------------------------------------------------------

# words that appear only once in the corpus are removed
word_count <- article_word[, .(count = .N), by = word]
word_count <- word_count[count == 1, .(word)]
article_word_count <- article_word[, .(count = .N), by = list(article_id, word)]
article_word_count <- article_word_count[!word_count, on = 'word']

# cast to document-term-matrix
articles_dtm_tf1 <- tidytext::cast_dtm(data = article_word_count, document = article_id, term = word, value = count)

# save result and clean session
saveRDS(articles_dtm_tf1, 'data/dtm stopwords/articles_dtm_tf1.rds')
rm(article_word_count, word_count, articles_dtm_tf1)


# TF-IDF ------------------------------------------------------------------


# using idf
# inverse document frequency is the log of number of documents / numbers of documents with the word
n_doc <- uniqueN(article_word_count$article_id)
article_word_count[, idf := uniqueN(article_id) / n_doc, by = word]

# clean session
rm(article_word_count, stopwords, apostrophe)


