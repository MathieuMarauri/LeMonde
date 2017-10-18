
# Topic modeling is performed on the articles from www.lemonde.fr The goal is
# to know if the LDA method can properly classify the articles based on their
# topics on the website

# Packages ----------------------------------------------------------------

library('data.table')
library('stringi')
library('ggplot2')
source('code/functions.R')


# Data pre-processing -----------------------------------------------------

articles <- readRDS('data/articles.rds')

# keep only id and text
articles <- articles[, .(id = as.character(id), text, category = stri_replace_all_regex(topic, '/.*', ''), other_category = topic, subcategory = subtopic)]

# replace apostrophe by proper character symbol so treetagger can properly
# tokenize words
apostrophe <- substr(articles$text[21], start = 4, 4)
articles$text <- stri_replace_all_fixed(str = articles$text, pattern = apostrophe, replacement = "'")

# tokenization and lemmatization are done using treetagger. A table with word
# count by article is obtained and cast as a document-term matrix while
# lemmatization is done, words with tags different than ADJ, ADV, NOM and VER
# are removed. Besides être and avoir are also removed
# article_word_count <- lapply(X = 1:nrow(articles), 
#                              FUN = function(i) lemmatize(text = articles$text[i], id = articles$id[i]))
# article_word_count <- do.call(rbind, article_word_count)
# saveRDS(article_word_count, 'data/article_word_count.rds')
article_word_count <- readRDS('data/article_word_count.rds')

# stopwords are removed using a pre-defined bag of words
stopwords <- readLines('data/stopwords_fr.txt')
stopwords <- data.table(lemma = stopwords)
article_word_count <- article_word_count[!stopwords, on = 'lemma']

# cast to document-term-matrix
articles_dtm <- tidytext::cast_dtm(data = article_word_count, document = id, term = lemma, value = count)

# clean session
rm(article_word_count, stopwords, apostrophe)


# Topic modeling ----------------------------------------------------------

# topic modeling is performed on the document-term matrix created with the
# articles. The number of topic is 9, the same as the number of categories found
# on the lemonde.fr.

# articles_lda <- topicmodels::LDA(articles_dtm, k = 9, control = list(seed = 1234))
# rm(articles_dtm)
# saveRDS(articles_lda, 'data/articles_lda.rds')


# Beta probabilities ------------------------------------------------------

# import model results
articles_lda <- readRDS('data/articles_lda.rds')

# per-topic-per-term probabilities: what are the words most associated with the
# topics?

# extract beta probabilities from the model
topic_term <- tidytext::tidy(articles_lda, matrix = "beta")
setDT(topic_term)

# top words by topic
topic_term_top10 <- topic_term[order(-beta), head(.SD, 10), by = topic]
topic_term_top10[, term := reorder(term, beta)]

ggplot(data = topic_term_top10, mapping = aes(x = term, y = beta)) +
  geom_point(show.legend = FALSE) + 
  geom_segment(mapping = aes(xend = term, yend = 0), show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()

# words that are most associated with one topic but not the others

# maximum difference between beta probability on one topic versus mean on all
# other topics
topic_term[, beta_diff := beta - mean(beta), by = term]

# top diff by topic
topic_term_diff10 <- topic_term[order(-beta_diff), head(.SD, 10), by = topic]
topic_term_diff10[, term := reorder(term, beta_diff)]

ggplot(data = topic_term_diff10, mapping = aes(x = term, y = beta)) +
  geom_point(show.legend = FALSE) + 
  geom_segment(mapping = aes(xend = term, yend = 0), show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()

# some topics seem to be clearly defined by the words defining them.
# topic 2 = sport
# topic 8 = health
# topic 6 = culture
# topic 7 = irma typhoon
# topic 3 = economy
# topic 5 = judiciary
# topic 9 = politics/catalan independance
# topic 1 = labour act
# topic 4 ?

# clean session
rm(articles_lda, topic_term, topic_term_top10)


# Gamma probabilities -----------------------------------------------------

# per-document-per-topic probabilities: what are the topics most associated with
# the articles ?

# Are the articles properly classified? First the most likely topic is retrieved
# for every article. Then a table gives the number of articles by couple
# opic/category at a given gamma threshold

# extarct gamma probabilities from the model
article_topic <- tidytext::tidy(articles_lda, matrix = "gamma")
setDT(article_topic)

# most likely topic with gamma probability higher than threshold
gamma_threshold <- 0.0
article_topic <- article_topic[gamma >= gamma_threshold]
article_topic <- article_topic[, .(topic = topic[which.max(gamma)], gamma = max(gamma)), by = document]
# number of article that are excluded
nrow(articles) - nrow(article_topic)

# get the category from the articles table 
article_topic <- merge(x = article_topic, 
                       y = articles[, .(id, category, other_category)],
                       by.x = 'document', 
                       by.y = 'id', 
                       all.x = TRUE,
                       all.y = FALSE)
article_topic <- article_topic[, .(article_id = document, topic, category, other_category, gamma)]

# number of articles by couple topic/category
article_topic <- article_topic[, .(value = uniqueN(article_id), gamma = mean(gamma)), by = list(topic, category)][order(-value)]

# number of article by topic and by category
article_topic[, topic_total := sum(value), by = topic]
article_topic[, category_total := sum(value), by = category]
article_topic[, c('topic_percent', 'category_percent') := list(round(value / topic_total * 100, digits = 1), round(value / category_total * 100, digits = 1))]
article_topic[, c('topic_total', 'category_total') := NULL]

rm(article_topic)
