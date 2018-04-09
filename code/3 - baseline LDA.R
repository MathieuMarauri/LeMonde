
# Topic modeling is performed on the document-term-matrices obtained after the
# pre-processing step. First the distribution of the topics over the words is analysed.
# Topics will be considered well defined if words with the highest probabilities are
# different enough across topics. Then the distribution of the document over the topics is
# analysed. It will be compared to the subcategories obtained from the website. Articles
# taht are 'misclassified' will be further analysed.

# LDA method is described here (https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation)
# and here
# (https://endymecy.gitbooks.io/spark-ml-source-analysis/content/%E8%81%9A%E7%B1%BB/LDA/docs/Latent%20Dirichlet%20Allocation.pdf).
# A more concise explanation can be found here
# (http://blog.echen.me/2011/08/22/introduction-to-latent-dirichlet-allocation/).

# The baseline document term matrix is used to defined a proper process of
# analysis. Other document term matrices will be analysed later. 

# Packages ----------------------------------------------------------------

library('data.table') # dataset manipulation
library('stringi') # string manipulation
library('ggplot2') # data visualisation
library('topicmodels') # lda class
library('tidyr') # tidy lda objects
source('code/helpers/3 - model interpretation.R')


# Corpus ------------------------------------------------------------------

articles <- readRDS('data/articles.rds')

# keep only relevant variables
articles <- articles[, .(id = as.character(id), text, category, subcategory)]

# transform category and subcategory to keep only the last item
articles[, subcategory_main := stri_replace_all_regex(subcategory, '.*/', '')]
articles[, category_main := stri_replace_all_regex(category, '/.*', '')]

# number of articles by subcategories 
articles_subcategory <- articles[, .(nb_articles = .N), by = subcategory_main]
articles_category <- articles[, .(nb_articles = .N), by = category_main]

# clean session
rm(articles_subcategory, articles_category)


# LDA model ----------------------------------------------------------

# The LDA method is applied on the document term matrix obtained from the different
# stopwords removal methods. The first one used is the baseline. The number of topics is
# first selected with the number of subcategories in the corpus (8), another try will be
# given with an higher number.

dtm_baseline <- readRDS('data/dtm/dtm_baseline.rds')

articles_lda <- LDA(x = dtm_baseline, 
                    k = 8, 
                    method = 'VEM', 
                    control = list(seed = 1234, 
                                   verbose = 5))

# save results and clean session
saveRDS(articles_lda, 'data/lda/lda_baseline.rds')
rm(dtm_baseline)

# import model results
articles_lda <- readRDS('data/lda/lda_baseline.rds')


# Beta probabilities ------------------------------------------------------

# In this section the beta probabilities are exported from the model. It is the
# distribution of a topic on the words. It will highlight the words used to define topics.
# From this analyse topics should be clear enough so that a category can be assigned to
# them. The goal is to answer the following question: what are the words most associated
# with the topics? The per-topic-per-term probabilities are used.

# extract beta probabilities from the model
topic_term <- tidy(articles_lda, matrix = "beta")
setDT(topic_term)

# First the highest probabilities by topics are extracted. 
topic_term_top10 <- topic_term[order(-beta), head(.SD, 10), by = topic]
topic_term_top10[, term := reorder(term, beta)]

ggplot(data = topic_term_top10, mapping = aes(x = term, y = beta)) +
  geom_point(show.legend = FALSE) + 
  geom_segment(mapping = aes(xend = term, yend = 0), show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()

# Since no stopwords removal method has been used, the highest probabilitis are for words
# with no inforamtion. We now extract, by topic, the words with the highest difference in
# probability between the given topic and the mean probabilities on the others. This way
# the words most associated with one topic and not the others will be extracted.

# compute the difference between onte topic and the mean on all other topics. Since we
# want the words with the maximum difference we can compute the difference between a topic
# and the global mean.
topic_term[, beta_diff := beta - mean(beta), by = term]
topic_term_diff10 <- topic_term[order(-beta_diff), head(.SD, 10), by = topic]
topic_term_diff10[, term := reorder(term, beta_diff)]

ggplot(data = topic_term_diff10, mapping = aes(x = term, y = beta)) +
  geom_point(show.legend = FALSE) + 
  geom_segment(mapping = aes(xend = term, yend = 0), show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()

# Even with words with the greatest difference with the mean on all topics, no clear
# topics arise. Topic 2 seems to be about politics and Europe, topic 4
# can be viewed as being about sport and topic 3 about Irma. The other topics contain only
# stopwords.

# clean session
rm(articles_lda, topic_term, topic_term_top10)


# Gamma probabilities -----------------------------------------------------

# In this section the gamma probabilities (or per-document-per-topic probabilities) are
# exported from the model. It is the distribution of the documents on the topics. We want
# to know what topics are present in which documents. By article, the topic with the
# highest probability is kept. A confusion matrix between the actual subcategories and the
# topics is drawn. We expect articles from a given topic to be mostly from the same
# subcategory. Even though articles can contain several topics we suppose that those case
# are rare enough so that we can have a clustering that is not fuzzy in most cases.

# extarct gamma probabilities from the model
article_topic <- tidy(articles_lda, matrix = "gamma")
setDT(article_topic)

# the topic with highest probability is extracted for every articles. In order not to
# select a topic that is only slighty more likely than what could be expected a threshold
# is defined. It is the mean + the standard deviation of all gamma probabilities. This way
# articles that are equivalently distributed oon all topics will not be assigned a main
# topic.
mean(article_topic$gamma)
sd(article_topic$gamma)
article_threshold <- article_topic[, .(median = median(gamma), sd = sd(gamma)), 
                                   by = document]
gamma_threshold <- mean(article_topic$gamma) + sd(article_topic$gamma)
rm(article_threshold)

# filter gamma probabilities that are under the threshold
article_topic <- article_topic[gamma >= gamma_threshold]
# how many articles are excluded by this threshlod?
(nrow(articles) - uniqueN(article_topic$document)) / nrow(articles) * 100
# how many topics by article?
article_topic[, nb_topic := uniqueN(topic), by = document]

# some articles have a probability of containing two different topics that is higher than
# the threshold. First we will consider only the most likely topic.
article_topic_top <- article_topic[, .(topic = topic[which.max(gamma)], 
                                       gamma = max(gamma)), 
                                   by = document]

# the subcategories are retrieved from the articles table so a confusion matrix can be
# constructed.
article_topic_top <- merge(x = article_topic_top, 
                           y = articles[, .(id, subcategory_main)],
                           by.x = 'document', 
                           by.y = 'id', 
                           all.x = TRUE,
                           all.y = FALSE)
article_topic_top <- article_topic_top[, .(article_id = document, 
                                           topic, 
                                           category = subcategory_main, 
                                           gamma)]

# a consufion matrix is build with percentage of articles by topic and by category
article_confusion <- article_topic_top[, .(value = uniqueN(article_id), gamma = mean(gamma)), 
                                       by = list(topic, category)][order(-value)]
article_confusion[, topic_percent := round(value / sum(value) * 100, digits = 1), by = topic]
article_confusion[, category_percent := round(value / sum(value) * 100, digits = 1), by = category]
article_confusion$topic <- paste('topic', article_confusion$topic)

# plot of confuson matrix
ggplot(data = article_confusion, 
       mapping = aes(x = topic, y = category)) +
  geom_tile(mapping = aes(fill = value), color = "grey") +
  geom_text(mapping = aes(label = value)) + 
  scale_fill_gradient(low = "gray90", 
                      high = "dodgerblue",
                      name = "Number of\narticles") +
  theme_minimal() + 
  labs(x = 'LDA topics', y = 'lemonde.fr\ncategories', 
       title = 'Number of articles by topic/category') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.position = 'none',
        panel.grid.major = element_blank()) + 
  coord_fixed()

# Some topics are clearly associated with one category from lemonde.fr (topics 3 and 7).
# 99.2% of the articles classified in topic 3 come from the football category which falls
# in the topic 3 in 85.6% of the cases. 84.5% of the articles from the category
# 'les-enfants-akira' fall in the topic7. 61% of articles whose main topic is the number 1
# are from the category 'energie'. The articles from the category 'energie' are in the
# topic 1 segment for 49.6% of the cases. As expected topic 6 concerns articles from the
# economy and politics categories. Topic 2 and the category culture are groups that have
# been the less discriminated.

rm(article_confusion, article_topic_top)

