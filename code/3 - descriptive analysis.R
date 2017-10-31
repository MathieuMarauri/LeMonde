
# A description analysis is performed on the corpus. The goal is to have a
# better understanding of the data that was extracted. How many articles by
# category there are, what the top words are, ...


# Packages ----------------------------------------------------------------

library('data.table')
library('stringi')


# Corpus ------------------------------------------------------------------

# First we focus on the metadata, the text will be analyzed later. 

# import the corpus
articles_raw <- readRDS('data/articles.rds')

# only keep metadata
articles_meta <- articles_raw[, .(id, date, time, author, topic, subtopic)]


# Analysis of metadata ----------------------------------------------------

# topic
topic <- articles_meta[, .(count = .N), by = topic]
