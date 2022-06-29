
#'
#' This function plots the distribution of the gamma probabilities on each category for
#' all topics using boxplot
#'
#' @param model an object of class LDA
#' @param categories table with categories associated to the articles
#' @param subcategory logical, use subcategories instead of categories
#'   
plotGammaDistribution <- function(model, categories) {
  article_topic <- tidy(model, matrix = "gamma")
  setDT(article_topic)
  article_topic <- merge(x = article_topic, 
                         y = categories, 
                         by.x = 'document',
                         by.y = 'id',
                         allow.cartesian = TRUE)
  ggplot(data = article_topic, mapping = aes(x = category, y = gamma)) + 
    geom_boxplot(outlier.size = 0.2, size = 0.2) + 
    coord_flip() + 
    facet_wrap(~ topic, scales = 'free') + 
    labs(x = '', y = 'Gamma median', 
         title = 'Gamma distribution for each category for all topics.', 
         subtitle = 'Articles found in several categories are duplicated.')
}

#'
#' This function plots the distribution of the gamma probabilities on each subcategory
#' with gamma mean higher than the overall mean for all topics using boxplot
#'
#' @param model an object of class LDA
#' @param categories table with categories associated to the articles
#' @param subcategory logical, use subcategories instead of categories
#'   
plotGammaDistribution2 <- function(model, categories) {
  article_topic <- tidy(model, matrix = "gamma")
  setDT(article_topic)
  article_topic <- merge(x = article_topic, 
                         y = categories, 
                         by.x = 'document',
                         by.y = 'id',
                         allow.cartesian = TRUE)
  gamma_threshold <- article_topic[, .(gamma_threshold = mean(gamma)), by = topic]
  article_topic <- merge(x = article_topic, 
                         y = gamma_threshold, 
                         by = 'topic')
  article_topic[, gamma_mean := mean(gamma), by = list(topic, subcategory)]
  article_topic <- article_topic[gamma_mean > gamma_threshold]
  ggplot(data = article_topic, mapping = aes(x = subcategory, y = gamma)) + 
    geom_boxplot(outlier.size = 0.2, size = 0.2) + 
    coord_flip() + 
    facet_wrap(~ topic, scales = 'free') + 
    labs(x = '', y = 'Gamma median', 
         title = 'Gamma distribution for each subcategory for all topics.', 
         subtitle = 'Articles found in several categories are duplicated. Only subcategories with gamma mean are displayed.')
  
}

#'
#' This function extracts the top beta probabilities from the LDA model object
#' and plot them by topic.
#'
#' @param model an object of class LDA
#' @param use_diff boolean, should the words with the max difference in
#'   probability be returned instead of simply the max probability?
#' @param n the number of words by topic to return
#'
#' @return a table and a plot
#'
#' @details Words with the highest beta probability are returne dif use_diff is
#'   FALSE. Else words the greatest difference between their probability for the
#'   given topic and the overall mean, which is 1 divided by the number of
#'   words. 
#'   
plotTopWords <- function(model, use_diff = FALSE, n = 10) {
  topic_term <- tidy(model, matrix = "beta")
  setDT(topic_term)
  if (use_diff) {
    topic_term[, beta := beta - mean(beta), by = term]
  }
  topic_term <- topic_term[order(-beta), head(.SD, n), by = topic]
  topic_term[, term := reorder(term, beta)]
  invisible(topic_term)
  ggplot(data = topic_term, mapping = aes(x = term, y = beta)) +
    geom_point(show.legend = FALSE) + 
    geom_segment(mapping = aes(xend = term, yend = 0), show.legend = FALSE) + 
    facet_wrap(~ topic, scales = "free") + 
    coord_flip()
}


confusionMatrix <- function(model, gamma_threshold = NULL, sankey = FALSE) {
  article_topic <- tidytext::tidy(articles_lda, matrix = "gamma")
  setDT(article_topic)
  if(is.null(gamma_threshold)) {
    article_threshold <- article_topic[, .(median = median(gamma), sd = sd(gamma)), 
                                       by = document]
    gamma_threshold <- mean(article_topic$gamma) + sd(article_topic$gamma)
  }
  article_topic <- article_topic[gamma >= gamma_threshold]
  excluded <- (nrow(articles) - uniqueN(article_topic$document)) / nrow(articles) * 100
  article_topic[, nb_topic := uniqueN(topic), by = document]
  article_topic_top <- article_topic[, .(topic = topic[which.max(gamma)], 
                                         gamma = max(gamma)), 
                                     by = document]
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
  article_confusion <- article_topic_top[, .(value = uniqueN(article_id), gamma = mean(gamma)), 
                                         by = list(topic, category)][order(-value)]
  article_confusion[, topic_percent := round(value / sum(value) * 100, digits = 1), by = topic]
  article_confusion[, category_percent := round(value / sum(value) * 100, digits = 1), by = category]
  article_confusion$topic <- paste('topic', article_confusion$topic)
  invisible(article_confusion)
  if (!sankey) {
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
  } else {
    sankey <- googleVis::gvisSankey(
      data = article_topic, 
      from = "topic", 
      to = "category", 
      weight = "value",
      options = list(
        sankey = "{
    link: {colorMode: 'gradient' },
    node: { color: { fill: '#a61d4c' },
    label: { color: '#871b47' } }}"
      ),
      chartid = 'sankey_plot'
    )
    plot(sankey)
  }
}


















