
# different ways of plotting a transfer matrix (or confusion matrix)
# the data comes from a lda model being omputed on the articles of lemonde

# Packages ----------------------------------------------------------------

library('googleVis')
library('ggplot2')
library('data.table')


# Data --------------------------------------------------------------------

# what topic for what article?
article_topic <- readRDS('data/article_topic.rds')
article_topic$topic <- paste('topic', article_topic$topic)

# by category percentage
article_topic[, by_category := round(value / sum(value) * 100, digits = 1), by = topic]

# by topic percentage
article_topic[, by_topic := round(value / sum(value) * 100, digits = 1), by = category]


# Sankey chart ------------------------------------------------------------

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

# to change color on hover : https://stackoverflow.com/questions/38051690/google-sankey-diagram-change-link-color-on-node-click

plot(sankey)


# Heatamp -----------------------------------------------------------------

# keep only upper triangle
heatmap_data <- article_topic

# heatmap
ggplot(data = heatmap_data, 
       mapping = aes(x = topic, y = category)) +
  geom_tile(mapping = aes(fill = value), color = "grey") +
  geom_text(mapping = aes(label = value)) + 
  scale_fill_gradient(low = "gray90", 
                      high = "dodgerblue",
                      name = "Number of\narticles") +
  theme_minimal() + 
  labs(x = 'LDA topics', y = 'lemonde.fr\ncategories', title = 'Number of articles by topic/category') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.position = 'none',
        panel.grid.major = element_blank()) + 
  coord_fixed()
