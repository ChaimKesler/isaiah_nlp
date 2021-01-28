library(tidyverse)
library(tidytext)
library(tm)
library(ggplot2)
library(cluster)

# Start with a clean  R Env
rm(list=ls())

# Import and reshape the data - working with JSON isn't easy
jsonfile <- jsonlite::fromJSON("isaiah.json")

chapters <- 24:27

tb_text <- jsonfile$text[chapters] %>% tibble()
names(tb_text) <- "text"

clean_unlist <-
  function(df, column, i, chap_list) {
    df <- tibble(df[i, column] %>% unlist())
    names(df) <- "text"
    df$chapter <- rep(chap_list[i], nrow(df))
    df$verse <- 1:nrow(df)
    df
  }

masterdf <- NULL
for(i in 1:nrow(tb_text)){  # Yes, I know this isn't the most "R" way to do this
  masterdf <- bind_rows(masterdf, clean_unlist(tb_text, "text", i, chapters))
}

# Let's look a word count

analysisdf <- masterdf  %>% unnest_tokens(word, text)

# Remove stop words which provide limited meaning
data(stop_words)
analysisdf <- analysisdf %>% anti_join(stop_words)

custom_stop <- tibble(c("and", "behold", "hath", "thee", "thou", "thy", "thereof", "ye", "yea"))
names(custom_stop) <- "word"
custom_df <- analysisdf %>% anti_join(custom_stop)


# Get a count and plot of the top words
analysisdf %>% 
  count(word, sort = TRUE)

plotdf <-
  analysisdf %>% 
    group_by(chapter) %>%
    count(word, sort = TRUE)

total_words <- plotdf %>% 
  group_by(chapter) %>% 
  summarize(total = sum(n))

plotdf <- left_join(plotdf, total_words)

plotdf %>%
  filter(n > 1) %>%
  ggplot(aes(n, word, fill=as.factor(chapter))) + 
  geom_bar(position="stack",stat = "identity") + 
  labs(y = NULL) +
  scale_fill_brewer(palette="Set2")

# Let's look at term frequency
ggplot(plotdf, aes(n/total, fill = chapter)) +
  geom_histogram(show.legend = FALSE, bins = 10) +
  facet_wrap(~chapter, ncol = 2)

# Clustering
# https://books.psychstat.org/textmining/cluster-analysis.html
dtm_df <- plotdf %>% cast_dtm(chapter, word, n)
dtm_df.subset <- removeSparseTerms(dtm_df, 0.4)
kmeans.data <- as.matrix(t(dtm_df.subset))
model <- kmeans(kmeans.data, 4)

clusplot(kmeans.data, model$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

with(plotdf, pairs(kmeans.data, col=c(1:3)[model$cluster])) 

# With Custom Stop
# Get a count and plot of the top words
custom_df %>% 
  count(word, sort = TRUE)

plotdf_custom <-
  custom_df %>% 
  group_by(chapter) %>%
  count(word, sort = TRUE)

total_words_custom <- plotdf_custom %>% 
  group_by(chapter) %>% 
  summarize(total = sum(n))

plotdf_custom <- left_join(plotdf_custom, total_words_custom)


# Let's look at term frequency

# Clustering
# https://books.psychstat.org/textmining/cluster-analysis.html
dtm_df_custom <- plotdf_custom %>% cast_dtm(chapter, word, n)
dtm_df_custom.subset <- removeSparseTerms(dtm_df_custom, 0.4)
kmeans.data_custom <- as.matrix(t(dtm_df_custom.subset))

model <- kmeans(kmeans.data_custom, 5)
clusplot(kmeans.data_custom, model$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)
with(plotdf, pairs(kmeans.data_custom, col=c(1:5)[model$cluster]))

save.image()
