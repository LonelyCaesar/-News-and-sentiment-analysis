#非負矩陣分解
newspaper.words <- tidy.articles %>%
  group_by(title , word) %>% 
  summarise(word_frequency =  n()) %>%
  ungroup()

total.count <- tidy.articles %>%
  group_by(word) %>% 
  summarise(total_count = n())

newspaper.words <- newspaper.words %>%
  left_join(total.count, by = "word") %>%
  bind_tf_idf(word, title, word_frequency) %>%
  filter(nchar(word) > 1) %>%
  group_by(title) %>%
  top_n(10, tf_idf)

head(newspaper.words)

# Transform into Document-Term Matrix by Word Frequency
article.dtm <- newspaper.words %>%
  select(title, word, word_frequency) %>%
  spread(word, word_frequency)
head(article.dtm, 5)

# Fill in NA values
article.dtm[is.na(article.dtm)] <- 0

# Give document names
row.names(article.dtm) <- article.dtm$title

article.dtm$title <- NULL

head(article.dtm, 5)

library(NMF)

article.nmf <- nmf(article.dtm, 9, "KL") 

# Get W and H Matrix 
w <- as.data.frame(basis(article.nmf))
head(w, 5)

h <- as.data.frame(t(coef(article.nmf)))
h$word <- row.names(h)
head(h, 5)

# Visualize topics
temp <- h %>%
  gather(key = "topic", value = "score", V1, V2, V3, V4, V5, V6, V7, V8, V9)
head(temp, 5)

h %>%
  gather(key = "topic", value = "score", V1, V2, V3, V4, V5, V6, V7, V8, V9) %>%
  group_by(topic) %>%
  top_n(10, score) %>% # Select top 10 words for each topic 
  ungroup() %>%
  ggplot(aes(reorder(word, -score), score, fill = topic)) +
  geom_col(show.legend = FALSE) + # Show barchats
  labs(x = NULL, y = "NFM Score") +
  facet_wrap(~topic, ncol = 3, scales = "free") +
  coord_flip() +
  theme_bw() +
  theme(text=element_text(family="黑體-繁 中黑", size=14),
        axis.text.x = element_text(angle = 60, hjust = 1))