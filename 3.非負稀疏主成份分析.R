library(nsprcomp)
nspca.model <- nsprcomp(article.dtm, ncomp = 9, k = 100, nneg = T)

summary(nspca.model)

h <- as.data.frame(nspca.model$rotation)
h$word <- row.names(h)
h %>%
  gather(key = "topic", value = "score",
         PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9) %>%
  group_by(topic) %>%
  filter(score > 0) %>%
  top_n(10, score) %>%
  ungroup() %>%
  ggplot(aes(reorder(word, -score), score, fill = topic)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "PCA Score") +
  facet_wrap(~topic, ncol = 3, scales = "free") +
  coord_flip() +
  theme_bw() +
  theme(text=element_text(family="黑體-繁 中黑", size=14),
        axis.text.x = element_text(angle = 60, hjust = 1))