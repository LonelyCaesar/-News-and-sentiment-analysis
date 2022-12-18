library(topicmodels)
article.lda <- LDA(article.dtm, k = 9, control = list(seed = 1234))
h <- tidy(article.lda, matrix = "beta")
head(h)

w <- tidy(article.lda, matrix = "gamma")
head(w)

h %>%
  group_by(topic) %>%
  filter(beta > 0) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  ggplot(aes(reorder(term, -beta), beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "LDA Score") +
  facet_wrap(~topic, ncol = 3, scales = "free") +
  coord_flip() +
  theme_bw() +
  theme(text=element_text(family="黑體-繁 中黑", size=14),
        axis.text.x = element_text(angle = 60, hjust = 1))

library(LDAvis)
library(slam)
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}
serVis(topicmodels2LDAvis(article.lda))