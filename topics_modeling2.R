library(topicmodels)

train_lem <- train_lem %>%
  mutate(document = row_number())

# create a document-term matrix from the train_lem data
dtm_train <- train_lem %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

# find the optimal number of topics using coherence score
k <- findTopicsNumber(dtm_train, topics = seq(5, 50, 5), metrics = "CaoJuan2009")

# print the coherence score for each number of topics
plot(k, type = "topics")

# set the optimal number of topics based on the plot and fit the LDA model
lda_model <- LDA(dtm_train, k = 15, control = list(seed = 1234))


install.packages("topicmodels", type = "source")
install.packages("ldatuning", type = "source")
install.packages("Rmpfr")
