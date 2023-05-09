# Load the required libraries
library(topicmodels)
library(tm)

# Add count column
train_lem <- train_lem %>%
  mutate(count = 1)

# Define the number of topics for each emotion
num_topics <- 6

# Create DocumentTermMatrix
dtm <- DocumentTermMatrix(train_lem, control = list(minWordLength = 3))


# Fit LDA model
lda_model <- LDA(dtm, k = num_topics, method = "Gibbs", control = list(seed = 1234))

# Show the top 10 terms for each topic
top_terms <- terms(lda_model, 10)
top_terms
