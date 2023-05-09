library(SnowballC)
library(textstem)

# Stemming
test_stem <- test_clean %>%
  mutate(word_stem = wordStem(word, language = "porter")) %>%
  select(-word) %>%
  rename(word = word_stem)

train_stem <- train_clean %>%
  mutate(word_stem = wordStem(word, language = "porter")) %>%
  select(-word) %>%
  rename(word = word_stem)

val_stem <- val_clean %>%
  mutate(word_stem = wordStem(word, language = "porter")) %>%
  select(-word) %>%
  rename(word = word_stem)


# Lemmetization
test_lem <- test_clean %>%
  mutate(word_lem = lemmatize_words(word, language = "en")) %>%
  select(-word) %>%
  rename(word = word_lem)

train_lem <- train_clean %>%
  mutate(word_lem = lemmatize_words(word, language = "en")) %>%
  select(-word) %>%
  rename(word = word_lem)

val_lem <- val_clean %>%
  mutate(word_lem = lemmatize_words(word, language = "en")) %>%
  select(-word) %>%
  rename(word = word_lem)
