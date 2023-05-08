# Load data
test <- read.delim("test.txt", header = FALSE, stringsAsFactors = FALSE, sep = ";", col.names = c("text", "emotion"))
train <- read.delim("train.txt", header = FALSE, stringsAsFactors = FALSE, sep = ";", col.names = c("text", "emotion"))
val <- read.delim("val.txt", header = FALSE, stringsAsFactors = FALSE, sep = ";", col.names = c("text", "emotion"))


# Check out the data frames
head(val)
head(test)
head(train)

dim(test)
dim(train)
dim(val)


library(tidyverse)

library(stringr)

# Combine all data into one data frame
all <- rbind(test, train, val)

# Check for uppercase characters
grepl("^[[:upper:]]+$", all)

library(tidytext)

# load the stop words from the tidytext package
stop_words <- tidytext::stop_words

# Count the number of occurrences of each emotion
counts <- all %>%
  group_by(emotion) %>%
  summarize(count = n())

counts

# Get unique emotions from data_all
unique_emotions <- unique(all$emotion)


library(RColorBrewer)

# Define the palette using unique emotions
my_palette <- colorRampPalette(brewer.pal(length(unique_emotions), "Paired"))(length(unique_emotions))
names(my_palette) <- unique_emotions


# Create bar chart
ggplot(counts, aes(x = emotion, y = count, fill = emotion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +
  scale_fill_manual(values = my_palette) +
  labs(title = "Emotion Total Word Counts",
       x = "Emotion",
       y = "Count") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "none")



# Remove stop words from the text column
all_clean <- all %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# group the words by emotion and count their frequency
emotion_counts <- all_clean %>%
  group_by(emotion) %>%
  count(word, sort = TRUE)

# show top 10 most counted words for each emotion
emotion_counts %>%
  group_by(emotion) %>%
  slice_max(n = 10, order_by = n) %>%
  ungroup()

head(emotion_counts)

all_clean <- all_clean %>%
  filter(!word %in% c("im", "feel", "feeling"))


library(packcircles)
library(viridis)

# Loop through emotions and create circle packing charts for the top 250 words
for (emotion in unique(all_clean$emotion)) {
  # Subset data for the current emotion
  data <- all_clean %>% 
    filter(emotion == !!emotion) %>% 
    count(word) %>% 
    top_n(250, n) %>% 
    arrange(desc(n))
  
  # Generate the layout
  packing <- circleProgressiveLayout(data$n, sizetype='area')
  data <- cbind(data, packing)
  dat.gg <- circleLayoutVertices(packing, npoints=50)
  
  # create plot
  plot <- ggplot() + 
    geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
    scale_fill_viridis(discrete = TRUE, option = "viridis") +
    geom_text(data = data, aes(x, y, size=n, label = word), check_overlap = TRUE, color = "black") +
    scale_size_continuous(range = c(1,8)) +
    theme_void() + 
    theme(legend.position="none") +
    coord_equal() +
    ggtitle(paste0("Most Used Words for Emotion: ", emotion)) +
    theme(plot.title = element_text(size = 20))  
  print(plot)
}


# Stemming
all_stem <- all_clean %>%
  mutate(word_stem = wordStem(word, language = "porter")) %>%
  select(-word) %>%
  rename(word = word_stem)

# Lemmatization
all_lem <- all_clean %>%
  mutate(word_lem = lemmatize_words(word, language = "en")) %>%
  select(-word) %>%
  rename(word = word_lem)

library(udpipe)

# download and load the English model for UDPipe
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# POS tagging using UDPipe
all_pos <- udpipe(all_lem$word, object = ud_model, tagger = "default")

head(all_pos)

# Identify the co-occurrence of emotions and words or phrases.

# Create a dataframe of emotion-word pairs
emotion_word <- all_pos %>%
  filter(upos %in% c("NOUN", "VERB", "ADJ")) %>%
  select(sent_id, word, emotion) %>%
  group_by(emotion, word) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Create a co-occurrence matrix
co_mat <- emotion_word %>%
  filter(count > 1) %>%
  spread(emotion, count, fill = 0)

# Visualize the co-occurrence matrix as a heatmap
ggplot(data = co_mat, aes(x = emotion, y = word, fill = joy)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Emotion", y = "Word", title = "Co-occurrence of Emotions and Words")


# Analyze the distribution of sentence length and word count across emotions.

# Create a dataframe of sentence lengths and word counts by emotion
sent_word_count <- all_pos %>%
  group_by(emotion, sent_id) %>%
  summarize(sent_len = n(), word_count = n_distinct(word)) %>%
  ungroup()

# Visualize the distribution of sentence lengths by emotion
ggplot(data = sent_word_count, aes(x = emotion, y = sent_len)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Emotion", y = "Sentence Length", title = "Distribution of Sentence Length by Emotion")

# Visualize the distribution of word counts by emotion
ggplot(data = sent_word_count, aes(x = emotion, y = word_count)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Emotion", y = "Word Count", title = "Distribution of Word Count by Emotion")


# Explore potential correlations between emotions and other variables in the dataset.

# Create a dataframe of emotion and age
emotion_age <- all_clean %>%
  select(sent_id, emotion) %>%
  left_join(metadata, by = "sent_id") %>%
  select(emotion, age)

# Visualize the distribution of age by emotion
ggplot(data = emotion_age, aes(x = emotion, y = age)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Emotion", y = "Age", title = "Distribution of Age by Emotion")

