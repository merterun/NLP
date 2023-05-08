# Load data
test <- read.delim("test.txt", header = FALSE, stringsAsFactors = FALSE, sep = ";", col.names = c("text", "emotion"))
train <- read.delim("train.txt", header = FALSE, stringsAsFactors = FALSE, sep = ";", col.names = c("text", "emotion"))
val <- read.delim("val.txt", header = FALSE, stringsAsFactors = FALSE, sep = ";", col.names = c("text", "emotion"))

# Combine all data into one data frame
all <- rbind(test, train, val)

# Check out the data frames
head(val)
head(test)
head(train)

dim(test)
dim(train)
dim(val)


library(tidyverse)

library(stringr)

# Check for uppercase characters
grepl("^[[:upper:]]+$", test)
grepl("^[[:upper:]]+$", train)
grepl("^[[:upper:]]+$", val)


library(RColorBrewer)

# Count the number of occurrences of each emotion
counts <- all %>%
  group_by(emotion) %>%
  summarize(count = n())

counts

# Get unique emotions from data_all
unique_emotions <- unique(all$emotion)

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

library(tidytext)

# load the stop words from the tidytext package
stop_words <- tidytext::stop_words

# remove stop words from the text column in each data frame
test_clean <- test %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

train_clean <- train %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

val_clean <- val %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

all_clean <- bind_rows(train_clean, test_clean, val_clean)

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
