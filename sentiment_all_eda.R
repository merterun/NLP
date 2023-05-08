
# Calculate sentence length in characters
all_len <- all %>%
  mutate(sent_len = nchar(text))

# Calculate word count
all_len <- all_len %>%
  mutate(word_count = str_count(text, "\\S+"))

# Group by emotion and calculate mean and standard deviation of sentence length and word count
stats <- all_len %>%
  group_by(emotion) %>%
  summarize(mean_sent_len = mean(sent_len), 
            sd_sent_len = sd(sent_len),
            mean_word_count = mean(word_count), 
            sd_word_count = sd(word_count))

# Print the results
table(stats)


ggplot(data = stats, aes(x = your_variable)) +
  geom_density(aes(fill = your_grouping_variable), alpha = 0.5) +
  labs(title = "Density Chart of Sentence Length/Word Count by Emotion",
       x = "Sentence Length/Word Count",
       y = "Density") +
  scale_fill_discrete(name = "Emotion")


# Reshape the data frame into a "long" format









# Create a boxplot for mean sentence length and standard deviation of sentence length by emotion
sent_len_plot <- ggplot(stats, aes(x = emotion, y = mean_sent_len)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.data = mean_sdl, mult = 1,
               geom = "errorbar", width = 0.2) +
  labs(title = "Boxplot of Mean and Standard Deviation of Sentence Length by Emotion",
       x = "Emotion",
       y = "Sentence Length") +
  theme_classic()

# Create a boxplot for mean word count and standard deviation of word count by emotion
word_count_plot <- ggplot(stats, aes(x = emotion, y = mean_word_count)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.data = mean_sdl, mult = 1,
               geom = "errorbar", width = 0.2) +
  labs(title = "Boxplot of Mean and Standard Deviation of Word Count by Emotion",
       x = "Emotion",
       y = "Word Count") +
  theme_classic()

# Combine the two plots
gridExtra::grid.arrange(sent_len_plot, word_count_plot, ncol = 2)


