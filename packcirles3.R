# Create separate circle packing charts for each emotion
emotions <- unique(all_clean$emotion)

for (e in emotions) {
  # Filter the words and their frequency count for the current emotion
  words <- all_clean %>%
    filter(emotion == e) %>%
    count(word) %>%
    filter(!word %in% emotions)  # Remove any emotion words from the list
  
  # Generate the layout
  packing <- circleProgressiveLayout(words$n, sizetype = 'area')
  words <- cbind(words, packing)
  dat.gg <- circleLayoutVertices(packing, npoints = 50)
  
  # Create the plot
  ggplot() +
    geom_polygon(data = dat.gg, aes(x, y, group = id, fill = as.factor(id)), color = "black", alpha = 0.6) +
    scale_fill_viridis(discrete = TRUE, option = "viridis") +
    geom_text(data = words, aes(x, y, size = n, label = word), color = "white") +
    scale_size_continuous(range = c(1, 4)) +
    theme_void() +
    theme(legend.position = "none") +
    ggtitle(paste("Circle packing chart for", e, "emotion")) +
    coord_equal() -> plot
  print(plot)
}