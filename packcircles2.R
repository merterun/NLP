library(packcircles)
library(ggplot2)
library(viridis)

# Create a function to generate the circle packing chart for each emotion
generate_circle_packing_chart <- function(emotion) {
  # Filter the data for the given emotion and limit the number of words to 250
  data <- all_clean %>%
    filter(emotion == !!emotion) %>%
    count(word) %>%
    top_n(250, n) %>%
    arrange(desc(n))
  
  # Generate the layout
  packing <- circleProgressiveLayout(data$n, sizetype = 'area')
  data <- cbind(data, packing)
  dat.gg <- circleLayoutVertices(packing, npoints = 50)
  
  # Set the bubble color
  bubble_color <- viridis(nrow(data), option = "A")
  
  # Generate the chart
  ggplot() + 
    geom_polygon(data = dat.gg, aes(x, y, group = id, fill = as.factor(id)), colour = "black", alpha = 0.6) +
    scale_fill_manual(values = bubble_color) +
    geom_text(data = data, aes(x, y, label = word, size = n, color = 'contrast'), 
              fontface = 'bold', show.legend = FALSE) +
    scale_size_continuous(range = c(1, 4)) +
    scale_color_manual(values = 'white', guide = FALSE) +
    ggtitle(emotion) +
    theme_void() + 
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),
          text = element_text(color = 'black'),
          panel.background = element_rect(fill = 'white', color = 'white'),
          legend.position = "none") +
    coord_equal()
}

# Generate the circle packing chart for each emotion
for (emotion in unique(all_clean$emotion)) {
  generate_circle_packing_chart(emotion)
}
