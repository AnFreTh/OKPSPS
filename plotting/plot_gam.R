# plotting/plot_gam.R

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to plot the GAM results
plot_gam <- function(data, model, knots, normal_model = NULL) {
  plot_data <- data
  plot_data$spline <- predict(model, newdata = plot_data)
  if (!is.null(normal_model)) {
    plot_data$normal_spline <- predict(normal_model, newdata = plot_data)
  }
  knot_data <- data.frame(x = knots, y = predict(model, newdata = data.frame(x = knots)))
  
  p <- ggplot() +
    geom_point(data = data, aes(x = x, y = y), shape = 16, size = 1, col = rgb(0, 0, 1, alpha = 0.4)) +
    geom_line(data = plot_data, aes(x = x, y = spline), color = "black", size = 1.5)
  if (!is.null(normal_model)) {
    p <- p + geom_line(data = plot_data, aes(x = x, y = normal_spline), color = "red", size = 1, linetype = "longdash")
  }
  p <- p + geom_point(data = knot_data, aes(x = x, y = y), shape = 17, size = 3, color = "chartreuse3") +
    labs(x = "x", y = "Spline Value") +
    theme_minimal()
  
  print(p)
}
