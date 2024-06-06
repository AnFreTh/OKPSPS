# plotting/plot_gam.R

# Load necessary libraries
library(ggplot2)
library(dplyr)


# Function to plot the GAM results
plot_gam <- function(data, model, knots, normal_model = NULL) {
  plot_data <- data
  plot_data$spline <- predict(model, newdata = plot_data)
  if (!is.null(normal_model)) {
    plot_data$normal_spline <-
      predict(normal_model, newdata = plot_data)
  }
  knot_data <-
    data.frame(x = knots, y = predict(model, newdata = data.frame(x = knots)))
  
  p <- ggplot() +
    geom_point(
      data = data,
      aes(x = x, y = y),
      shape = 16,
      size = 1,
      col = rgb(0, 0, 1, alpha = 0.4)
    ) +
    geom_line(
      data = plot_data,
      aes(x = x, y = spline),
      color = "black",
      size = 1.5
    )
  if (!is.null(normal_model)) {
    p <-
      p + geom_line(
        data = plot_data,
        aes(x = x, y = normal_spline),
        color = "red",
        size = 1,
        linetype = "longdash"
      )
  }
  p <-
    p + geom_point(
      data = knot_data,
      aes(x = x, y = y),
      shape = 17,
      size = 3,
      color = "chartreuse3"
    ) +
    labs(x = "x", y = "Spline Value") +
    theme_minimal()
  
  print(p)
}



plot_multivariate_splines <-
  function(model, data, knots, steps = 100) {
    num_dims <- length(knots)
    plots <- list()
    
    for (i in seq_len(num_dims)) {
      x <-
        seq(min(data[[paste0("x", i)]]), max(data[[paste0("x", i)]]), length.out = steps)
      newdata <-
        data.frame(matrix(0, nrow = steps, ncol = num_dims))
      colnames(newdata) <- paste0("x", seq_len(num_dims))
      newdata[[paste0("x", i)]] <- x
      
      y <- predict(model, newdata)
      
      plot_data <- data.frame(x = x, y = y)

      
      # Generate new data for knot prediction
      knot_newdata <-
        data.frame(matrix(0, nrow = length(knots[[i]]), ncol = num_dims))
      colnames(knot_newdata) <- paste0("x", seq_len(num_dims))
      knot_newdata[[paste0("x", i)]] <- knots[[i]]
      knot_predictions <- predict(model, newdata = knot_newdata)
      
      knot_data <- data.frame(x = knots[[i]], y = knot_predictions)
      
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_line(color = "black", size = 1.5) +
        #geom_point(
        #  data = data.frame(x = data[[paste0("x", i)]], y = data$y - mean(data$y)),
        #  aes(x = x, y = y),
        #  color = rgb(0, 0, 1, alpha = 0.4)
        #) +
        geom_point(
          data = knot_data,
          aes(x = x, y = y),
          color = "chartreuse3",
          size = 3
        ) +
        labs(title = paste("Spline", i),
             x = paste("x", i),
             y = "y") +
        theme_minimal()
      
      plots[[i]] <- p
    }
    
    return(plots)
  }
