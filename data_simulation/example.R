# example.R

# Source the necessary functions
source("data_simulation/simulate_data.R")
source("module/multivariate.R")
source("module/replace_close_points.R")
source("module/loss_functions.R")
source("plotting/plot_gam.R")
source("data_simulation/functions.R")
library(ggplot2)
library(gridExtra)


ranges <-
  list(c(0, 1),
       c(0, 1),
       c(0, 1),
       c(0, 1),
       c(0, 1),
       c(-1, 1))

# Generate the data
data <-
  generate_multivariate_data(list(f1, f2, f3, f4, f5, f6), sd = 0.01, ranges, n = 1000)





plot_all_variables <- function(data) {
  num_vars <- ncol(data) - 1 # Exclude the y column
  plots <- list()
  
  for (i in seq_len(num_vars)) {
    var_name <- paste0("x", i)
    p <- ggplot(data, aes_string(x = var_name, y = "y")) +
      geom_point(color = rgb(0, 0, 1, alpha = 0.4)) +
      labs(
        title = paste("Plot of", var_name, "against y"),
        x = var_name,
        y = "y"
      ) +
      theme_minimal()
    plots[[i]] <- p
  }
  
  # Arrange all plots in a grid
  do.call(grid.arrange, c(plots, ncol = 2))
}

plot_all_variables(data)






# Fit the GAM with optimal knots using PSO for multiple splines
n_knots <- c(12, 12, 12, 12, 12, 12)
fit_results <-
  fit_gam_pso_multi(data,
                    n_knots,
                    max_iterations = 100,
                    lambda_max = 1000)

# Plot the results for each spline
plots <-
  plot_multivariate_splines(fit_results$model, data, fit_results$knots)

# Display the plots
for (p in plots) {
  print(p)
}
