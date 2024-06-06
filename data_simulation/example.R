source("data_simulation/functions.R")
source("module/single_variable.R")
source("plotting/plot_gam.R")

# Simulate data
set.seed(505)
x <- seq(0, 1, length.out = 1000)
data <- simulate_data(f2, x, SD2)

# Fit the GAM with optimal knots using PSO
fit_results <- fit_gam_pso(data)

# Fit a normal GAM model for comparison
normal_model <- gam(y ~ s(x, k = 12, bs = "cr"), data = data, method = "GCV.Cp")

# Plot the results
plot_gam(data, fit_results$model, fit_results$knots, normal_model)