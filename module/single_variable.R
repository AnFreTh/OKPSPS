# main_functionality/gam_pso.R

# Load necessary libraries
library(pso)
library(mgcv)
library(dplyr)

source("module/loss_functions.R")


# Function to fit GAM with optimal knots using PSO
fit_gam_pso <- function(data, n_knots = 12, alpha = 0.00000001, method = "GCV.Cp", swarm_size = 100, max_iterations = 100, trace=1, report=1, w=0.5, c1=0.2, c2=0.2, lambda_max=10000) {
  num_dimensions <- n_knots + 1
  search_space <- matrix(0, nrow = num_dimensions, ncol = 2)
  search_space[, 2] <- 1
  search_space[num_dimensions, 2] <- lambda_max
  initial_params <- runif(num_dimensions, search_space[, 1], search_space[, 2])
  initial_params[1:n_knots] <- sort(initial_params[1:n_knots])
  initial_params <- c(seq(0, 1, length.out = n_knots), swarm_size)
  
  result <- psoptim(
    fn = loss_func,
    lower = search_space[, 1],
    upper = search_space[, 2],
    par = initial_params,
    control = list(
      s = swarm_size,
      maxit = max_iterations,
      trace = trace,
      REPORT = report,
      w = w,
      c1 = c1,
      c2 = c2
    ),
    data = data,
    method = method,
    alpha = alpha
  )
  
  optimal_solution <- result$par
  knots_1 <- optimal_solution[-length(optimal_solution)]
  knots_1 <- replace_close_points(knots_1, alpha)
  
  model <- gam(
    y ~ s(x, k = length(knots_1), sp = optimal_solution[length(optimal_solution)], bs = "cr"),
    knots = list(x = knots_1),
    data = data,
    method = method
  )
  
  return(list(model = model, knots = knots_1, optimal_solution = optimal_solution))
}
