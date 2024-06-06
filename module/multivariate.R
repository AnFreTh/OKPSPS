# module/fit_gam_pso_multi.R

# Load necessary libraries
library(pso)
library(mgcv)
library(dplyr)

# Source the loss function and helper functions
source("module/loss_functions.R")
source("module/replace_close_points.R")

fit_gam_pso_multi <-
  function(data,
           n_knots,
           alpha = 1e-06,
           method = "GCV.Cp",
           swarm_size = 100,
           max_iterations = 100,
           trace = 1,
           report = 1,
           w = c(1/(2*log(2)), 0.3),
           c1 = .5+log(2),
           c2 = .5+log(2),
           lambda_max = 10000) {
    num_dims <- length(n_knots)
    num_params <- sum(n_knots) + num_dims
    search_space <- matrix(0, nrow = num_params, ncol = 2)
    
    start_idx <- 1
    for (i in seq_len(num_dims)) {
      search_space[start_idx:(start_idx + n_knots[i] - 1), 1] <-
        min(data[[paste0("x", i)]])
      search_space[start_idx:(start_idx + n_knots[i] - 1), 2] <-
        max(data[[paste0("x", i)]])
      start_idx <- start_idx + n_knots[i]
    }
    
    search_space[start_idx:num_params, 2] <- lambda_max
    
    initial_params <-
      runif(num_params, search_space[, 1], search_space[, 2])
    for (i in seq_len(num_dims)) {
      idx <- (sum(n_knots[1:(i - 1)]) + 1):sum(n_knots[1:i])
      initial_params[idx] <- sort(initial_params[idx])
    }
    
    result <- psoptim(
      fn = function(params)
        loss_func_multi(params, data, n_knots, alpha, method),
      lower = search_space[, 1],
      upper = search_space[, 2],
      par = initial_params,
      control = list(
        s = swarm_size,
        maxit = max_iterations,
        trace = trace,
        REPORT = report,
        w = w,
        c.p = c1,
        c.g = c2
      )
    )
    
    optimal_solution <- result$par
    knots <- list()
    start_idx <- 1
    
    for (i in seq_len(num_dims)) {
      end_idx <- start_idx + n_knots[i] - 1
      knots[[i]] <-
        replace_close_points(optimal_solution[start_idx:end_idx], alpha)
      start_idx <- end_idx + 1
    }
    
    formula_str <- "y ~ -1"
    for (i in seq_len(num_dims)) {
      formula_str <-
        paste0(
          formula_str,
          " + s(x",
          i,
          ", k = length(knots[[",
          i,
          "]]), sp = optimal_solution[(start_idx + ",
          i - 1,
          ")], bs = 'cr')"
        )
    }
    
    formula <- as.formula(formula_str)
    
    model <- gam(
      formula,
      data = data,
      knots = setNames(knots, paste0("x", seq_len(num_dims))),
      method = method
    )
    
    return(list(
      model = model,
      knots = knots,
      optimal_solution = optimal_solution
    ))
  }
