source("module/replace_close_points.R")
source("module/loss_functions.R")
library(mgcv)
library(pso)
# Function to fit the gam model using PSO
fit_gam_pso <-
  function(gam_model,
           data,
           n_knots = 12,
           alpha = 1e-07,
           method = "GCV.Cp",
           swarm_size = 100,
           max_iterations = 100,
           trace = 1,
           report = 1,
           w = c(1 / (2 * log(2)), 0.3),
           c1 = .5 + log(2),
           c2 = .5 + log(2),
           lambda_max = 5000) {
    # Extract the necessary parameters from the gam model
    smooth_terms <-
      lapply(gam_model$smooth, function(smooth_term)
        smooth_term$term)
    bs_list <-
      sapply(gam_model$smooth, function(smooth_term)
        strsplit(class(smooth_term)[1], "\\.")[[1]][1])
    sp_list <- gam_model$sp
    
    
    
    num_smooth_terms <- length(smooth_terms)
    num_dimensions <- num_smooth_terms * n_knots + num_smooth_terms
    search_space <- matrix(0, nrow = num_dimensions, ncol = 2)
    # Set search space for knot locations according to the min and max of the data
    for (i in 1:num_smooth_terms) {
      var_min <- min(data[[smooth_terms[[i]]]])
      var_max <- max(data[[smooth_terms[[i]]]])
      search_space[((i - 1) * n_knots + 1):(i * n_knots), 1] <-
        var_min
      search_space[((i - 1) * n_knots + 1):(i * n_knots), 2] <-
        var_max
    }
    
    
    
    # Set search space for smoothing parameters
    search_space[(num_smooth_terms * n_knots + 1):num_dimensions, 2] <-
      lambda_max
    
    # Initialize parameters
    initial_params <- numeric(num_dimensions)
    for (i in 1:num_smooth_terms) {
      var_min <- min(data[[smooth_terms[[i]]]])
      var_max <- max(data[[smooth_terms[[i]]]])
      initial_params[((i - 1) * n_knots + 1):(i * n_knots)] <-
        seq(var_min, var_max, length.out = n_knots)
    }
    initial_params[(num_smooth_terms * n_knots + 1):num_dimensions] <-
      sp_list
    
    result <- psoptim(
      fn = loss_func_multidim,
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
      ),
      data = data,
      method = method,
      alpha = alpha,
      bs_list = bs_list,
      smooth_terms = smooth_terms,
      n_knots = n_knots
    )
    
    optimal_solution <- result$par
    sp_list <-
      optimal_solution[(num_smooth_terms * n_knots + 1):length(optimal_solution)]
    knots_list <-
      split(optimal_solution[1:(num_smooth_terms * (n_knots - 2))], rep(1:num_smooth_terms, each = (n_knots - 2)))
    
    for (i in 1:length(knots_list)) {
      knots_list[[i]] <-
        replace_close_points(knots_list[[i]], alpha, data, smooth_terms[[i]]) # Include boundary knots
    }
    
    
    
    
    formula_parts <- sapply(1:length(smooth_terms), function(i) {
      paste0(
        "s(",
        smooth_terms[[i]],
        ", k = ",
        length(knots_list[[i]]),
        ", sp = ",
        sp_list[i],
        ", bs = '",
        bs_list[[i]],
        "')"
      )
    })
    
    formula_str <-
      paste("y ~", paste(formula_parts, collapse = " + "))
    
    knots <-
      setNames(lapply(knots_list, function(k)
        k), smooth_terms)
    model <- gam(
      as.formula(formula_str),
      knots = knots,
      data = data,
      method = method
    )
    
    return(list(
      model = model,
      knots = knots_list,
      sps = sp_list
    ))
  }