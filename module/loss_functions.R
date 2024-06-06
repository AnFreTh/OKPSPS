# Loss function based on GCV of a GAM
source("module/replace_close_points.R")


loss_func_1dim <- function(params, data, method, alpha) {
  knots_1 <- params[-length(params)]
  knots_1 <- replace_close_points(knots_1, alpha)
  model <- gam(
    y ~ s(
      x,
      k = length(knots_1),
      sp = params[length(params)],
      bs = "cr"
    ),
    knots = list(x = knots_1),
    data = data,
    method = method
  )
  gcv <- model$gcv.ubre[[1]]
  if (is.infinite(gcv) || is.nan(gcv)) {
    gcv <- 100000
  }
  return(gcv)
}




# Loss function based on GCV of a GAM for multiple splines
loss_func_multi <- function(params, data, n_knots, alpha, method) {
  knots <- list()
  start_idx <- 1
  num_dims <- length(n_knots)
  
  for (i in seq_len(num_dims)) {
    end_idx <- start_idx + n_knots[i] - 1
    knots[[i]] <-
      replace_close_points(params[start_idx:end_idx], alpha)
    start_idx <- end_idx + 1
  }
  
  sp <- params[(start_idx):length(params)]
  
  formula_str <- "y ~ -1"
  for (i in seq_len(num_dims)) {
    formula_str <-
      paste0(formula_str,
             " + s(x",
             i,
             ", k = length(knots[[",
             i,
             "]]), sp = sp[",
             i,
             "], bs = 'cr')")
  }
  
  formula <- as.formula(formula_str)
  
  model <- gam(
    formula,
    data = data,
    knots = setNames(knots, paste0("x", seq_len(num_dims))),
    method = method
  )
  
  gcv <- model$gcv.ubre[[1]]
  if (is.infinite(gcv) || is.nan(gcv)) {
    gcv <- 100000
  }
  
  return(gcv)
}
