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



# Updated loss function to include smoothing parameters in the optimization
loss_func_multidim <-
  function(params,
           data,
           method,
           alpha,
           bs_list,
           smooth_terms,
           n_knots) {
    num_smooth_terms <- length(smooth_terms)
    sp_list <-
      params[(num_smooth_terms * n_knots + 1):length(params)]
    idx <- length(params) - num_smooth_terms
    knots_list <- params[1:idx]
    knots_list <-
      split(knots_list[1:(num_smooth_terms * n_knots)], rep(1:num_smooth_terms, each = n_knots))
    
    
    for (i in 1:length(knots_list)) {
      knots_list[[i]] <-
        replace_close_points(knots_list[[i]], alpha, data, var_name = smooth_terms[[i]])
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
    
    
    gcv <- model$gcv.ubre[[1]]
    if (is.infinite(gcv) || is.nan(gcv)) {
      gcv <- 100000
    }
    return(gcv)
  }
