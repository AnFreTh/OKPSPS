# Loss function based on GCV of a GAM
source("module/replace_close_points.R")
library(torch)

loss_func_1dim <-
  function(params,
           data,
           smoothing_method,
           alpha) {
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
      method = smoothing_method
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
           smoothing_method,
           alpha,
           bs_list,
           smooth_terms,
           n_knots,
           merge = TRUE) {
    num_smooth_terms <- length(smooth_terms)
    sp_list <-
      params[(num_smooth_terms * n_knots + 1):length(params)]
    idx <- length(params) - num_smooth_terms
    
    knots_list <- params[1:idx]
    
    knots_list <-
      split(knots_list[1:(num_smooth_terms * n_knots)], rep(1:num_smooth_terms, each = n_knots))
    
    
    
    for (i in 1:length(knots_list)) {
      if (merge == TRUE) {
        knots_list[[i]] <-
          replace_close_points(knots_list[[i]], alpha, data, var_name = smooth_terms[[i]])
      }
      else{
        knots_list[[i]] <- drift_apart_knots(knots_list[[i]], alpha)
      }
      knots_list[[i]] <- sort(knots_list[[i]])
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
    
    gcv <- tryCatch({
      model <-
        gam(
          as.formula(formula_str),
          knots = knots,
          data = data,
          method = smoothing_method
        )
      model$gcv.ubre[[1]]
    }, error = function(e) {
      if (grepl("Supplied matrix not symmetric", e$message)) {
        message("Encountered a non-symmetric matrix. Returning a large value...")
      }
      100000
    })
    
    if (is.infinite(gcv) || is.nan(gcv)) {
      gcv <- 100000
    }
    
    return(gcv)
  }






# Compute gradients using finite differences
compute_gradients <- function(params,
                              data,
                              smoothing_method,
                              alpha,
                              bs_list,
                              smooth_terms,
                              n_knots,
                              epsilon = 1e-09,
                              merge = FALSE) {
  base_loss <- loss_func_multidim(params,
                                  data,
                                  smoothing_method,
                                  alpha,
                                  bs_list,
                                  smooth_terms,
                                  n_knots,
                                  merge)
  gradients <- numeric(length(params))
  
  for (i in seq_along(params)) {
    params[i] <- params[i] + epsilon
    new_loss <- loss_func_multidim(params,
                                   data,
                                   smoothing_method,
                                   alpha,
                                   bs_list,
                                   smooth_terms,
                                   n_knots,
                                   merge)
    gradients[i] <- (new_loss - base_loss) / epsilon
    params[i] <- params[i] - epsilon
  }
  return(gradients)
}
