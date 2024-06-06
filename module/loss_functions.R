# Loss function based on GCV of a GAM
source("module/replace_close_points.R")


loss_func_single_variat <- function(params, data, method, alpha) {
  knots_1 <- params[-length(params)]
  knots_1 <- replace_close_points(knots_1, alpha)
  model <- gam(
    y ~ s(x, k = length(knots_1), sp = params[length(params)], bs = "cr"),
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
