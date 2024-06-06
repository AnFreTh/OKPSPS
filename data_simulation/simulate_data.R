library(dplyr)
library(ggplot2)
library(mgcv)
library(purrr)
library(tibble)


simulate_data <- function(func, x, sd) {
  y <- func(x) + rnorm(length(x), mean = 0, sd = sd)
  data <- data.frame(x, y)
  return(data)
}

generate_multivariate_data <-
  function(funcs, sd, ranges, n = 1000) {
    data <- list()
    y_sum <- numeric(n)
    
    for (i in seq_along(funcs)) {
      x <- runif(n, ranges[[i]][1], ranges[[i]][2])
      y <- funcs[[i]](x)
      data[[paste0("x", i)]] <- x
      y_sum <- y_sum + y / max(y)
    }
    
    y <- y_sum + rnorm(n, mean = 0, sd = sd)
    data$y = y / (max(y) / 5)
    data <- as.data.frame(data)
    return(data)
  }
