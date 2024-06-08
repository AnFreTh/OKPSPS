# Function to replace close points
replace_close_points <-
  function(points, alpha = 1e-06, data, var_name) {
    points <- sort(points)
    points <- unique(points)
    result <- numeric(0)
    i <- 1
    while (i < length(points)) {
      diff <- points[i + 1] - points[i]
      if (diff >= alpha) {
        result <- c(result, points[i])
        i <- i + 1
      } else {
        avg <- mean(c(points[i], points[i + 1]))
        result <- c(result, avg)
        i <- i + 2
      }
    }
    if (i == length(points))
      result <- c(result, points[i])
    
    # Ensure boundary knots are the min and max of the data for the given variable
    var_min <- min(data[[var_name]])
    var_max <- max(data[[var_name]])
    
    result <- sort(result)
    result[1] <- var_min
    result[length(result)] <- var_max
    return(result)
  }