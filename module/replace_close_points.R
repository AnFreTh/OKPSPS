replace_close_points <- function(points,
                                 alpha = 1e-06,
                                 data,
                                 var_name,
                                 multiplicity = FALSE,
                                 max_m = 4) {
  if (!is.numeric(max_m) || max_m <= 1 || max_m %% 1 != 0) {
    stop("max_m must be an integer greater than 1.")
  }
  
  points <- sort(points)
  result <- numeric(0)
  i <- 1
  
  while (i <= length(points)) {
    # Find all knots within the threshold alpha
    j <- i
    while (j < length(points) && points[j + 1] - points[j] < alpha) {
      j <- j + 1
    }
    
    if (multiplicity) {
      # Determine the number of repetitions dynamically, capped by max_m
      num_knots_in_group <- min(j - i + 1, max_m)
      avg <- mean(points[i:j])  # Average of all knots within the group
      result <- c(result, rep(avg, num_knots_in_group))
    } else {
      # Replace with the average of the close points
      avg <- mean(points[i:j])
      result <- c(result, avg)
    }
    
    # Move to the next group of knots
    i <- j + 1
  }
  
  # Ensure boundary knots are the min and max of the data for the given variable
  var_min <- min(data[[var_name]])
  var_max <- max(data[[var_name]])
  
  # Drop points outside the valid range
  result <- result[result >= var_min & result <= var_max]
  
  # Ensure the first and last points match boundaries exactly
  result <- sort(result)
  if (length(result) > 0) {
    result[1] <- var_min
    result[length(result)] <- var_max
  }
  
  return(result)
}





drift_apart_knots <- function(knots, alpha) {
  for (i in 2:length(knots)) {
    if (abs(knots[i] - knots[i - 1]) < alpha) {
      mid_point <- (knots[i] + knots[i - 1]) / 2
      knots[i - 1] <- mid_point - alpha / 2
      knots[i] <- mid_point + alpha / 2
    }
  }
  return(knots)
}