# Function to replace close points
replace_close_points <- function(points, alpha=1e-8) {
  points <- sort(points)
  points[1] <- 0
  points[length(points)] <- 1
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
  return(result)
}