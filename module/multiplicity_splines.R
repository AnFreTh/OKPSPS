library(mgcv)
library(splines2)

smooth.construct.MuSP.smooth.spec <- function(object, data, knots) {
  x <- data[[object$term]]  # Extract the variable
  
  # Generate knots if not provided
  if (is.null(knots[[object$term]])) {
    if (is.null(object$bs.dim)) {
      stop("Either knots or k (via bs.dim) must be specified for 'MuSP' smooths.")
    }
    
    # Generate equidistant knots within the range of the data
    custom_knots <- seq(min(x), max(x), length.out = object$bs.dim)
  } else {
    custom_knots <- knots[[object$term]]
  }
  
  # Ensure the boundary knots are correctly set
  boundary_knots <- c(custom_knots[1], custom_knots[length(custom_knots)])
  
  # Internal knots exclude the boundaries
  internal_knots <- custom_knots[-c(1, length(custom_knots))]
  
  # Create the B-spline basis with repeated knots for multiplicity
  bs_basis <- bSpline(
    x,
    knots = internal_knots,
    degree = 3,
    Boundary.knots = boundary_knots
  )
  
  intercept_column <- rep(1, length(x))
  bs_basis <- cbind(intercept_column, bs_basis)
  
  # Define the penalty matrix (second derivative penalty)
  S <- diff(diag(ncol(bs_basis)), differences = 2)
  S <- crossprod(S)
  
  # Construct the smooth object
  object$X <- bs_basis                   # Basis matrix
  object$S <- list(S)                    # Penalty matrix
  object$rank <- ncol(bs_basis) - 2      # Rank of the smooth
  object$null.space.dim <- 2             # Null space dimension (cubic spline)
  object$knots <- custom_knots           # Store the knots
  class(object) <- "MuSP.smooth"         # Assign custom smooth class
  return(object)
}


# Define the Predict.matrix method for MySP.smooth
Predict.matrix.MuSP.smooth <- function(object, data) {
  # Extract the variable from the new data
  x <- data[[object$term]]
  
  # Use the stored knots to create the prediction basis
  boundary_knots <- c(object$knots[1], object$knots[length(object$knots)])
  internal_knots <- object$knots[-c(1, length(object$knots))]
  
  # Generate the B-spline basis for prediction
  bs_basis <- bSpline(
    x,
    knots = internal_knots,
    degree = 3,
    Boundary.knots = boundary_knots
  )
  
  # Add the intercept column to match the fitted basis
  intercept_column <- rep(1, length(x))
  bs_basis <- cbind(intercept_column, bs_basis)
  
  return(bs_basis)
}

