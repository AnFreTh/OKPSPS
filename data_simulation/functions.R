# data_simulation/functions.R

# Simulated data functions

# Function f1
f1 <- function(x) {
  y <- ifelse(x < 0.5, -1.5, 0.25 * sin(x ^ 2 * pi ^ 1.5))
  return(y)
}

f2 <- function(x){
  y = 2*x
  return(y)
}

f3 <- function(x){
  y = x**2
  return(y)
}

f4 <- function(x){
  y = sin(x)
  return(y)
}
# Function f2
f5 <- function(x) {
  y <- ifelse(x < 0.6, 1 / (0.01 + (x - 0.3) ** 2), 1 / (0.015 + (x - 0.65) ** 2))
  return(y)
}

# Function f3
f6 <- function(x) {
  y <- 100 / (exp(abs(10 * x - 5))) + ((10 * x - 5) ^ 5) / 500
  return(y)
}

# Function f4
f7 <- function(x) {
  y <- sin(15 * x) + 0.3 * x ^ 2
  
  jump1 <- 0.4
  jump2 <- 0.6
  jump3 <- 0.8
  
  y[x < jump1] <- y[x < jump1] + 2
  y[x >= jump1 & x < jump2] <- y[x >= jump1 & x < jump2] - 2
  y[x >= jump2 & x < jump3] <- y[x >= jump2 & x < jump3] + 1
  
  return(y)
}

# Function f5
f8 <- function(x) {
  result <- 90 / (1 + exp(-100 * (x - 0.4)))
  return(result)
}

# Function f6
f9 <- function(x) {
  result <- sin(10 * pi * x) + 2 * exp(-50 * x ^ 3) + 2
  return(result)
}

# Function f7
f10 <- function(x) {
  y <- 3 * sin(x ^ 2 * pi ^ 1.5)
  
  jump1 <- 0.4
  jump2 <- 0.6
  
  y[x < jump1] <- y[x < jump1] + 2
  y[x >= jump1 & x < jump2] <-  -2
  
  return(y)
}
