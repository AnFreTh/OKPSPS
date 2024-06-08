# Load necessary libraries
library(mgcv)
library(pso)
source("data_simulation/functions.R")
source("module/ok_gam.R")




# Simulate some multivariate data
set.seed(101)
n <- 1000
x1 <- runif(n,-1, 3)
x2 <- runif(n, 0, 2)
x3 <- runif(n,-1, 1)


y <-
  f1(x1) / max(f1(x1)) + f2(x2) / max(f2(x2)) + f3(x3) / max(f3(x3)) +  rnorm(n, sd = 0.2)
my_data <- data.frame(
  x1 = x1,
  x2 = x2,
  x3 = x3,
  x4 = x4,
  y = y
)

# Define the initial gam model
initial_gam_model <-
  gam(
    y ~ s(x1, bs = "cr") + s(x2, bs = "cr") + s(x3, bs = "cr"),
    data = my_data,
    method = "GCV.Cp"
  )

# Call the fit_gam_pso function
result <- fit_gam_pso_multidim(
  gam_model = initial_gam_model,
  data = my_data,
  n_knots = 15,
  alpha = 1e-07,
  method = "GCV.Cp",
  swarm_size = 100,
  max_iterations = 100,
  trace = 1,
  report = 1,
  w = c(1 / (2 * log(2)), 0.3),
  c1 = .5 + log(2),
  c2 = .5 + log(2),
  lambda_max = 5000
)

print(result)

# Plot the fitted model
par(mfrow = c(2, 1))
plot(result$model, pages = 1)
