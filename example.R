# Load necessary libraries
library(mgcv)
library(pso)
source("data_simulation/functions.R")
source("module/ok_gam.R")

#start by simulating some univariate data

set.seed(101)
n <- 1000
x1 <- runif(n,-1, 1)



y <-
  f1(x1) / max(f1(x1)) +  rnorm(n, sd = 0.2)
my_data <- data.frame(
  x1 = x1,
  y = y
)

# Define the initial gam model
initial_gam_model <-
  gam(
    y ~ s(x1, bs = "cr"),
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
)

plot(result$model, pages = 1)


# compare to "normal" model
plot(initial_gam_model, pages=1)
