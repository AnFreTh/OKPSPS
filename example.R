# Load necessary libraries
library(mgcv)
library(pso)
source("data_simulation/functions.R")
source("module/ok_gam.R")

#start by simulating some univariate data

set.seed(101)
n <- 1000
x1 <- runif(n,-0.5, 1)


y <-
  f1(x1) / max(f1(x1)) +  rnorm(n, sd = 0.2)
my_data <- data.frame(x1 = x1,
                      y = y)

# Define the initial gam model
initial_gam_model <-
  gam(y ~ s(x1, bs = "cr", k = 12),
      data = my_data, )

x_seq <- seq(min(x1), max(x1), length.out = 100)
y_seq <- 4 + f1(x_seq) / max(f1(x_seq))

plot(
  initial_gam_model,
  pages = 1,
  residuals = TRUE,
  shade = TRUE,
  lwd = 3,
  ylim = range(c(y + 3, y_seq + 2))
)

# Add grid lines
abline(h = pretty(range(c(y, y_seq))),
       col = "lightgray",
       lty = "dotted")
abline(v = pretty(range(x1)),
       col = "lightgray",
       lty = "dotted")

# Add the data generating function line
lines(x_seq, y_seq, col = "red", lwd = 2, lty=2)

# Call the fit_gam_pso function
result <- fit_gam_optim(
  gam_model = initial_gam_model,
  data = my_data,
  n_knots = 12,
  alpha = 1e-07,
  smoothing_method = "GCV.Cp",
  max_iterations = 10000
)

plot(
  result$model,
  pages = 1,
  residuals = TRUE,
  shade = TRUE,
  lwd = 3,
  ylim = range(c(y + 3, y_seq + 2))
)
# Add grid lines
abline(h = pretty(range(c(y, y_seq))),
       col = "lightgray",
       lty = "dotted")
abline(v = pretty(range(x1)),
       col = "lightgray",
       lty = "dotted")
lines(x_seq,
      y_seq,
      col = "red",
      lwd = 2,
      lty = 2)


result <- fit_gam_pso(
  gam_model = initial_gam_model,
  data = my_data,
  n_knots = 12,
  alpha = 1e-07,
  smoothing_method = "GCV.Cp",
  max_iterations = 100
)

plot(
  result$model,
  pages = 1,
  residuals = TRUE,
  shade = TRUE,
  lwd = 3,
  ylim = range(c(y + 3, y_seq + 2))
)
# Add grid lines
abline(h = pretty(range(c(y, y_seq))),
       col = "lightgray",
       lty = "dotted")
abline(v = pretty(range(x1)),
       col = "lightgray",
       lty = "dotted")
lines(x_seq,
      y_seq,
      col = "red",
      lwd = 2,
      lty = 2)

