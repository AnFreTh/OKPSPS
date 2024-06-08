<div align="center">
<img src="pso_header.png" width="600"/>
</div>

# Enhancing Adaptive Spline Regression

## Overview

This repository contains R code for the paper:

**Enhancing Adaptive Spline Regression: An Evolutionary Approach to Optimal Knot Placement and Smoothing Parameter Selection**


## Repository Structure

- **data_simulation/simulate_data.R**: Contains functions for generating simulated data.
- **module/ok_gam.R**: Contains functions for fitting Generalized Additive Models (GAM) using Particle Swarm Optimization (PSO) for multiple splines.
- **module/replace_close_points.R**: Contains helper functions to replace close points.
- **module/loss_functions.R**: Contains loss functions for evaluating the model.
- **plotting/plot_gam.R**: Contains functions for plotting the results.
- **example.R**: Example script demonstrating how to use the provided functions.

## Installation

Ensure you have the following R packages installed:


```R
install.packages(c("ggplot2", "dplyr", "mgcv", "pso", "purrr", "tibble", "gridExtra"))
```


## Usage
Simply fit a gam with mgcv on any dataframe.
Then pass the gam to the provided function and optimize with respect to your preferred criterion. E.g. "REML" for the Restricted Maximum Likelihood criterion, "GCV.Cp" for the generalized cross validation or any other method specified in the mgcv packages 'gam' function.

### Example Script

Below is an example script demonstrating how to use the provided functions to fit a GAM model using PSO for optimal knot placement and smoothing parameter selection.

```R
# Load necessary libraries
library(mgcv)
library(pso)

# Source required functions
source("data_simulation/functions.R")
source("module/ok_gam.R")
```
Get your data in a data.frame, just like you would when you fit a normal gam
```R
# Define the initial gam model
initial_gam_model <- gam(
  y ~ s(x1, bs = "cr") + s(x2, bs = "cr") + s(x3, bs = "cr"),
  data = data,
  method = "GCV.Cp"
)

# Call the fit_gam_pso function
result <- fit_gam_pso_multidim(
  gam_model = initial_gam_model,
  data = data,
  n_knots = 15,
  alpha = 1e-07,
  method = "GCV.Cp",
)
```
This returns a dictionary with knot locations, the optimized gam model and the smoothing parameters.

```R
# Plot the fitted model
par(mfrow = c(2, 1))
plot(result$model, pages = 1)
```


