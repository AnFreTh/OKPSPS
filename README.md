<div align="center">
  <img src="pso_header.png" width="600"/>


# Enhancing Adaptive Spline Regression

## Overview

This repository contains R code for the paper:

**Enhancing Adaptive Spline Regression: An Evolutionary Approach to Optimal Knot Placement and Smoothing Parameter Selection**


## Repository Structure

- **data_simulation/simulate_data.R**: Contains functions for generating simulated data.
- **module/multivariate.R**: Contains functions for fitting Generalized Additive Models (GAM) using Particle Swarm Optimization (PSO) for multiple splines.
- **module/replace_close_points.R**: Contains helper functions to replace close points.
- **module/loss_functions.R**: Contains loss functions for evaluating the model.
- **plotting/plot_gam.R**: Contains functions for plotting the results.
- **example.R**: Example script demonstrating how to use the provided functions.

## Installation

Ensure you have the following R packages installed:

```R
install.packages(c("ggplot2", "dplyr", "mgcv", "pso", "purrr", "tibble", "gridExtra"))
