# A Robust Method for Estimating the Zero-exposure Proportion under Multiple Left-censored Detection Limits

## Overview

This repository provides an R implementation of a maximum likelihood-based method for estimating the zero-inflation proportion (ZIP) in left-censored data with multiple detection limits (MDLs).

The method is designed for environmental monitoring and exposure assessment datasets in which observations may contain both true zeros and positive values below the detection limits. By integrating information from different detection limits, the proposed method estimates the proportion of true zeros, helping distinguish zero exposure from unquantified low-level exposure.

## Method Description

The method assumes a zero-inflated distribution consisting of a point mass at zero and a continuous distribution for positive observations. Given the parameters of the positive-value distribution, the zero-inflation proportion is estimated by maximizing the observed-data likelihood using an expectation-maximization (EM) algorithm or numerical optimization.

For applications involving unknown distribution parameters, a profile likelihood approach can be used to jointly estimate the zero-inflation proportion and the parameters of the positive-value distribution under a specified parametric model.

Bootstrap resampling can be applied to quantify estimation uncertainty and construct 95% confidence intervals.

## File Description

| File | Description |
|---|---|
| `[simulation_cal.R]` | Simulation experiments evaluating estimation accuracy and precision under different sampling conditions. |
| `[SI_tables.xlsx]` | Example dataset containing observations obtained under multiple detection limits. The data that support the findings of this study are available from the corresponding author upon reasonable request. |
| `[example_cal.R]` | Example workflow demonstrating data preparation, parameter estimation, and result interpretation. |

*Note: File names should be updated to match the actual repository contents.*

## Requirements

- R version 3.6.2 or later.
- Required R packages: ggplot2, patchwork, dplyr, openxlsx.
