
# tsgp <img src="man/figures/logo.png" align="right" width="120" />

Simple, lightweight R package for fitting, visualising, and predicting
time-series data using Gaussian processes.

## Installation

You can install the development version of `tsgp` from GitHub using the
following:

``` r
devtools::install_github("hendersontrent/tsgp")
```

## Premise

`tsgp` implements the functionality presented in a [recent
tutorial](https://hendersontrent.github.io/posts/2024/05/gaussian-process-time-series/)
for modelling time-series data with Gaussian processes. Currently, only
the univariate setting is supported. `tsgp` works on a [structural time
series](https://www.sciencedirect.com/science/article/abs/pii/S0169716105800458)
perspective. That is, by decomposing a time series into its constituent
statistical parts (e.g., trend, seasonality, noise), one can model each
component independently before combining them to form the complete
picture of temporal dynamics. This is not only intuitive, but it is also
highly transparent—meaning that intelligent and justifiable modelling
decisions must be made in order to appropriately capture the data
generating process.

`tsgp` is extremely lightweight in both its dependencies and
computational approach. If you are seeking a more rigorous or flexible
approach to using GPs for time-series analysis, please look into
[`Stan`](https://mc-stan.org),
[`GPy`](https://gpy.readthedocs.io/en/deploy/),
[`GauPro`](https://github.com/CollinErickson/GauPro), [Tensorflow
Probability](https://www.tensorflow.org/probability), or
[`GaussianProcesses.jl`](https://github.com/STOR-i/GaussianProcesses.jl).

## Functionality

Currently, `tsgp` supports the following covariance functions (kernels):

- Exponentiated quadratic (squared exponential)
- Rational quadratic
- Periodic
- Linear

`tsgp` flexibly enables composite kernels (either through addition or
multiplication) to be constructed and is actively encouraged to
appropriately model complex temporal dynamics.

`tsgp` also includes functions for computing and visualising draws from
Gaussian process priors and posteriors, visualising covariance matrices,
and plotting predictions.

## Performance

`tsgp` is *extremely* fast at what it does. Well, as fast as it can be
given the $\mathcal{O}(N^{3})$ computation time of inverting a matrix
involved in fitting a GP. A full model with trend, seasonality, and
noise can be calculated on a time series of $T = 1000$ time points in a
few seconds. This makes it an ideal tool for iterative model building
and principled time-series exploration.
