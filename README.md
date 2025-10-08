
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lextremagam

<!-- badges: start -->
<!-- badges: end -->

The lextremagam package serves as a streamlined way to quantify peaks
and troughs in complex data using generalized additive models. While it
was developed primarily for ecologists, it can be applied to any dataset
where a local extremum (peak or trough) in a one-dimentional smooth is
of interest. Broadly, this package takes a gam object generated using
mgcv and identifies confidence intervals for local extrema (peaks or
troughs) based on the first derivative of the smooth. This method, its
application and its performance are described in \[Dupont et
al. preprint\]. lextremagam can also identify other segments
(increasing, decreasing and plateaus) based on the first derivative, but
the properties of these segments have not been evaluated and they must
be interpreted with caution.

## Installation

You can install the development version of lextremagam from
[GitHub](https://github.com/) as below:

``` r
# if you do not have remotes installed
# install.packages("remotes")
```

``` r
#installing the package from github
remotes::install_github("pedersen-fisheries-lab/lextremagam", build_vignettes = TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lextremagam)
#> Welcome to lextremagam. For the proper functionning of this package, please ensure that mgcv is loaded.
#> Other useful packages include marginaleffects, dplyr, gratia, data.table, and ggplot2
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
