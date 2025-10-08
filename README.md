
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
install.packages("remotes")
```

``` r
#installing the package from github
remotes::install_github("pedersen-fisheries-lab/lextremagam", build_vignettes = TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
#load in lextremagam
library(lextremagam)
#> Welcome to lextremagam. For the proper functionning of this package, please ensure that mgcv is loaded.
#> Other useful packages include marginaleffects, dplyr, gratia, data.table, and ggplot2

#import a simple dataset
data("data_norm")
plot(y~x, data = data_norm)
```

<img src="man/figures/README-import data-1.png" width="100%" /> From a
simple graph, this dataset seems to be peaked, but it’s not super clear.
Let’s run a simple gam using mgcv

``` r
gam1 <- mgcv::gam(y~s(x), data= data_norm, method = "REML")
gratia::draw(gam1, residuals = TRUE)
```

<img src="man/figures/README-run gam-1.png" width="100%" /> The gam
certainly looks to be peaked! With the naked eye, we can approximate a
possible location for this peak, but we cannot estimate a confidence
interval for the peak. To do so, let’s use quantify_lextrema!

``` r
lextr1 <- lextremagam::quantify_lextrema(mod = gam1,                  #mod is the gam model object to be evaluate
                                         var = "x",                   #var is the variable 
                                         step_size = 0.01,            #step_size is the Δx at which the derivative will be evaluated using finite differences
                                         conf_level = 0.95,           #conf_level is the confidence level (1-α) at which to evaluate the peak
                                         deriv_method = "gratia",     #lextremagam can use gratia (default) or marginaleffects (not recommended) for calculating the first derivated
                                         frequentist = FALSE)         #lextremagam can either use the bayesian (default) or frequentist posterior variance-covariance matrix to calculate the first derivative uncertainty

lextr1$segment_summary
#> # A tibble: 5 × 7
#>   seg_id slope_sign x_start x_end   lag  lead feature     
#>    <int>      <dbl>   <dbl> <dbl> <dbl> <dbl> <chr>       
#> 1      1          0     1    15.9    NA     1 boundary_min
#> 2      2          1    16.0  84.0     0     0 increase    
#> 3      3          0    84.0 114.      1    -1 local_max   
#> 4      4         -1   114.  179.      0     0 decrease    
#> 5      5          0   179   200      -1    NA boundary_min
```

quantify_lextrema outputs a lextrema object. This contains the original
GAM model, the input variables, the calculated first derivative table
and most importantly, a summary of the identified segments. Here for
peaks (local_max) and troughs (local_min), the x_start and x_end are the
lower and upper confidence bounds of the local extremum. The bounds of
other segments are also given, but have no been evaluate for confidence

lextremagam provides a basic plotting function to easily visualize your
data:

``` r
lextremagam::plot_lextrema(lextr = lextr1,              #lextr is the lextremagam object to be visualized
                                    plot_deriv = TRUE,           #plot_lextrema always produces a figure of the original model smooth. plot_deriv = TRUE will produce a second plot of the derivative smooth
                                    show_segs = "local_max",     #which segments to be shown in the smooth plot. You can specify multiple segments in a vector. (ex: show_segs = c("local_max", "local_min"))
                                    show_segs_deriv = TRUE,      # if the segments should be shown in the derivative plot. They will be shown as a ribbon
                                    type = "response" )          #if the original gam smooth should be plotted on the response scale or the link scale
```

<img src="man/figures/README-plot lextrema-1.png" width="100%" /><img src="man/figures/README-plot lextrema-2.png" width="100%" />

    #> $model_plot

<img src="man/figures/README-plot lextrema-3.png" width="100%" />

    #> 
    #> $deriv_plot

<img src="man/figures/README-plot lextrema-4.png" width="100%" />

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
