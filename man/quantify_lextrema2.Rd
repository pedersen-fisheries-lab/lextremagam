% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantify_lextrema.R
\name{quantify_lextrema2}
\alias{quantify_lextrema2}
\title{NOT TO BE USED Estimate the first derivative - alpha-corrected}
\usage{
quantify_lextrema2(
  mod,
  var = NULL,
  step_size = NULL,
  conf_level = 0.95,
  deriv_method = c("gratia", "marginaleffects"),
  multivariate = FALSE,
  smooth = NULL,
  frequentist = FALSE
)
}
\arguments{
\item{mod}{gam model object to be evaluated}

\item{var}{predictor variable over which the slope is evaluated}

\item{step_size}{the step size at which to evaluate the first derivative}

\item{conf_level}{the confidence level (between 0 and 1) at which the confidence interval of the extrema is to be estimated}

\item{deriv_method}{whether to use gratia's derivatives function or marginaleffects' slopes function}

\item{multivariate}{TRUE/FALSE if the model to be evaluated is multivariate}

\item{smooth}{name of the smooth in the format \"s(...)\"}

\item{frequentist}{if using gratia::derivatives, TRUE/FALSE boolean whether or not the derivatives are calculated using the bayesian (default) or frequentist covariance matrix}
}
\value{
a slopes object dataframe built by marginaleffects::slopes, including the model rowid, term, estimate, std.error, conf.low, conf.high, y, x
}
\description{
Evaluates the first derivative at a given (very small) step size for a given gam model input. Note: THIS FUNCTION IS CURRENTLY UNIVARIATE
}
