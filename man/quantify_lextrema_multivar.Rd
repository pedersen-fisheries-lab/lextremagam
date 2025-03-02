% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantify_lextrema.R
\name{quantify_lextrema_multivar}
\alias{quantify_lextrema_multivar}
\title{Estimate the first derivative}
\usage{
quantify_lextrema_multivar(
  mod,
  var = NULL,
  smooth = NULL,
  step_size = NULL,
  conf_level = 0.95,
  deriv_method = c("gratia", "marginaleffects"),
  frequentist = FALSE
)
}
\arguments{
\item{mod}{gam model object to be evaluated}

\item{var}{predictor variable name to be evaluated}

\item{smooth}{smooth name to be evaluated in the format \"s()\" (ex: \"s(temp)\")}

\item{step_size}{the step size at which to evaluate the first derivative}

\item{conf_level}{the confidence level (between 0 and 1) at which the confidence interval of the first derivative is estimated}

\item{deriv_method}{whether to use gratia's derivatives function or marginaleffects' slopes function}

\item{frequentist}{if using gratia::derivatives, TRUE/FALSE boolean whether or not the derivatives are calculated using the bayesian (default) or frequentist covariance matriz}
}
\value{
a list object built by marginaleffects::slopes or gratia::derivative, including the model rowid, term, estimate, std.error, conf.low, conf.high, y, x
}
\description{
Evaluates the first derivative at a given (very small) step size for a given gam model input. Note: THIS FUNCTION IS CURRENTLY UNIVARIATE
}
