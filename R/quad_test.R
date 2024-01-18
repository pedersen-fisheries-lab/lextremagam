# quad_test.R
# Functions to quantify extrema (peaks and troughs)
# Author: Natalie Dupont (most code written by Uri Simonshon, and used under a CC-By Attribution 4.0 International license)
# # Code for functions peaktest_quad written by Uri Simonsohn (2018)
    # Source: https://osf.io/wdbmr

#' Tests for the presence of a peak using the quadratic test.
#'
#' ****THIS IS NOT AN APPRORPIATE APPROACH****
#'This function is used to compare the performance of the new proposed gam peak quantification method in comparison to the historically used (and inappropriate) quadratic test
#'
#'This function was written by Uri Simonsohn 2018 (https://osf.io/wdbmr) and is used in the present package under a CC-By Attribution 4.0 International license (https://creativecommons.org/licenses/by/4.0/)
#'The only part of this code that was changed was the name of the function (originally called flood.u).
#'
#' @param x vector of x-values from the data
#' @param y vector of y-values from the data
#'
#' @returns u (boolean 0 or 1 indicating if the test detected a peak or not)
peaktest_quad <- function(x,y) {
  #Sample size
  n=length(x)
  #Square x to include in the regression as predictor
  x2=x^2
  #Run quadratic regression
  qlm=lm(y~x+x2)
  #Run floodlight on results
  coef <- coefficients(qlm) #point estimates
  vc <- vcov(qlm)           #VAR matrix
  t <- qt(.975,df = n-3)    #critical t-stat
  #Critical points where slope is significantly of each sign
  jn1 = (-(4*t^2*vc[2,3]-4*coef[2]*coef[3]) -
           sqrt((4*t^2*vc[2,3] - 4*coef[2]*coef[3])^2 - 4*(4*t^2*vc[3,3]-4*coef[3]^2)*(t^2*vc[2,2]-coef[2]^2)))/(2*(4*t^2*vc[3,3]-4*coef[3]^2))
  jn2 = (-(4*t^2*vc[2,3]-4*coef[2]*coef[3]) +
           sqrt((4*t^2*vc[2,3] - 4*coef[2]*coef[3])^2 - 4*(4*t^2*vc[3,3]-4*coef[3]^2)*(t^2*vc[2,2]-coef[2]^2)))/(2*(4*t^2*vc[3,3]-4*coef[3]^2))

  #if both are within the range of values, we have a u-shape
  u=0
  if (!is.na(jn1) & !is.na(jn2)) if (jn1>min(x) & jn1<max(x) & jn2>min(x) & jn2<max(x) & jn1>jn2) u=1
  #return u=1 if significant u (both ts have opposite sign and are p<.05), =0 otherwise
  return(u)
}
