# quad_test.R
# Functions to quantify extrema (peaks and troughs)
# Author: Natalie Dupont
# # Code for functions peaktest_quad, reg2 and peaktest_twolines written by Uri Simonsohn (2018)
    # Source: https://osf.io/wdbmr

#' Tests for the presence of a peak using the quadratic test. ****NOT AN APPRORPIATE APPROACH****
#'
#'function from Uri Simonsohn 2018 (https://osf.io/wdbmr)
#'
#' @param x vector of x-values from the data
#' @param y vector of y-values from the data
#'
#' @returns dasf
#' @export peaktest_quad
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
