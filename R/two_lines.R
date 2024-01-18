# two_lines.R
# Functions to quantify extrema (peaks and troughs)
# Author: Natalie Dupont (code written by Uri Simonshon, and used under a CC-By Attribution 4.0 International license)
# # Code for functions peaktest_quad, reg2 and peaktest_twolines written by Uri Simonsohn (2018)
  # Source: https://osf.io/wdbmr

#' Runs the two-line regression test based on a given break point xc
#'
#'This function is used to compare the performance of the new proposed gam peak quantification method to the two-lines test proposed by Uri Simonsohn (2017)
#'This function was written by Uri Simonsohn 2018 (https://osf.io/wdbmr) and is used in the present package under a CC-By Attribution 4.0 International license (https://creativecommons.org/licenses/by/4.0/)
#'
#' @param x vector of x-values from the data
#' @param y vector of y-values from the data
#' @param xc the step size at which to evaluate the first derivative
#'
#' @returns a slopes object dataframe built by marginaleffects::slopes, including the model rowid, term, estimate, std.error, conf.low, conf.high, y, x
reg2 <- function(x, y, xc){
  xlow1=ifelse(x<=xc,x-xc,0)     #xlow=x-xc when x<xc, 0 otherwise
  xhigh1=ifelse(x>xc,x-xc,0)     #xhigh=x when x<xmax, 0 otherwise
  high1=ifelse(x>xc,1,0)         #high dummy, allows interruption

  #Now include xc in second line
  xlow2=ifelse(x<xc,x-xc,0)
  xhigh2=ifelse(x>=xc,x-xc,0)
  high2=ifelse(x>=xc,1,0)         #high dummy, allows interruption
  #Run the regressions (they differe only on whetehr xc is 'high' or 'low')
  lm1=lm(y~xlow1+xhigh1+high1)     #estimate regression
  lm2=lm(y~xlow2+xhigh2+high2)     #estimate regression
  #Fitted values
  yhat1=fitted(lm1)
  yhat2=fitted(lm2)
  #Regression results
  lmc1=summary(lm1)$coefficients
  lmc2=summary(lm2)$coefficients
  #Turn into individual results (scalars)
  #Results for 1st line come from 1st regression, including xc1
  b1=lmc1[2,1]
  t1=lmc1[2,3]
  p1=lmc1[2,4]
  #Results for 2nd line come from 2nd regression, including xc1 as well
  b2=lmc2[3,1]
  t2=lmc2[3,3]
  p2=lmc2[3,4]
  #Is the u-shape significnat?
  u.sig =ifelse(b1*b2<0 & p1<.05 & p2<.05,1,0)
  #All results
  res=list(b1=b1,p1=p1,b2=b2,p2=p2,u.sig=u.sig,xc=xc,t1=t1,t2=t2,yhat1=yhat1,yhat2=yhat2)  #Output list with all those parameters, betas, t-values, p-values and significance for u
  return(res)
}

#' Runs the two-lines regression test
#'
#'This function is used to compare the performance of the new proposed gam peak quantification method to the two-lines test proposed by Uri Simonsohn (2017)
#'This function was written by Uri Simonsohn 2018 (https://osf.io/wdbmr) and is used in the present package under a CC-By Attribution 4.0 International license (https://creativecommons.org/licenses/by/4.0/)
#'This function runs the two-lines test using different breakpoint definitions
#'
#' @param x vector of x-values (predictor)
#' @param y vector of y-values (response)
#'
#' @returns res (a vector of 4 boolean 0 or 1 values indicating if a peak was identified for any of the four methods. In order: breakpoint = maximum response value, breakpoint = the median of the flat segment from a gam, breakpoint = median of the flat segemnt readjusted to give more power to the weakest slope, three-lines test)
peaktest_twolines <- function(x,y) {
  #Syntax:
  #1 Run gam()
  g=mgcv::gam(y~s(x,bs="cr"))
  #2 Get fitted values
  g.fit= mgcv::predict.gam(g,se.fit=T)
  y.hat=g.fit$fit
  y.se =g.fit$se.fit
  #3 Focus on the middle 80% of the x-values (smoothed values are not reliable near the end)
  x10=quantile(x,.1)
  x90=quantile(x,.9)
  middle=(x>x10 & x<x90)       #Don't consider extreme values for cutoff
  y.ub=y.hat+1*y.se            #+1 SE is the flat region
  x.middle=x[middle]           #X values associated with flat region
  xc.max=x.middle[match(max(y.hat[middle]),y.hat[middle])]       #find value of x associated with highest predicted value
  #4 Find flat maximum
  flat=(y.ub>max(y.hat) & middle)
  xflat=x[flat]
  #5 If empty, use median x
  if (length(xflat)==0) xflat=median(x)

  #6 Regression split based on predicted maximum
  rmax=reg2(x,y,xc=xc.max)

  #7  Regression split based median of xflat
  rmed=reg2(x,y,xc=median(xflat))  #At the median of  xflat
  #8 Adjust split point based on ratio of t1,t2, move split away from less significant slope, to give more of a chance
  t1=abs(rmed$t1)
  t2=abs(rmed$t2)
  xc.prop=quantile(xflat,t2/(t1+t2))  #Choose the percentile value in flat region proportional to t1,t2
  #For example, if t2=t1 it stays at median flat, if t2>t1, it moves lower
  #9 Regression split based on adjusted based on t1,t2
  rprop=reg2(x,y,xc=xc.prop)

  #10 Run 3-semgent regression
  xc1=min(x.middle)
  xc2=max(x.middle)

  #10.1 Gen variables for twice interrupted regression
  x1=ifelse(x<xc1,x-xc1,0)
  x2=ifelse(x>xc1 & x<xc2,x-xc1,0)
  x3=ifelse(x>xc2, x-xc2,0)
  high1=ifelse(x<xc1,0,1)
  high2=ifelse(x<xc2,0,1)

  #10.2 Run the regressions (they differe only on whetehr xc is 'high' or 'low')
  lm1=lm(y~x1+x2+x3+high1+high2)      #estimate regression
  lmc=summary(lm1)$coefficients

  #10.3 Turn into individual results (scalars)
  #Results for 1st line come from 1st regression, including xc1
  b1=lmc[2,1]
  t1=lmc[2,3]
  p1=lmc[2,4]

  #10.4 Results for 3rd line come
  b3=lmc[4,1]
  t3=lmc[4,3]
  p3=lmc[4,4]

  #10.5 Is the u-shape significant?
  u.3lines =ifelse(b1*b3<0 & p1<.05 & p3<.05,1,0)

  #11 Output is four 1/0 dummies for significant/not of each of the four regressions
  res=c(rmax$u.sig,rmed$u.sig, rprop$u.sig,u.3lines)
  return(res)
  #to find significance values, rmax$p1 and p2 wwill be the p-values for each line
  #u.sig =ifelse(b1*b2<0 & p1<.05 & p2<.05,1,0)
  #considered significant if slopes are of opposite signs and both slope p-values are significant
}
