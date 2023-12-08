#Treatments

#error levels: 0.1, 0.2, 0.3, 0.4
#function shapes:
#   Peaked: normal, beta/skewed, off-center-symmetrical,
#   Non-peaked: Concave, linear, convex, sigmoid
#Parameters: either continuous or 20?
#samples size: 20, 40, 60, 80, 100
#replicates: 500 or 1000?

#what to extract from sims: just peak locations (cannot save all gam data)
params <- c(1:10)

#sample size
  n <- c(21, 42, 63, 84, 105)
  n <- c(1, 2, 3, 4, 5)
x <- seq(0,1,length = 21)
#Domain distribution is restricted to 0<=x<=1, and uniformely distributed
error <- c(.2,.3,.4, 0.5)
reps <- 1000

param <- seq(0.05, .95, by= .05)

func <- c("var-location", "var-scale", "var-curvature",
          "var-p-location", "var-skew", "var-p-width")

sim_data <-  tidyr::expand_grid(func = func,
                                param=param,
                                x=x)
sim_data <- dplyr::mutate(sim_data,
                          y_true_unscaled = dplyr::case_when(func=="var-location"~plogis(x, location=param, scale = .1),
                                                      func=="var-scale"~plogis(x, location=0.5, scale = (param*0.4)^2),
                                                      func=="var-curvature"~plogis(x, location=.05, scale = (param*0.5)^2),
                                                      func=="var-p-location"~10*dnorm(x,mean = param, sd = 4/20),
                                                      func=="var-skew"~dbeta(x, shape1=10*param, shape2=10*(1-param)),
                                                      func=="var-p-width"~10*dnorm(x,mean=.5,sd = param^2)))


sim_data <- dplyr::group_by(sim_data,
                            func, param)
sim_data <- dplyr::mutate(sim_data,
                          y_true_scaled = y_true_unscaled-min(y_true_unscaled))
sim_data <- dplyr::mutate(sim_data,
                          y_true_scaled = y_true_scaled/max(y_true_scaled))
sim_data <- dplyr::ungroup(sim_data,
                            func, param)



ggplot(data=sim_data)+
  geom_line(aes(x=x, y=y_true_scaled, colour=as.factor(param)))+
  #geom_point(aes(x=x, y=y_true_unscaled, colour=as.factor(param)))+

  facet_wrap(~func)

