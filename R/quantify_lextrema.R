# quantify_lextrema.R
# Functions to quantify extrema (peaks and troughs)
# Author: Natalie Dupont


#' Estimate the first derivative
#'
#' Evaluates the first derivative at a given (very small) step size for a given gam model input. Note: THIS FUNCTION IS CURRENTLY UNIVARIATE
#'
#' @param model gam model object to be evaluated
#' @param step_size the step size at which to evaluate the first derivative
#' @param conf_level the confidence level (between 0 and 1) at which the confidence interval of the first derivative is estimated
#'
#' @returns a slopes object dataframe built by marginaleffects::slopes, including the model rowid, term, estimate, std.error, conf.low, conf.high, y, x
#' @export quantify_lextrema
quantify_lextrema <- function(model, step_size = NULL, conf_level= 0.95){
  stopifnot( class(model)%in% c("gam", "glm", "lm") )

  if (is.null(step_size)){
    range <- max(model$model[2])- min(model$model[2])
    unique_x <- length(unique(model$model[2]))

    avg_inp_step_size <- range/unique_x

    step_size <- avg_inp_step_size/1000
    warning(paste0("step_size not defined. step_size will be defined as the average step size/1000. Step_size = ", step_size))
  }

  new_x <- seq(min(model$model[2]), max(model$model[2]), by=step_size)

  est_slopes <- marginaleffects::slopes(model = model,
                                        newdata = marginaleffects::datagrid(x = new_x),
                                        conf_level = conf_level)

  quant_segments <- find_segments(est_slopes)

  return(quant_segments)
}

#' Identify the local extremas
#'
#' @param est_slopes an output of the marginaleffects::slopes function, including the row ID, the x and y values, the conf.low and conf.high
#' @export find_segments
find_segments <- function(est_slopes){
  #Checking that the slopes entry includes the necessary variables
  stopifnot("rowid" %in% colnames(est_slopes),
            ("estimate" %in% colnames(est_slopes)),
            ("std.error" %in% colnames(est_slopes)),
            ("conf.low" %in% colnames(est_slopes)),
            ("conf.high" %in% colnames(est_slopes)),
            ("y" %in% colnames(est_slopes)),
            ("x" %in% colnames(est_slopes)))

  #finding regions where the first derivative confidence interval includes 0
  est_slopes$lextreme <- est_slopes$conf.low <0 & est_slopes$conf.high >0
  est_slopes$slope_sign <- ifelse(est_slopes$conf.low <0 & est_slopes$conf.high <0,
                                  -1,
                                  ifelse(est_slopes$conf.low <=0 & est_slopes$conf.high >=0,
                                         0, 1))
  est_slopes$seg_id <- data.table::rleid(est_slopes$lextreme)

  est_slopes <- .eval_segments(est_slopes)


  return(est_slopes)
}


#' Evaluate  sections
#'
#' Evaluates sections identified by find_lextrema (confidence interval of first derivative does not exclude 0)  to identify it as a peak, trough, upper step or lower step
#'
#' @param est_slopes gam model object to be evaluated
#'
#' @returns returns the est_slopes object with an added column defining the sections
.eval_segments <- function(est_slopes){
  slope_segments <- dplyr::group_by(est_slopes, seg_id)
  slope_segments <- dplyr::summarize(slope_segments, slope_sign=mean(slope_sign), x_start=dplyr::first(x), x_end=dplyr::last(x))
  slope_segments <- dplyr::ungroup(slope_segments)
  slope_segments$lag <- dplyr::lag(slope_segments$slope_sign)
  slope_segments$lead <- dplyr::lead(slope_segments$slope_sign)
  slope_segments$feature <- ifelse(slope_segments$slope_sign==1, "increase",
                                   ifelse(slope_segments$slope_sign==-1, "decrease",
                                          ifelse(slope_segments$lag==1 & slope_segments$lead == -1 , "peak",
                                                 ifelse(slope_segments$lag== -1 & slope_segments$lead == 1, "trough",
                                                        ifelse(slope_segments$lag== 1 & slope_segments$lead == 1, "increase_step",
                                                               ifelse(slope_segments$lag== -1 & slope_segments$lead == -1, "decrease_step",
                                                                      "edge_flat"))))))
  est_slopes <- dplyr::full_join(est_slopes, slope_segments[, -2], by="seg_id")


  segments <- list("model_slopes"=est_slopes,
                   "segment_summary"= slope_segments)

  return(segments)
}





quantify_sig_sin <- quantify_lextrema(gam_sig_sin)
#
# sig_sin_plot <- draw(gam_sig_sin, residuals = TRUE)

smooth_sig_sin <- smooth_estimates(gam_sig_sin)
confint_sig_sin <- add_confint(smooth_sig_sin)

ggplot()+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = x),
                      alpha = 0.2, data=confint_sig_sin) +
  geom_line(aes(x=gam_sig_sin$model$x, y=gam_sig_sin$fitted.values-coef(gam_sig_sin)[1]))+
  geom_rect(aes(xmin=quantify_sig_sin$segment_summary$x_start, xmax=quantify_sig_sin$segment_summary$x_end, ymin=-Inf, ymax=Inf, fill=quantify_sig_sin$segment_summary$feature), alpha=.2)+
  geom_point(aes(x=quantify_sig_sin$x, y=quantify_sig_sin$slope_sign, colour=quantify_sig_sin$feature))

ggplot(data=quantify_sig_sin$model_slopes)+
  geom_point(aes(x=x, y=estimate, colour=feature), size=0.1)+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.5)
