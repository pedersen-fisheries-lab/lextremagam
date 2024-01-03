# quantify_lextrema.R
# Functions to quantify extrema (peaks and troughs)
# Author: Natalie Dupont


#' Estimate the first derivative
#'
#' Evaluates the first derivative at a given (very small) step size for a given gam model input. Note: THIS FUNCTION IS CURRENTLY UNIVARIATE
#'
#' @param mod gam model object to be evaluated
#' @param var predictor variable over which the slope is evaluated
#' @param step_size the step size at which to evaluate the first derivative
#' @param conf_level the confidence level (between 0 and 1) at which the confidence interval of the first derivative is estimated
#'
#' @returns a slopes object dataframe built by marginaleffects::slopes, including the model rowid, term, estimate, std.error, conf.low, conf.high, y, x
#' @export quantify_lextrema
quantify_lextrema <- function(mod, var = NULL, step_size = NULL, conf_level= 0.95){
  #Error management
    #general checks
    stopifnot( any(class(mod)=="gam"),
               var %in% gratia::model_vars(mod),
               is.numeric(step_size) | is.null(step_size),
               is.numeric(conf_level),
               conf_level >0 & conf_level < 1)

    if (! any(class(mod) == "gam")) {
      warning("Your mod object does not seem to be a gam. This method has not been tested on non-gam objects and may not operate well")
    }

    #Makeing sure the model is univariate
    if(length(gratia::model_vars(mod))>1){
      stop("this is a multivariate model. The function is currently only set up to handle univariate models")
    }

  #variable management
    #Checking the step-size
    if (is.null(step_size)){
      range <- max(mod$model[2])- min(mod$model[2])
      unique_x <- nrow(unique(mod$model[2])) - 1

      avg_inp_step_size <- range/unique_x

      step_size <- avg_inp_step_size/1000
      warning(paste0("step_size not defined. step_size will be defined as the average step size/1000. Step_size = ", step_size))
    }
    if(is.numeric(step_size)){
      if(step_size <= 0){
        stop("step_size must be a numeric value greater than 0.")
      }
    }

    #Extracting the predictor variable name
    if(is.null(var)){
      var <- gratia::model_vars(mod)[1]
      warning(paste0("No specific predictor variable was set. The first predictor was extracted. Evaluating the slope of: ", var))
    }

  #generating predictor values to evaluate
  new_x <-  data.frame(x= seq(min(mod$model[2]), max(mod$model[2]), by=step_size))
  names(new_x) <- var

  #getting first derivative estimates
  est_slopes <- marginaleffects::slopes(model = mod,
                                        newdata = new_x,
                                        conf_level = conf_level,
                                        type = "link")

  #characterizing the curve segments
  quant_segments <- find_segments(est_slopes, var)
  quant_segments$model <- mod
  quant_segments$var <- var

  class(quant_segments) <- c("lextrema", class(quant_segments))

  return(quant_segments)
}

#' Identify the local extremas
#'
#' @param est_slopes an output of the marginaleffects::slopes function, including the row ID, the x and y values, the conf.low and conf.high
#' @param var predictor variable name
#'
#' @export find_segments
find_segments <- function(est_slopes, var){
  #Checking that the est_slopes entry includes the necessary variables
  stopifnot(any(class(est_slopes) == "slopes") | any(class(est_slopes) == "marginaleffects"),
            "rowid" %in% colnames(est_slopes),
            ("estimate" %in% colnames(est_slopes)),
            ("std.error" %in% colnames(est_slopes)),
            ("conf.low" %in% colnames(est_slopes)),
            ("conf.high" %in% colnames(est_slopes)))

  #finding regions where the first derivative confidence interval includes 0
  est_slopes$slope_sign <- ifelse(est_slopes$conf.low <0 & est_slopes$conf.high <0,
                                  -1,
                                  ifelse(est_slopes$conf.low <=0 & est_slopes$conf.high >=0,
                                         0, 1))
  est_slopes$seg_id <- data.table::rleid(est_slopes$slope_sign)


  est_slopes <- .eval_segments(est_slopes, var)


  return(est_slopes)
}


#' Evaluate  sections
#'
#' Evaluates sections identified by find_lextrema (confidence interval of first derivative does not exclude 0)  to identify it as a peak, trough, upper step or lower step
#'
#' @param est_slopes gam model object to be evaluated
#' @param var predictor variable name
#'
#' @returns returns the est_slopes object with an added column defining the sections
.eval_segments <- function(est_slopes, var){
  #renaming predictor value
  colnames(est_slopes)[which(colnames(est_slopes)==var)] <- "x"

  #getting a summary table of slope sign sections
  slope_segments <- dplyr::group_by(est_slopes, seg_id)
  slope_segments <- dplyr::summarize(slope_segments, slope_sign=mean(slope_sign), x_start=dplyr::first(x), x_end=dplyr::last(x))
  slope_segments <- dplyr::ungroup(slope_segments)

  #evaluating the slope sign before and after
  slope_segments$lag <- dplyr::lag(slope_segments$slope_sign)
  slope_segments$lead <- dplyr::lead(slope_segments$slope_sign)

  #Defining the characteristic of the slope sign section base on its slope sign and lead-lag slope sign
  slope_segments$feature <- dplyr::case_when(
    slope_segments$slope_sign == 1 ~ "increase",
    slope_segments$slope_sign == -1 ~ "decrease",
    slope_segments$slope_sign == 0 & slope_segments$lag == 1 & slope_segments$lead == -1 ~ "peak",
    slope_segments$slope_sign == 0 & slope_segments$lag == -1 & slope_segments$lead == 1 ~ "trough",
    slope_segments$slope_sign == 0 & slope_segments$lag == 1 & slope_segments$lead == 1 ~ "increase_step",
    slope_segments$slope_sign == 0 & slope_segments$lag == -1 & slope_segments$lead == -1 ~ "decrease_step",
    slope_segments$slope_sign == 0 & is.na(slope_segments$lag) ~ "start_edge_flat",
    slope_segments$slope_sign == 0 & is.na(slope_segments$lead) ~ "end_edge_flat"
  )

  #rejoining these defining features to the inital slopes dataframe
  est_slopes <- dplyr::full_join(est_slopes, slope_segments[, -2], by="seg_id")

  # #defining peak and trough and edge-flat confidence intervals
  # intervals <- dplyr::filter(slope_segments, feature == "peak" | feature == "trough" | grepl(pattern = "edge_flat", x = feature))
  # intervals <- dplyr::group_by(intervals, feature)
  # intervals <- dplyr:: summarize(intervals,
  #                                conf_low = first())

  #renaming column names according to the variable name
  colnames(est_slopes)[which(colnames(est_slopes)=="x")] <- var
  colnames(slope_segments)[which(colnames(slope_segments)=="x_start")] <- paste0(var, "_start")
  colnames(slope_segments)[which(colnames(slope_segments)=="x_end")] <- paste0(var, "_end")

  segments <- list("model_slopes"=est_slopes,
                   "segment_summary"= slope_segments)

  #reassigning the correct name

  return(segments)
}

#
#
#
#
# quantify_sig_sin <- quantify_lextrema(gam_sig_sin)
# #
# # sig_sin_plot <- draw(gam_sig_sin, residuals = TRUE)
#
# smooth_sig_sin <- smooth_estimates(gam_sig_sin)
# confint_sig_sin <- add_confint(smooth_sig_sin)
#
# ggplot()+
#   geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = x),
#                       alpha = 0.2, data=confint_sig_sin) +
#   geom_line(aes(x=gam_sig_sin$model$x, y=gam_sig_sin$fitted.values-coef(gam_sig_sin)[1]))+
#   geom_rect(aes(xmin=quantify_sig_sin$segment_summary$x_start, xmax=quantify_sig_sin$segment_summary$x_end, ymin=-Inf, ymax=Inf, fill=quantify_sig_sin$segment_summary$feature), alpha=.2)+
#   geom_point(aes(x=quantify_sig_sin$x, y=quantify_sig_sin$slope_sign, colour=quantify_sig_sin$feature))
#
# ggplot(data=quantify_sig_sin$model_slopes)+
#   geom_point(aes(x=x, y=estimate, colour=feature), size=0.1)+
#   geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.5)
