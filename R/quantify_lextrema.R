# quantify_lextrema.R
# Functions to quantify extrema (peaks and troughs)
# Author: Natalie Dupont

#' NOT TO BE USED Estimate the first derivative - alpha-corrected
#'
#' Evaluates the first derivative at a given (very small) step size for a given gam model input. Note: THIS FUNCTION IS CURRENTLY UNIVARIATE
#'
#' @param mod gam model object to be evaluated
#' @param var predictor variable over which the slope is evaluated
#' @param step_size the step size at which to evaluate the first derivative
#' @param conf_level the confidence level (between 0 and 1) at which the confidence interval of the extrema is to be estimated
#' @param deriv_method whether to use gratia's derivatives function or marginaleffects' slopes function
#' @param multivariate TRUE/FALSE if the model to be evaluated is multivariate
#' @param smooth name of the smooth in the format \"s(...)\"
#' @param frequentist if using gratia::derivatives, TRUE/FALSE boolean whether or not the derivatives are calculated using the bayesian (default) or frequentist covariance matrix
#'
#' @returns a slopes object dataframe built by marginaleffects::slopes, including the model rowid, term, estimate, std.error, conf.low, conf.high, y, x
quantify_lextrema2 <- function(mod, var = NULL, step_size = NULL, conf_level= 0.95, deriv_method = c("gratia", "marginaleffects"), multivariate = FALSE, smooth = NULL, frequentist = FALSE){
  deriv_method <- match.arg(deriv_method)
  #Error management
  #general checks
  stopifnot( any(class(mod)=="gam"),
             var %in% gratia::model_vars(mod),
             is.numeric(step_size) | is.null(step_size),
             is.numeric(conf_level),
             conf_level >0 & conf_level < 1)

  if(is.null(smooth) & multivariate){
    smooth <- paste0("s(", var, ")")
  }

  if (! any(class(mod) == "gam")) {
    warning("Your mod object does not seem to be a gam. This method has not been tested on non-gam objects and may not operate well")
  }

  #Makeing sure the model is univariate
  if(length(gratia::model_vars(mod))>1 & multivariate == FALSE){
    stop("this is a multivariate model. Select multivariate = TRUE to proceed")
  }

  .check_gam(mod = mod, var = var, smooth = smooth, multivariate)

  #corrrecting conf-level
  conf_level_corrected <- 1-sqrt(1-conf_level)

  if (multivariate){
    .quantify_lextrema_multivar(var=var,
                                mod = mod,
                                smooth = smooth,
                                step_size = step_size,
                                conf_level= conf_level_corrected,
                                deriv_method = deriv_method,
                                frequentist = frequentist)
  } else {
    quantify_lextrema(mod = mod,
                      var = var,
                      step_size = step_size,
                      conf_level= conf_level_corrected,
                      deriv_method = deriv_method,
                      frequentist = frequentist)
  }
}

#' Estimate the first derivative
#'
#' Evaluates the first derivative at a given (very small) step size for a given gam model input. Note: THIS FUNCTION IS CURRENTLY UNIVARIATE
#'
#' @param mod gam model object to be evaluated
#' @param var predictor variable name to be evaluated
#' @param smooth smooth name to be evaluated in the format \"s()\" (ex: \"s(temp)\")
#' @param step_size the step size at which to evaluate the first derivative
#' @param conf_level the confidence level (between 0 and 1) at which the confidence interval of the first derivative is estimated
#' @param deriv_method whether to use gratia's derivatives function or marginaleffects' slopes function
#' @param, if using gratia::derivatives, TRUE/FALSE boolean whether or not the derivatives are calculated using the bayesian (default) or frequentist covariance matriz
#'
#' @returns a list object built by marginaleffects::slopes or gratia::derivative, including the model rowid, term, estimate, std.error, conf.low, conf.high, y, x
#' @export quantify_lextrema_multivar

quantify_lextrema_multivar <- function(mod, var=NULL, smooth = NULL, step_size = NULL, conf_level= 0.95, deriv_method = c("gratia", "marginaleffects"), frequentist = FALSE){

  deriv_method <- match.arg(deriv_method)
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

  if(is.null(var)){
    var <- substring(smooth, 3, nchar(smooth)-1)
    warning(paste0("No var is provided. This is very risky. Var extracted from smooth and has been set to \"", var, "\". If this is incorrect, please enter the true var value"))
  }
  #variable management
  #Checking the step-size
  range <- max(dplyr::select(mod$model, all_of(var)))- min(dplyr::select(mod$model, all_of(var)))
  if (is.null(step_size)){
    unique_x <- nrow(unique(dplyr::select(mod$model, all_of(var)))) - 1 #not quite

    avg_inp_step_size <- range/unique_x

    step_size <- avg_inp_step_size/100
    warning(paste0("step_size not defined. step_size will be defined as the average x interval between datapoints/100. Step_size = ", step_size))
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

  #getting first derivative estimates
  if(deriv_method == "marginaleffects"){
    stop("Sorry, multivariate analysis of lextremagam using marginaleffects has not yet been implemented. Please use deriv_methd= \"gratia\"")
    # est_slopes <- marginaleffects::slopes(model = mod,
    #                                       newdata = new_x,
    #                                       conf_level = conf_level,
    #                                       type = "link")
  } else if (deriv_method == "gratia"){
    #default returns on link scale
    #MUST FIGURE OUT HOW TO CONTROL FOR ALL OTHER VARIAVLES IN THE NEW_X
    est_slopes <- gratia::derivatives(object = mod,
                                      select = smooth,
                                      n = range/step_size,
                                      order=1,
                                      type = "central",
                                      interval = "confidence",
                                      level = conf_level,
                                      frequentist = frequentist)
  }else{
    stop("invalid deriv_method. Must be \"gratia\" or \"marginaleffects\".")
  }

  #characterizing the curve segments
  quant_segments <- find_segments(est_slopes, var, deriv_method)
  quant_segments$model <- mod
  quant_segments$var <- var
  quant_segments$smooth <- smooth
  quant_segments$deriv_method <- deriv_method

  class(quant_segments) <- c("lextrema", "multilextrema", class(quant_segments))

  return(quant_segments)
}

#' Estimate the first derivative
#'
#' Evaluates the first derivative at a given (very small) step size for a given gam model input. Note: THIS FUNCTION IS CURRENTLY UNIVARIATE
#'
#' @param mod gam model object to be evaluated
#' @param var predictor variable over which the slope is evaluated
#' @param step_size the step size at which to evaluate the first derivative
#' @param conf_level the confidence level (between 0 and 1) at which the confidence interval of the first derivative is estimated
#' @param deriv_method whether to use gratia's derivatives function or marginaleffects' slopes function
#' @param frequentist if using gratia::derivatives, TRUE/FALSE boolean whether or not the derivatives are calculated using the bayesian (default) or frequentist covariance matrix
#'
#' @returns a slopes object dataframe built by marginaleffects::slopes, including the model rowid, term, estimate, std.error, conf.low, conf.high, y, x
#' @export quantify_lextrema
quantify_lextrema <- function(mod, var = NULL, step_size = NULL, conf_level= 0.95, deriv_method = c("gratia", "marginaleffects"), frequentist = FALSE){
  deriv_method <- match.arg(deriv_method)
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
      unique_x <- nrow(unique(mod$model[2])) - 1 #not quite

      avg_inp_step_size <- range/unique_x

      step_size <- avg_inp_step_size/100
      warning(paste0("step_size not defined. step_size will be defined as the average x interval between datapoints/100. Step_size = ", step_size))
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
  if(deriv_method == "marginaleffects"){
    est_slopes <- marginaleffects::slopes(model = mod,
                                          newdata = new_x,
                                          conf_level = conf_level,
                                          type = "link")
  } else if (deriv_method == "gratia"){
    #default returns on link scale
    est_slopes <- gratia::derivatives(object = mod,
                                      data = new_x,
                                      order=1,
                                      type = "central",
                                      interval = "confidence",
                                      level = conf_level,
                                      frequentist=frequentist)
  }else{
    stop("invalid deriv_method. Must be \"gratia\" or \"marginaleffects\".")
  }

  #characterizing the curve segments
  quant_segments <- find_segments(est_slopes, var, deriv_method)
  quant_segments$model <- mod
  quant_segments$var <- var
  quant_segments$deriv_method <- deriv_method

  class(quant_segments) <- c("lextrema", "unilextrema",  class(quant_segments))

  return(quant_segments)
}

#' Identify the local extremas
#'
#' @param est_slopes an output of the marginaleffects::slopes function, including the row ID, the x and y values, the conf.low and conf.high
#' @param var predictor variable name
#' @param deriv_method gratia or marginaleffects
#'
#' @export find_segments
find_segments <- function(est_slopes, var, deriv_method){
  #Checking that the est_slopes entry includes the necessary variables
  stopifnot(any(class(est_slopes) == "slopes") | any(class(est_slopes) == "marginaleffects" | any(class(est_slopes) == 'derivatives')),
            ("estimate" %in% colnames(est_slopes) | "derivative" %in% colnames(est_slopes) | var %in% colnames(est_slopes)),
            ("std.error" %in% colnames(est_slopes)| "se" %in% colnames(est_slopes) | ".se" %in% colnames(est_slopes)),
            ("conf.low" %in% colnames(est_slopes)| "lower" %in% colnames(est_slopes) | ".lower_ci" %in% colnames(est_slopes)),
            ("conf.high" %in% colnames(est_slopes)| "upper" %in% colnames(est_slopes) | ".upper_ci" %in% colnames(est_slopes)))

  #finding regions where the first derivative confidence interval includes 0
  if(deriv_method =="marginaleffects"){
  est_slopes$slope_sign <- ifelse(est_slopes$conf.low <0 & est_slopes$conf.high <0,
                                  -1,
                                  ifelse(est_slopes$conf.low <=0 & est_slopes$conf.high >=0,
                                         0, 1))
  }else if(deriv_method =="gratia"){
    est_slopes$slope_sign <- ifelse(est_slopes$.lower_ci <0 & est_slopes$.upper_ci <0,
                                    -1,
                                    ifelse(est_slopes$.lower_ci <=0 & est_slopes$.upper_ci >=0,
                                           0, 1))
  }else {
    stop("deriv_method is invalid")
  }
  est_slopes$seg_id <- data.table::rleid(est_slopes$slope_sign)

  eval_segments_summary <- .eval_segments(est_slopes, var, deriv_method)


  return(eval_segments_summary)
}


#' Evaluate  sections
#'
#' Evaluates sections identified by find_lextrema (confidence interval of first derivative does not exclude 0)  to identify it as a peak, trough, upper step or lower step
#'
#' @param est_slopes gam model object to be evaluated
#' @param var predictor variable name
#' @param deriv_method gratia or marginal effects
#'
#' @returns returns the est_slopes object with an added column defining the sections
.eval_segments <- function(est_slopes, var, deriv_method){
  #renaming predictor value
  if(deriv_method == "marginaleffects"){
    colnames(est_slopes)[which(colnames(est_slopes)==var)] <- "x"
  }else if (deriv_method == "gratia"){
    colnames(est_slopes)[which(colnames(est_slopes)==var)] <- "x"
  }else{
    stop('invalid deriv_method')
  }

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
    slope_segments$slope_sign == 0 & slope_segments$lag == 1 & slope_segments$lead == -1 ~ "local_max",
    slope_segments$slope_sign == 0 & slope_segments$lag == -1 & slope_segments$lead == 1 ~ "local_min",
    slope_segments$slope_sign == 0 & slope_segments$lag == 1 & slope_segments$lead == 1 ~ "plateau",
    slope_segments$slope_sign == 0 & slope_segments$lag == -1 & slope_segments$lead == -1 ~ "plateau",
    slope_segments$slope_sign == 0 & is.na(slope_segments$lag) & slope_segments$lead == 1 ~ "boundary_min",
    slope_segments$slope_sign == 0 & is.na(slope_segments$lag) & slope_segments$lead == -1 ~ "boundary_max",
    slope_segments$slope_sign == 0 & is.na(slope_segments$lead) & slope_segments$lag == 1 ~ "boundary_max",
    slope_segments$slope_sign == 0 & is.na(slope_segments$lead) & slope_segments$lag == -1 ~ "boundary_min",
    slope_segments$slope_sign == 0 & is.na(slope_segments$lag) & is.na(slope_segments$lead) ~ "notrend"
  )

  #rejoining these defining features to the inital slopes dataframe
  est_slopes <- dplyr::full_join(est_slopes, slope_segments[, -2], by="seg_id")

  #renaming column names according to the variable name
  colnames(est_slopes)[which(colnames(est_slopes)=="x")] <- var
  colnames(slope_segments)[which(colnames(slope_segments)=="x_start")] <- paste0(var, "_start")
  colnames(slope_segments)[which(colnames(slope_segments)=="x_end")] <- paste0(var, "_end")

  segments <- list("model_slopes"=est_slopes,
                   "segment_summary"= slope_segments)

  #reassigning the correct name

  return(segments)
}

#' Check that gam object
#'
#' Evaluates if the gam object is appropriate to be passed on to lextrema
#'
#' @param mod gam model object to be evaluated
#' @param var predictor variable name
#' @param smooth for multivariate models, smooth is the smooth term to be evaluated in the format "s(...)"
#' @param multivariate TRUE/FALSE if the model to be evaluated is multivariate or not
#'
#' @returns returns the est_slopes object with an added column defining the sections
.check_gam <- function(mod, var, smooth, multivariate){
  smooth_names <- gratia::smooths(mod)

  if( length(smooth_names)>1 & ! multivariate){
    stop("Your model is multivariate. Please select multivariate = TRUE")
  }
  if(multivariate){
    if (is.null(smooth)){
      stop(paste0("If working with a multivariate model, you must specify a smooth in the function. Select one of the following: \n", gratia::smooths(mod)))
    }
    if (!(smooth %in% smooth_names)){
      stop(paste0("The smooth you have selected is not in the model. Select one of the following: \n", gratia::smooths(mod)))
    }

    smooth_info <- gratia::get_smooth(mod, smooth)
    if (length(smooth_info$term)>1){
      stop(paste0("The smooth selected has more than one term. Lextremagam does not yet work on mutlidimensional smooths. Select a smooth that has only one term"))
    }
  }

  #check that smooth type is appropiate
  smooth_type <- NULL
  if(multivariate){
    smooth_type <- class(smooth_info)[1]
    p_order <- smooth_info$p.order
  } else{
    smooth_type <- class(mod$smooth[[1]])[1]
    p_order <- mod$smooth[[1]]$p.order
  }

  if( ! smooth_type %in% c("tprs.smooth", "Bspline.smooth", "pspline.smooth", "ts.smooth", "cr.smooth")){
    if(smooth_type %in% c("cpspline.smooth", "cyclic.smooth")){
        warning(paste0("Smooth type (", smooth_type, ") is cyclic. The properties of the lextremagam method have not been developed nor tested for cyclic smooths. Proceed with caution"))
    }else if(smooth_type %in% c("random.effect", "mrf.smooth")){
      stop("Smooth type is ", smooth_type, ". This cannot be evaluated using lextremagam. Ensure the smooth is appropriate for the lextremagam. Examples of appropriate smooths are TPRS, adaptive, P-spline, B-spline, CRS.")
    } else {
      warning(paste0("Smooth_type is ", smooth_type, ". This does not correspond to any of the expected smooths (tprs.smooth, Bspline.smooth, pspline.smooth, ts.smooth, cr.smooth). Please proceed with extreme caution and make sure that you understand the behaviour of the smooth with regard to the lextremagam method. Please also ensure that the penalty order is 2 or greater"))
    }
  }

  if(smooth_type %in%c("tprs.smooth", "ts.smooth", "cr.smooth")){
    if (p_order == 1){
      warning("Penalty order is set to 1, which is too low and will give unexpected results. Please set to 2 or greater, or leave a default values")
    }
  }
  if(smooth_type == "pspline.smooth"){
    if(p_order[1]<2){
      warning("Penalty order is set too low, which is too low and will give unexpected results. Please set to 2 or greater, or leave a default values. Ex: m = c(2, ... )")
    }
  }
  if(smooth_type == "Bspline.smooth"){
    if(p_order[2]<2){
      warning("Penalty order is set too low, which is too low and will give unexpected results. Please set to 2 or greater, or leave a default values. Ex: m = c(..., 2 )")
    }
  }
}

