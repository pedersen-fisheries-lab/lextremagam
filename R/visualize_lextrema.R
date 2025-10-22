# visualize_lextremagam.R
# Functions to visualize extrema (peaks and troughs)
# Author: Natalie Dupont

#' Plot the lextrema
#'
#' Returns a plot showing delineated segments along a smooth when given a lextrema object. The lextrema object can be univariate or multivariate. Can also provide the first derivative plot of the smooth.
#'
#' @param lextr a lextrema object produced by the function quantify_lextrema
#' @param plot_deriv boolean TRUE/FALSE if the first derivative should be plotted
#' @param show_segs which segment types to show. see lextremagam::feature_codes for the options
#' @param show_segs_deriv TRUE/FALSE if the derivative plot should show segments
#' @param type on what scale to draw the data. The options are "response" or "link" with "response" as the default
#'
#' @returns one or two ggplot object
#' @export plot_lextrema
plot_lextrema <- function(lextr, plot_deriv = TRUE, show_segs=c("local_max", "local_min"), show_segs_deriv = TRUE, type = "response"){
  shows_segs <- show_segs
  if(! "lextrema" %in% class(lextr)){
    stop("lextr must be an object of class lextrema produced by the quantify_lextrema function")
  }
  if(! is.logical(plot_deriv)){
    stop("plot_deriv must be a logical TRUE/FALSE value")
  }
  if (!all(show_segs %in% feature_codes)){
    if( show_segs[1] != "all"){
      stop(paste0("all show_segs values must be codes represented in the approved feature codes: ", paste0(feature_codes, collapse = ", ")))
    }
  }

  if("multilextrema"%in% class(lextr)){
    plots <- .plot_lextrema_multivar(lextr, plot_deriv = plot_deriv, show_segs=show_segs, show_segs_deriv = show_segs_deriv, type = type)
  } else if ("unilextrema"%in% class(lextr)) {
    plots <- .plot_lextrema_univar(lextr, plot_deriv = plot_deriv, show_segs=show_segs, show_segs_deriv = show_segs_deriv, type = type)
  }

  return(plots)
}

#' Plot the lextrema (STILL UNIVARIATE)
#'
#' Returns a plot object
#'
#' @param lextr a lextrema object produced by the function quantify_lextrema
#' @param plot_deriv boolean TRUE/FALSE if the first derivative should be plotted
#' @param show_segs which segment types to show. see lextremagam::feature_codes for the options
#' @param show_segs_deriv TRUE/FALSE if the derivative plot should be produced
#' @param type on what scale to draw the data. The options are "response" or "link" with "response" as the default
#'
#' @returns a ggplot object
.plot_lextrema_univar <- function(lextr, plot_deriv = TRUE, show_segs=c("local_max", "local_min"), show_segs_deriv = TRUE, type = "response"){
  if(length(gratia::model_vars(lextr$model))>1){
    stop("Sorry, plot_lextrema is not yet updated to visualize results from multivariate models")
  }

  if(any(show_segs == "all")){
    show_segs <- feature_codes
  }

  new_x <-  dplyr::select(lextr$model_slopes, lextr$var)

  if(lextr$deriv_method == "gratia"){
    colnames(lextr$model_slopes)[which(colnames(lextr$model_slopes)==".lower_ci")] <- "conf.low"
    colnames(lextr$model_slopes)[which(colnames(lextr$model_slopes)==".upper_ci")] <- "conf.high"
    colnames(lextr$model_slopes)[which(colnames(lextr$model_slopes)==".derivative")] <- "estimate"

    predicted <- mgcv::predict.gam(object = lextr$model, type = type, newdata = new_x)

    lextr$model_slopes$predicted <- predicted
  }

  seg_data <- dplyr::filter(lextr$model_slopes, feature %in% show_segs|is.na(feature))

  model_plot <- marginaleffects::plot_predictions(lextr$model, by=lextr$var, newdata = new_x, type = type)
  model_plot <- model_plot+ggplot2::geom_line(data=seg_data,
                                              ggplot2::aes(x = unlist(dplyr::select(seg_data, lextr$var)), y = predicted, group = seg_id, colour = feature),
                                              linewidth = 1.2)+
    ggplot2::theme_bw()

  if(plot_deriv){

    if(show_segs_deriv){
      deriv_plot <- ggplot2::ggplot(data=lextr$model_slopes)+
        ggplot2::geom_line(
          ggplot2::aes(x=unlist(dplyr::select(lextr$model_slopes, lextr$var)),
                       y=estimate))+
        ggplot2::geom_ribbon(ggplot2::aes(x=unlist(dplyr::select(lextr$model_slopes, lextr$var)),
                                          ymin=conf.low, ymax=conf.high, group = seg_id, fill = feature), alpha=0.25)+
        ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype=3, colour="grey40", linewidth=1.1)+
        ggplot2::theme_bw()
    } else{
      deriv_plot <- ggplot2::ggplot(data=lextr$model_slopes)+
        ggplot2::geom_line(
          ggplot2::aes(x=unlist(dplyr::select(lextr$model_slopes, lextr$var)), y=estimate))+
        ggplot2::geom_ribbon(ggplot2::aes(x=unlist(dplyr::select(lextr$model_slopes, lextr$var)), ymin=conf.low, ymax=conf.high), fill="black", alpha=0.25)+
        ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype=3, colour="grey40", linewidth=1.1)+
        ggplot2::theme_bw()
    }
  }
  ####DEPENDS ON Predictor variable always being in column 13, which is not always the case. Change to be variable dependent
  # print(model_plot)
  # if(plot_deriv){
  #   print(deriv_plot)
  # }

  if(!plot_deriv){
    plots <- model_plot
  }else {
    plots <- list(model_plot = model_plot, deriv_plot = deriv_plot)
  }
  return(plots)
}

#' Plot the lextrema multivariate
#'
#' Returns a plot object
#'
#' @param lextr a lextrema object produced by the function quantify_lextrema
#' @param plot_deriv boolean TRUE/FALSE if the first derivative should be plotted
#' @param show_segs which segment types to show. see lextremagam::feature_codes for the options
#' @param show_segs_deriv TRUE/FALSE if the derivative plot should be produced
#' @param type if the data should be plotted on the repsonse scale (default) or link scale
#'
#' @returns a ggplot object
.plot_lextrema_multivar <- function(lextr, plot_deriv = TRUE, show_segs=c("local_max", "local_min"), show_segs_deriv = TRUE, type = "response"){

  if(any(show_segs == "all")){
    show_segs <- feature_codes
  }

  new_x <-  dplyr::select(lextr$model_slopes, lextr$var)
  by <- lextr$model_slopes$.by[1]
  if(!is.na(by)){
    by <- dplyr::select(lextr$model_slopes, by)
    new_x <- cbind(new_x, by)
  }

  if(ncol(new_x) < (ncol(lextr$model$model) - 1)) {
    for(i in setdiff(colnames(lextr$model$model)[-1], colnames(new_x))) {
      if(is.numeric(lextr$model$model[[i]])) {
        new_x <- dplyr::mutate(new_x, new_var = mean(lextr$model$model[[i]]))
      } else if (is.factor(lextr$model$model[[i]]) | is.character(lextr$model$model[[i]])) {
          new_x <- dplyr::mutate(new_x, new_var = dplyr::first(lextr$model$model[[i]]))
      }
      colnames(new_x)[colnames(new_x) == "new_var"] <- i
    }
    }

  # #add back in. variable names
  # colnames(new_x) <- colnames(lextr$model$model)[-1]

  if(lextr$deriv_method == "gratia"){
    colnames(lextr$model_slopes)[which(colnames(lextr$model_slopes)==".lower_ci")] <- "conf.low"
    colnames(lextr$model_slopes)[which(colnames(lextr$model_slopes)==".upper_ci")] <- "conf.high"
    colnames(lextr$model_slopes)[which(colnames(lextr$model_slopes)==".derivative")] <- "estimate"

    predicted <- mgcv::predict.gam(object = lextr$model, type = type, newdata = new_x)

    lextr$model_slopes$predicted <- predicted
  }

  seg_data <- dplyr::filter(lextr$model_slopes, feature %in% show_segs|is.na(feature))

  model_plot <- marginaleffects::plot_predictions(lextr$model, by=lextr$var, newdata = new_x, type = type)
  model_plot <- model_plot+ggplot2::geom_line(data=seg_data,
                                              ggplot2::aes(x = unlist(dplyr::select(seg_data, lextr$var)), y = predicted, group = seg_id, colour = feature),
                                              linewidth = 1.2)+
    ggplot2::theme_bw()

  if(plot_deriv){

    if(show_segs_deriv){
      deriv_plot <- ggplot2::ggplot(data=lextr$model_slopes)+
        ggplot2::geom_line(
          ggplot2::aes(x=unlist(dplyr::select(lextr$model_slopes, lextr$var)),
                       y=estimate))+
        ggplot2::geom_ribbon(ggplot2::aes(x=unlist(dplyr::select(lextr$model_slopes, lextr$var)),
                                          ymin=conf.low, ymax=conf.high, group = seg_id, fill = feature), alpha=0.25)+
        ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype=3, colour="grey40", linewidth=1.1)+
        ggplot2::theme_bw()
    } else{
      deriv_plot <- ggplot2::ggplot(data=lextr$model_slopes)+
        ggplot2::geom_line(
          ggplot2::aes(x=unlist(dplyr::select(lextr$model_slopes, lextr$var)), y=estimate))+
        ggplot2::geom_ribbon(ggplot2::aes(x=unlist(dplyr::select(lextr$model_slopes, lextr$var)), ymin=conf.low, ymax=conf.high), fill="black", alpha=0.25)+
        ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype=3, colour="grey40", linewidth=1.1)+
        ggplot2::theme_bw()
    }
  }
  # print(model_plot)
  # if(plot_deriv){
  #   print(deriv_plot)
  # }

  if(!plot_deriv){
    plots <- model_plot
  }else {
    plots <- list(model_plot = model_plot, deriv_plot = deriv_plot)
  }
  return(plots)
}













