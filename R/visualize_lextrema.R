# visualize_lextremagam.R
# Functions to visualize extrema (peaks and troughs)
# Author: Natalie Dupont

#' Plot the lextrema (STILL UNIVARIATE)
#'
#' Returns a plot object
#'
#' @param quant_segments a lextrema object produced by the function quantify_lextrema
#' @param plot_deriv boolean TRUE/FALSE if the first derivative should be plotted
#' @param show_segs which segment types to show. see lextremagam::feature_codes for the options
#' @param show_segs_deriv TRUE/FALSE if the derivative plot should be produced
#'
#' @returns a ggplot object
#' @export plot_lextrema
plot_lextrema <- function(quant_segments, plot_deriv = TRUE, show_segs=c("local_max", "local_min"), show_segs_deriv = TRUE){
  shows_segs <- show_segs
  if(! "lextrema" %in% class(quant_segments)){
    stop("quant_segments must be an object of class lextrema produced by the quantify_lextrema function")
  }
  if(! is.logical(plot_deriv)){
    stop("plot_deriv must be a logical TRUE/FALSE value")
  }
  if (!all(show_segs %in% feature_codes)){
    if( show_segs != "all"){
      stop(paste0("all show_segs values must be codes represented in the approved feature codes: ", paste0(feature_codes, collapse = ", ")))
    }
  }
  if(any(show_segs == "all")){
    show_segs <- feature_codes
  }

  new_x <-  dplyr::select(quant_segments$model_slopes, quant_segments$var)

  if(quant_segments$deriv_method == "gratia"){
    colnames(quant_segments$model_slopes)[which(colnames(quant_segments$model_slopes)==".lower_ci")] <- "conf.low"
    colnames(quant_segments$model_slopes)[which(colnames(quant_segments$model_slopes)==".upper_ci")] <- "conf.high"
    colnames(quant_segments$model_slopes)[which(colnames(quant_segments$model_slopes)==".derivative")] <- "estimate"

    predicted <- predict.gam(object = quant_segments$model, type = "link", newdata = new_x)

    quant_segments$model_slopes$predicted <- predicted
    }

  seg_data <- dplyr::filter(quant_segments$model_slopes, feature %in% show_segs|is.na(feature))

  model_plot <- marginaleffects::plot_predictions(quant_segments$model, by=quant_segments$var, newdata = new_x)
  model_plot <- model_plot+ggplot2::geom_line(data=seg_data,
                                     ggplot2::aes(x = unlist(dplyr::select(seg_data, quant_segments$var)), y = predicted, group = seg_id, colour = feature),
                                     linewidth = 1.2)+
    ggplot2::theme_bw()

  if(plot_deriv){

    if(show_segs_deriv){
      deriv_plot <- ggplot2::ggplot(data=quant_segments$model_slopes)+
        ggplot2::geom_line(
          ggplot2::aes(x=unlist(dplyr::select(quant_segments$model_slopes, quant_segments$var)),
                       y=estimate))+
        ggplot2::geom_ribbon(ggplot2::aes(x=unlist(dplyr::select(quant_segments$model_slopes, quant_segments$var)),
                                          ymin=conf.low, ymax=conf.high, group = seg_id, fill = feature), alpha=0.25)+
        ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype=3, colour="grey40", linewidth=1.1)+
        ggplot2::theme_bw()
    } else{
      deriv_plot <- ggplot2::ggplot(data=quant_segments$model_slopes)+
        ggplot2::geom_line(
          ggplot2::aes(x=unlist(dplyr::select(quant_segments$model_slopes, quant_segments$var)), y=estimate))+
        ggplot2::geom_ribbon(ggplot2::aes(x=unlist(dplyr::select(quant_segments$model_slopes, quant_segments$var)), ymin=conf.low, ymax=conf.high), fill="black", alpha=0.25)+
        ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype=3, colour="grey40", linewidth=1.1)+
        ggplot2::theme_bw()
    }
  }
 ####DEPENDS ON Predictor variable always being in column 13, which is not always the case. Change to be variable dependent
  print(model_plot)
  if(plot_deriv){
    print(deriv_plot)
  }

  if(!plot_deriv){
    plots <- model_plot
  }else {
    plots <- list(model_plot = model_plot, deriv_plot = deriv_plot)
  }
  return(plots)
}















