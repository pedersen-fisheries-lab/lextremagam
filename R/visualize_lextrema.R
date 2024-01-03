# visualize_lextremagam.R
# Functions to visualize extrema (peaks and troughs)
# Author: Natalie Dupont

#' Plot the lextrema (STILL UNIVARIATE)
#'
#' Returns a plot object
#'
#' @param quant_segments a lextrema object produced by the function quantify_lextrema
#' @param plot_deriv boolean TRUE/FALSE if the first derivative should be plotted
#'
#' @returns a ggplot object
#' @export plot_lextrema
plot_lextrema <- function(quant_segments, plot_deriv = TRUE, show_segs=c("peak", "trough")){
  shows_segs <- show_segs
  if(! "lextrema" %in% class(quant_segments)){
    stop("quant_segments must be an object of class lextrema produced by the quantify_lextrema function")
  }
  if(! is.logical(plot_deriv)){
    stop("plot_deriv must be a logical TRUE/FALSE value")
  }
  if (!all(show_segs %in% feature_codes) ){
    if( show_segs != "all"){
      stop(paste0("all show_segs values must be codes represented in the approved feature codes: ", paste0(feature_codes, collapse = ", ")))
    }
  }

  seg_data <- dplyr::filter(quant_segments$model_slopes, feature %in% show_segs)

  model_plot <- marginaleffects::plot_predictions(quant_segments$model, by=colnames(quant_segments$model$model)[2])
  model_plot <- model_plot+ggplot2::geom_line(data=seg_data,
                                     ggplot2::aes(x = unlist(dplyr::select(seg_data, quant_segments$var)), y = predicted, group = seg_id, colour = feature))+
    ggplot2::theme_bw()
 ####DEPENDS ON Predictor variable always being in column 13, which is not always the case. Change to be variable dependent
  return(model_plot)
}















