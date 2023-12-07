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

  est_slopes <- find_segments(est_slopes)

  return(est_slopes)
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
  slope_sections <- dplyr::group_by(est_slopes, seg_id)
  slope_sections <- dplyr::summarize(slope_sections, slope_sign=mean(slope_sign))
  slope_sections <- dplyr::ungroup(slope_sections)
  slope_sections$lag <- dplyr::lag(slope_sections$slope_sign)
  slope_sections$lead <- dplyr::lead(slope_sections$slope_sign)
  slope_sections$feature <- ifelse(slope_sections$slope_sign==1, "increase",
                                   ifelse(slope_sections$slope_sign==-1, "decrease",
                                          ifelse(slope_sections$lag==1 & slope_sections$lead == -1 , "peak",
                                                 ifelse(slope_sections$lag== -1 & slope_sections$lead == 1, "trough",
                                                        ifelse(slope_sections$lag== 1 & slope_sections$lead == 1, "increase_step",
                                                               ifelse(slope_sections$lag== -1 & slope_sections$lead == -1, "decrease_step",
                                                                      "edge_flat"))))))
  est_slopes <- dplyr::full_join(est_slopes, slope_sections[, -2], by="seg_id")

  return(est_slopes)
}








#
#   test <- data.frame(x = seq(0, 3.96, by=.04),
#                      ytrue=sin(seq(0, 3.96, by=.04)),
#                      y=sin(seq(0, 3.96, by=.04))+rnorm(100, 0, .2))
#   gam1 <- gam(y~s(x), data=test)
#   plot(gam1, residuals=TRUE, cex=1, pch=1)
#   slopes1 <- slopes(gam1)
#   slopes1
#   d10 <- data.frame( rowid= which(slopes1$conf.low<0&slopes1$conf.high>0),
#                      x= slopes1$x[ which(slopes1$conf.low<0&slopes1$conf.high>0)])
#   d10
#   abline(v=min(d10$x), col="red")
#   abline(v=max(d10$x), col="red")
#   abline(v=test$x[which(test$ytrue==max(test$ytrue))], col="darkgreen")
#
#
# 1-fit gam
# 2-find slope
# 3-identify extrema ranges



#   if(est_slopes$conf.low <0 & est_slopes$conf.high <0){
#   return(as.factor(-1))
# } else if (est_slopes$conf.low <=0 & est_slopes$conf.high >=0){
#   return(as.factor(-0))
# } else if (est_slopes$conf.low > 0 & est_slopes$conf.high > 0) {
#   return(as.factor(1))
# } else {
#   return(NA)
# }
# est_slopes$seg_id <- data.table::rleid(est_slopes$lextreme)
# est_slopes$p_or_t <- #FIGURE OUT HOW TO IDENTIFY PEAKS OR MINIMA ???






# mutate(lag=slope_sign-lag(slope_sign),
#        lead=lead(slope_sign)-slope_sign)%>%
# mutate(feature=ifelse(lead==0 & lag==-1,
#                       "inc_to_flat",
#                       ifelse(lead==0 & lag == 1,
#                              "dec_to_flat",
#                              ifelse(lead==-1 & lag == 0,
#                                     "flat_to_dec",
#                                     ifelse(lead == 1 & lag == 0,
#                                            "flat_to_inc", "const")))))
#
# for( i in unique(est_slopes$seg_id)){
#
#   groub_by( grp= lag(slope_sign))
#
#   summarize by group id and do checks on that then rejoin it DUUUUUUHHHHHHHH
