
test_that("quantify_lextrema works with defaults", {
  data(data_sig_sin)

  mod_sig_sin <- mgcv::gam(y~s(x), data=data_sig_sin)

  default_warnings <- capture_warnings(code = quantify_lextrema(mod = mod_sig_sin))

  #check for correct wornings
  expect_match(object = default_warnings,
               "step_size not defined. step_size will be defined as the average x interval between datapoints/100. Step_size = 0.0025",
               all = FALSE)

  expect_match(object = default_warnings,
               "No specific predictor variable was set. The first predictor was extracted",
               all = FALSE)

  suppressWarnings( expect_no_error(object = quantify_lextrema(mod = mod_sig_sin)))

  suppressWarnings(expect_type(object = quantify_lextrema(mod = mod_sig_sin),
                               type = "list"))

  suppressWarnings(expect_type(object = quantify_lextrema(mod = mod_sig_sin),
                               type = "list"))

})

test_that("quantify_lextrema works with valid set conf_level, step_size and var", {
  data(data_sig_sin)
  mod_sig_sin <- mgcv::gam(y~s(x), data=data_sig_sin)

  expect_no_error(object = quantify_lextrema(mod = mod_sig_sin, var = "x", step_size = 0.001, conf_level = .96))

  expect_no_warning(object = quantify_lextrema(mod = mod_sig_sin, var = "x", step_size = 0.001, conf_level = .96))

  expect_type(object = quantify_lextrema(mod = mod_sig_sin, var = "x", step_size = 0.001, conf_level = .96),
              type = "list")
})

test_that("inappropriate arguments return correct response ",{
  data(data_sig_sin)
  mod_sig_sin <- mgcv::gam(y~s(x), data=data_sig_sin)

  #inappropriate var
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, var = NA))
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, var = "not present"))
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, var = 1))
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, var = TRUE))

  #inappropriate step_size
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, step_size = NA))
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, step_size = "1"))
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, step_size = -1))
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, step_size = 0))

  #inapprorpaite confidence level
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, conf_level = NA))
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, conf_level = NULL))
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, conf_level = "95"))
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, conf_level = 1))
  expect_error(object = quantify_lextrema(mod = mod_sig_sin, conf_level = 0))
})

#testthat all components are present
test_that("lextrema object has the right structure",{
  data(data_sig_sin)
  mod_sig_sin <- mgcv::gam(y~s(x), data=data_sig_sin)
  lextr <- suppressWarnings(quantify_lextrema(mod = mod_sig_sin))

  #check that all elements are there
  expect_length(lextr, 5)
  expect_contains(object =names(lextr), expected = c( "model_slopes","segment_summary", "model","var","deriv_method"))
  expect_equal(mod_sig_sin,lextr$model) #model copied over model list entry exactly
  expect_contains(object =colnames(lextr$model_slopes),
                  expected = c(".smooth",".by",".fs",".derivative" ,".se",".crit",".lower_ci",".upper_ci","x","slope_sign" , "seg_id","x_start","x_end","lag","lead","feature"))
  expect_contains(object =colnames(lextr$segment_summary),
                  expected = c("seg_id","slope_sign", "x_start","x_end","lag","lead","feature" ))

})

test_that("returns correct results", {
  data(data_sig_sin)
  data("data_compr")

  #testing that a peaked dataset indeed finds a peak
  data_peaked <- dplyr::filter(data_sig_sin, x>=9.75)
  mod <- mgcv::gam(y_true~s(x), data=data_peaked, method = "REML")
  lextr <- suppressWarnings(quantify_lextrema(mod = mod))

  #check that it returns an appropriate summary for everything
  expect_equal(lextr$segment_summary$feature, c("increase", "local_max", "decrease") )
  expect_equal(lextr$segment_summary$x_start, c(9.750, 12.545, 12.580), tolerance = 1e-2)
  expect_equal(lextr$segment_summary$x_end, c(12.5425, 12.5775, 15.0000), tolerance = 1e-2)

  #testing that a non-peaked dataset does not find a peak
  data_npeaked <- dplyr::filter(data_compr, x>=213)
  mod <- mgcv::gam(y~s(x), data=data_npeaked, method = "REML")
  lextr <- suppressWarnings(quantify_lextrema(mod = mod))

  #check that it returns an appropriate summary for everything
  expect_equal(lextr$segment_summary$feature, c("notrend") )
  expect_equal(lextr$segment_summary$x_start,min(data_npeaked$x))
  expect_equal(lextr$segment_summary$x_end, max(data_npeaked$x))
})

test_that("multilextrema objects are approrpiate",{
  data("data_multivar")

  mod_sep <- mgcv::gam(y~s(x1)+x2+s(x3), data=data_multivar, method="REML")

  expect_error(quantify_lextrema(mod = mod_sep), "this is a multivariate model. The function is currently only set up to handle univariate models")

  expect_no_error(suppressWarnings(quantify_lextrema_multivar(mod = mod_sep, var = "x1", smooth = "s(x1)")))
  expect_error(quantify_lextrema_multivar(mod = mod_sep))
  expect_error(quantify_lextrema_multivar(mod = mod_sep, var = "x2", smooth = "s(x2)"),
               "Defined variable \"x2\" is not numeric. Please use a numeric variable")
  expect_warning(quantify_lextrema_multivar(mod = mod_sep,smooth = "s(x1)", step_size =  0.01))

  #var 1, var 2, only var, only smooth,
  quant_mod_sep_v1 <- quantify_lextrema_multivar(mod = mod_sep, var = "x1",smooth = "s(x1)", step_size =  0.01)
  expect_equal(quant_mod_sep_v1$segment_summary$feature[3], "local_max")
  expect_equal(c(quant_mod_sep_v1$segment_summary$x1_start[3], quant_mod_sep_v1$segment_summary$x1_end[3]), c(48.55480, 55.73553), tolerance = 1e-2)

  quant_mod_sep_v3 <- quantify_lextrema_multivar(mod = mod_sep, var = "x3",smooth = "s(x3)", step_size =  0.01)
  expect_equal(quant_mod_sep_v3$segment_summary$feature[1], "notrend")
  expect_equal(c(quant_mod_sep_v3$segment_summary$x3_start[1], quant_mod_sep_v3$segment_summary$x3_end[1]), c(-11.1, 12.0), tolerance = 1e-2)

  #mod factor, for by =
  mod_fac <- mgcv::gam(y~s(x1, by = x2), data = data_multivar, method = "REML")

  expect_error(suppressWarnings(quantify_lextrema_multivar(mod = mod_fac, smooth = "s(x1)", step_size =0.01)))
    #var must be specified for complex models like this
  expect_error(suppressWarnings(quantify_lextrema_multivar(mod = mod_fac, smooth = "s(x1):x2A", step_size =0.01)))
  expect_error(quantify_lextrema_multivar(mod = mod_fac, smooth = "s(x1):x2A", var = "x2", step_size =0.01))

  quant_mod_fac_A <-quantify_lextrema_multivar(mod = mod_fac, smooth = "s(x1):x2A", var = "x1", step_size =0.01)
  expect_equal(quant_mod_fac_A$segment_summary$feature[3], "local_max")
  expect_equal(c(quant_mod_fac_A$segment_summary$x1_start[3], quant_mod_fac_A$segment_summary$x1_end[3]),  c(39.9, 63.5), tolerance = 1e-2)

  mod_fac_x3 <- mgcv::gam(y~s(x3, by = x2), data = data_multivar, method = "REML")
  quant_mod_fac_x3_A <-quantify_lextrema_multivar(mod = mod_fac_x3, smooth = "s(x3):x2A", var = "x3", step_size =0.01)
  expect_equal(c(quant_mod_fac_x3_A$segment_summary$x3_start[1], quant_mod_fac_x3_A$segment_summary$x3_end[1]), c(-11.1, 12.0), tolerance = 1e-2)

  #mod random effect
  mod_re <- mgcv::gam(y~s(x1)+s(x2, bs = "re") +s(x3), data=data_multivar, method = "REML")
  expect_error(suppressWarnings(quantify_lextrema_multivar(mod = mod_re, smooth = "s(x2)", step_size = 0.01)))
})

