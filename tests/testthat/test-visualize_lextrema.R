
test_that("inappropriate arguments return correct response ",{
  data(data_sig_sin)
  mod_sig_sin <- mgcv::gam(y~s(x), data=data_sig_sin)
  quant_seg_sig_sin <- quantify_lextrema(mod = mod_sig_sin, step_size = 0.001, var = "x")

  #inappropriate lextr
  expect_error(object = plot_lextrema(lextr = mod_sig_sin))
  expect_error(object = plot_lextrema(lextr = quant_seg_sig_sin[[1]]))

  #inappropriate step_size
  expect_error(object = plot_lextrema(lextr = quant_seg_sig_sin, plot_deriv = "1"))
  expect_error(object = plot_lextrema(lextr = quant_seg_sig_sin, plot_deriv = NA))
  expect_error(object = plot_lextrema(lextr = quant_seg_sig_sin, plot_deriv = NULL))
  expect_error(object = plot_lextrema(lextr = quant_seg_sig_sin, plot_deriv = 1))

  #inappropriate show_segs
  expect_error(object = plot_lextrema(lextr = quant_seg_sig_sin, plot_deriv = FALSE, show_segs = c(increase)))
  expect_error(object = plot_lextrema(lextr = quant_seg_sig_sin, plot_deriv = FALSE, show_segs = c("increases")))
  expect_error(object = plot_lextrema(lextr = quant_seg_sig_sin, plot_deriv = FALSE, show_segs = NA))
})

test_that("plot_lextrema works well with defaults",{
  data(data_sig_sin)
  mod_sig_sin <- mgcv::gam(y~s(x), data=data_sig_sin)
  quant_seg_sig_sin <- quantify_lextrema(mod = mod_sig_sin, step_size = 0.001, var = "x")

  #inappropriate lextr
  expect_no_error(object = plot_lextrema(lextr = quant_seg_sig_sin))
  expect_type(object = plot_lextrema(lextr = quant_seg_sig_sin), type = "list")
})

test_that("plot_lextrema works with univariate model", {
  data(data_sig_sin)
  mod_sig_sin <- mgcv::gam(y~s(x), data=data_sig_sin)
  quant_sig_sin <- quantify_lextrema(mod = mod_sig_sin, var = "x", step_size = 0.0025)

  #catches non -unlextrema objects
  expect_error(plot_lextrema("a"), "lextr must be an object of class lextrema produced by the quantify_lextrema function")

  #default works correctly
  expect_length(plot_lextrema(quant_sig_sin),2)

  #other settings work properly
  expect_length(plot_lextrema(quant_sig_sin, plot_deriv = TRUE), 2)
  expect_length(plot_lextrema(quant_sig_sin, plot_deriv = FALSE), 11)

  # #example data
  # set.seed(1)
  # x <- seq(0, 1, 0.05)
  # y <- dnorm(x = x, mean = 0.5, sd = 0.3) + rnorm(n = 21, mean = 0, sd = 0.2)
  # y_beta <- y/(max(y)+0.001)
  # mod_beta <- mgcv::gam(y_beta~s(x), method="REML", family = "betar")
  # lextrema_beta <- quantify_lextrema(mod = mod_beta,
  #                                    var = "x",
  #                                    step_size = 0.001,
  #                                    conf_level = 0.95,
  #                                    deriv_method = "gratia")
  # plot_lextrema(lextr = lextrema_beta, show_segs = "all", type="link")
})

test_that("plot_lextrema works with mutlivariate model", {



})

