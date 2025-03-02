
test_that("inappropriate arguments return correct response ",{
  data(data_sig_sin)
  mod_sig_sin <- mgcv::gam(y~s(x), data=data_sig_sin)
  quant_seg_sig_sin <- quantify_lextrema(mod = mod_sig_sin, step_size = 0.001, var = "x")

  #inappropriate quant_segments
  expect_error(object = plot_lextrema(quant_segments = mod_sig_sin))
  expect_error(object = plot_lextrema(quant_segments = quant_seg_sig_sin[[1]]))

  #inappropriate step_size
  expect_error(object = plot_lextrema(quant_segments = quant_seg_sig_sin, plot_deriv = "1"))
  expect_error(object = plot_lextrema(quant_segments = quant_seg_sig_sin, plot_deriv = NA))
  expect_error(object = plot_lextrema(quant_segments = quant_seg_sig_sin, plot_deriv = NULL))
  expect_error(object = plot_lextrema(quant_segments = quant_seg_sig_sin, plot_deriv = 1))

  #inappropriate show_segs
  expect_error(object = plot_lextrema(quant_segments = quant_seg_sig_sin, plot_deriv = FALSE, show_segs = c(increase)))
  expect_error(object = plot_lextrema(quant_segments = quant_seg_sig_sin, plot_deriv = FALSE, show_segs = c("increases")))
  expect_error(object = plot_lextrema(quant_segments = quant_seg_sig_sin, plot_deriv = FALSE, show_segs = NA))
})

test_that("plot_lextrema works well with defaults",{
  data(data_sig_sin)
  mod_sig_sin <- mgcv::gam(y~s(x), data=data_sig_sin)
  quant_seg_sig_sin <- quantify_lextrema(mod = mod_sig_sin, step_size = 0.001, var = "x")

  #inappropriate quant_segments
  expect_no_error(object = plot_lextrema(quant_segments = quant_seg_sig_sin))
  expect_type(object = plot_lextrema(quant_segments = quant_seg_sig_sin), type = "list")
})


