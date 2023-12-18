
test_that("quantify_lextrema works with defaults", {
  data(data_sig_sin)

  mod_sig_sin <- mgcv::gam(y~s(x), data=data_sig_sin)

  expect_warning(object = quantify_lextrema(mod = mod_sig_sin),
                 "step_size not defined. step_size will be defined as the average step size/1000")

  expect_warning(object = quantify_lextrema(mod = mod_sig_sin),
                 "No specific predictor variable was set. The first predictor was extracted")

  expect_no_error(object = quantify_lextrema(mod = mod_sig_sin))

  expect_type(object = quantify_lextrema(mod = mod_sig_sin),
              type = "list")
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
