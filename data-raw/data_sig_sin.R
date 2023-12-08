## code to prepare `data_sig_sin` dataset goes here
data_sig_sin <- data.frame(x=seq(-10, 15, .25))
data_sig_sin$y_true <- 10/(1+.9*exp(-.5*data_sig_sin$x))+cos(data_sig_sin$x)
data_sig_sin$y <- data_sig_sin$y_true+rnorm(nrow(data_sig_sin), 0, .5)

usethis::use_data(data_sig_sin, overwrite = TRUE)
