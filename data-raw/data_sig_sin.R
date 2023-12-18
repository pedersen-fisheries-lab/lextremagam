## code to prepare `data_sig_sin` dataset goes here
data_sig_sin <- data.frame(x=seq(-10, 15, .25))
data_sig_sin$y_true <- 10/(1+.9*exp(-.5*data_sig_sin$x))+cos(data_sig_sin$x)
data_sig_sin$y <- data_sig_sin$y_true+rnorm(nrow(data_sig_sin), 0, .5)

usethis::use_data(data_sig_sin, overwrite = TRUE)

## code to prepare `data_compr` dataset goes here
data_compr <- data.frame(x=seq(1, 237, by=1))
data_compr$y_true <- c(rep(1, times=20), 2:20, rep(20, times=20), 20:10, rep(10, times=20),
                       10:30, rep(30, times=20),31:45, rep(45, times=20),45:30,rep(30, times=20),
                       29:15,rep(15, times=20))
data_compr$y <- data_compr$y_true+rnorm(237, 0, 1.5)

usethis::use_data(data_compr)
