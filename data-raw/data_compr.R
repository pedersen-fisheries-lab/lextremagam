## code to prepare `data_compr` dataset goes here
set.seed(100)
data_compr <- data.frame(x=seq(1, 234, by=1))
data_compr$y_true <- c(rep(1, times=20), 2:20, seq(20, 21.8, by = 0.2), seq(21.6, 20, by = -0.2), 20:10, seq(10, 8.2, by = -0.2), seq(8.4, 10, by = 0.2),
                       10:30, rep(30, times=20),31:45, seq(45,46.8, by=0.2) , seq(46.6, 45, by = -0.2),45:30,rep(30, times=20),
                       29:15,rep(15, times=20))
data_compr$y <- data_compr$y_true+rnorm(234, 0, 1.5)

usethis::use_data(data_compr, overwrite = TRUE)
