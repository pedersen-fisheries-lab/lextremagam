## code to prepare `data_norm` dataset goes here
set.seed(100)
data_norm <- data.frame(x=seq(1, 200, by=1))
data_norm$y_true <- dnorm(data_norm$x, mean = 100, sd = 40)*100
data_norm$y <- data_norm$y_true+rnorm(200, 0, .5)

usethis::use_data(data_norm, overwrite = TRUE)
