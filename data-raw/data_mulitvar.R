## code to prepare `data_multivar` dataset goes here
set.seed(1)
data_multivar <- data.frame(x1=c(1:100),
                            x2 = as.factor(rep(c("A", "B"),times = 50)),
                            x3 = rnorm(n=100, mean=0, sd = 5))

data_multivar$y_true <- dnorm(x = data_multivar$x1, mean = 50, sd = 15)*100
data_multivar$y_true[data_multivar$x2 == "B"] <- data_multivar$y_true[data_multivar$x2 == "B"]+2
data_multivar$y <- data_multivar$y_true +rnorm(n=100, mean = 0, sd = .5)

usethis::use_data(data_multivar, overwrite = TRUE)


