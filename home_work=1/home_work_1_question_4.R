x <- runif(40,-1,1)
x
y <- 2*x + rnorm(40,0,0.1)
y
# For regression passing through origin
# Summation of x^2
sum_x_sqr <- sum((x)^2)
# Summation of x*y
sum_xy <- sum((x)*(y))
# Calculating b0
b0 <- 0
# Calculating b1
b1 <- sum_xy/sum_x_sqr
# Calculating e_2
e_1 <- y - b1*x
sum_of_e1 = sum(e_1)
sum_of_e1
# Calculating r^2
sst <- sum((y - mean(y))^2)
sse <- sum((e_1)^2)
r_1 <- 1 - (sse/sst)
r_1
# For ordinary linear regression
# Summation of x^2
sum_x_sqr <- sum((x - mean(x))^2)
# Summation of x*y
sum_xy <- sum((x - mean(x))*(y - mean(y)))
# Calculating b1
b1 <- sum_xy/sum_x_sqr
# Calculating b0
b0 <- (mean(y)) - (b1*mean(x))
# Calcualting e_2
e_2 <- y - b0 - b1*x
sum_of_e2 = sum(e_2)
sum_of_e2
# Calculating r^2
sst <- sum((y - mean(y))^2)
sse <- sum((e_2)^2)
r_2 <- 1 - (sse/sst)
r_2
