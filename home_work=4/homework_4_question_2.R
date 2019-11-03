# @author - rahul_nair
setwd("/Users/rahulnair/desktop")
data_1 <- read.csv('cement.csv', header = TRUE)
y <- data_1$y
x1 <- data_1$x1
x2 <- data_1$x2
x3 <- data_1$x3
x4 <- data_1$x4
mod <- lm(y~x1+x2+x3+x4)

# intercept only model
model <- lm(y~1)


add1(model, ~.+x1+x2+x3+x4, test = "F")

# adding x4
model <- update(model, .~.+x4)
summary(model)

add1(model, ~.+x1+x2+x3, test = "F")

# adding x1
model <- update(model,.~.+x1)
summary(model)

add1(model, ~.+x2+x3, test = "F")

# adding x2
model <- update(model, .~.+x2)
summary(model)

# removing x4
model <- update(model, .~.-x4)
summary(model)
add1(model, ~.+x3+x4, test = "F")
# Thus our final model will have x1 and x2.