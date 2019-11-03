# @author - rahul_nair
# QUESTION 1
setwd("/Users/rahulnair/desktop")
data <- read.csv('CDI_1.csv', header = TRUE)
x1 <- data$variable_6
x2 <- data$variable_8
x3 <- data$variable_9
x4 <- data$variable_13
x5 <- data$variable_14
x6 <- data$variable_15
y <- data$variable_10
# data_2 <- data[,c('variable_6','variable_8','variable_9','variable_13','variable_14','variable_15')]
# PART 1
# install.packages("car")
library("car")
vif(lm(y~x1+x2+x3+x4+x5+x6), data=Duncan)
# 2 variables, x2 and x3 have serious multi-collinearity issues at present.

########
# PART 2
#install.packages("MASS")
library("MASS")
deleted_residuals <-rstudent(lm(y~x1+x2+x3+x4+x5+x6))
mod <- lm(y~x1+x2+x3+x4+x5+x6)
ols_plot_resid_stud_fit(mod)
# a graph has been plotted. There are 9 outliers.

########
# PART 3
diag = hatvalues(mod, type = "diagonal")
diag
extreme <- c()
for (i in 1:length(diag)){
  if(diag[i]>3*mean(diag)){
    extreme <- c(extreme, diag[i])
  }
}
extreme

#######
# PART 4(I)
# We check the threshold for dfBetas (by ols_plot_dfbetas(mod), which is 0.1 and based on that we get influence values  which is 2, 6.
print("dfbetas")
dfbetas(mod)[c(2, 6, 8, 48, 128, 206, 404),]
ols_plot_dfbetas(mod)


########
#PART 4(II)
# We check the threshold for dffits (by ols_plot_dffits(mod), which is 0.25 and based on that we get influence values  which is 6. 
print("dffits")
dffits(mod)[c(2, 6, 8, 48, 128, 206, 404)]
ols_plot_dffits(mod)

########
#PART 4(III)
# We know the threshold which is p = 0.5 and based on that we get influence values  which is 6. 
print("cook's distance")
cook = cooks.distance(mod)[c(2,6,  8, 48, 128, 206, 404)]
cook
pf(cook,7,433)

