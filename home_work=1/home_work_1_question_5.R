#library("data.table")
setwd("/Users/rahulnair/desktop")
data <- read.delim('skin cancer2.txt', header = TRUE, sep = "|")
data$Lat
x <- data$Lat
y <- data$Mort
# Calculating the estimates
sum_x_sqr = sum((x - mean(x))^2)
sum_xy = sum((y - mean(y)) * (x - mean(x)))
# Calculating b1
b1 <- sum_xy/sum_x_sqr
b0 <- (mean(y)) - (b1*(mean(x)))
b1
# Calculating e
e <- y - b0 - (b1*x)
sum(e)
# Calculating r
sst <- sum((y - mean(y))^2)
sst
sse <- sum((e^2))
sse
ssr <- sum(((b0+b1*x) - mean(y))^2)
ssr
# confidence interval
b1_lower <- b1 - ((qt((0.05)/2,(46)))*abs(sqrt(sse/46)/sqrt(sum((x-mean(x))^2))))
b1_lower
b1_upper <- b1+  ((qt((0.05)/2,(46)))*abs(sqrt(sse/46)/sqrt(sum((x-mean(x))^2))))
b1_upper
# with functions
reg<-lm(y~x)
summary(reg)
confint(reg,level=0.95)
plot(x,y,pch=16,cex=1.3, col="blue", main="Mortality vs Latitude", xlab="Latitude", ylab="Mortality")
abline(lm(y~x))
