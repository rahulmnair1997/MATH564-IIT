setwd("/Users/rahulnair/desktop")
#read data
data_1 <- read.table('Copier.txt', header = FALSE)
data_2 <- read.table('Model.txt', header = FALSE)
x1 <- data_1$V2
x1
x2 <- data_2$V1
x2
y <- data_1$V1

# (1)
RModel <- lm(y~x1+x2)
summary(RModel)

# (2)
confint(RModel, level=0.95)

# (3)
x <- x1*x2
#plot(x, resid(RModel))

# (4)
RModel1 <- lm(y~x1+x2+x)
summary(RModel1)
#plot(x, resid(RModel1))
#abline(RModel, col = 'red')
#abline(RModel1, col = 'blue')
#plot(RModel)
coplot(y~x1|x2, panel = panel.smooth)

# (5)
anova(RModel1)