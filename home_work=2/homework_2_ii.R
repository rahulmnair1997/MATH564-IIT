DST=read.table("APPENC07.txt",fill = FALSE, header = TRUE) 
X=DST$sqft
Y=DST$Price
z=sample(1:522,100)
x=X[z]
y=Y[z]
fit=lm(formula = y~x)
plot(x,y,main="Sales Price v/s Finished Square Feet")
abline(fit,col="red")
conf_interval = predict(fit,newdata = data.frame(x), interval="confidence",
                        level = 0.95)
lines(x, conf_interval[,2], col="blue", lty=2)
lines(x, conf_interval[,3], col="blue", lty=2)
pred_interval = predict(fit,newdata = data.frame(x), interval="prediction",
                        level = 0.95)
lines(x, pred_interval[,2], col="orange", lty=2)
lines(x, pred_interval[,3], col="orange", lty=2)
legend(1000,8e+05, legend=c("Regression", "95% CI", "95% PI"),
       col=c("red", "blue", "orange"), lty=1:2, cex=0.8)
x2=sum((x-mean(x))^2)
y2=sum((x-mean(x))*(y-mean(y)))
b1=y2/x2
b0=mean(y)-b1*mean(x)
err=y-b0-b1*x
sse=sum(err^2)
mse=sse/98
t=qt(0.975,98)
se_conf=sqrt(mse/100)
y_hat=b0+b1*mean(x)
y_confUpper=y_hat+t*se_conf
y_confLower=y_hat-t*se_conf
se_pred=sqrt(mse*(1+1/100))
y_predUpper_formula=y_hat+t*se_pred
y_predLower_formula=y_hat-t*se_pred
conf_interval_y <- predict(fit, newdata=data.frame(x), interval="confidence",level = 0.95)
pred_interval_y <- predict(fit, newdata=data.frame(x), interval="prediction",level = 0.95)
y_confUpperR=mean(conf_interval_y[,3])
y_confLowerR=mean(conf_interval_y[,2])
y_predUpper_formulaR=mean(pred_interval_y[,3])
y_predLower_formulaR=mean(pred_interval_y[,2])