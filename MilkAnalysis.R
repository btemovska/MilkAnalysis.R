
# Milk analysis
#load the Milk.RData

#produce scatterplot
plot(production, protein, xlab="production (kg/day)", ylab="protein (kg/day)")

#calc the correlation coefficient
cor(production, protein) #0.9777319

#fit a linear regression model
milk.model<-lm(protein~production) #note y variable goes first, than x variable
#summarize the model
summary(milk.model)

coef(milk.model) #B0 intercept and B1slope

#calc the fitted and resid values of the linear regression  model
fits<-fitted(milk.model)
resids<-resid(milk.model)

#produce plots 2x2, reduce font size 0.75, reduce margins
par(mfrow=c(2,2), cex=0.75, mar=c(4,4,0.5,0.5))
plot(production, protein, xlab="production (kg/day)", ylab="protein (kg/day)")
abline(milk.model, lty=2)  #fitted line with dashed 2
#produce plot of residuals vs production
plot(production, resids, xlab="production (kg/day)", ylab="Residuals")
abline(h=0, lty=2)
#produce plot of residuals vs fitted values
plot(fits, resids, xlab="Fitted values", ylab="Residuals")
abline(h=0, lty=2)

#build 95% confidence interval for the slope parameter
n<-length(protein)
qt(0.975, n-2)
t.quantile=2.178813
B1slope=0.024576
B1standarderror=0.001523
lowerBound<-B1slope-t.quantile*B1standarderror
upperBound<-B1slope+t.quantile*B1standarderror
#95%CI (0.02125767, 0.02789433)

#estimate the mean of milk protein level for a milk production level 
#at 50kg/day & 30kg/day
milk.model<-lm(protein~production)
plot(production, protein, xlim=c(18,55), ylim=c(0.5,1.5))
abline(v=c(30,50), col="purple", lwd=2, lty=2)
#note the milk production values ranges from 20kg/day to around 42kg/day
#the 30kg/day is an interpolation (inside our data)
#the 50kg/day is an extrapolation (outside out data)

#R2 tells us there is 95.6% variation in milk protein

#estimate the mean milk of 30kg/day production & calc 95% confidence interval
predict(milk.model, data.frame(production=30), se.fit=TRUE, interval="confidence")
#estimated mean is 0.91 kg/day when the milk production is 30kg/day
#95% CI interval are 0.0888, 0.9374
#or
predict(milk.model, data.frame(production=30), se.fit=TRUE, interval="confidence", level=0.95)

#predict the mean milk of 30kg/day production & calc 95% prediction interval
predict(milk.model, data.frame(production=30), se.fit=TRUE, interval="prediction")
#estimated mean stays the same at 0.91 kg/day
#95% PI interval are 0.818, 1.001 (prediction is wider)
