plot(mpg.data[,1],mpg.data[,2], main="Scatterplot of Fuel Effiency vs Speed",xlab="Speed (in miles per hour)",ylab="Fuel Efficiency (in miles per gallon)")
var(mpg.data[,2])
var(log(mpg.data[,2]))
plot(mpg.data[,1],log(mpg.data[,2]), main="Scatterplot of Transformed MPG vs Speed",xlab="Speed (in miles per hour)",ylab="Transformed log(mpg)")
MPG= mpg.data[,2]
Speed = mpg.data[,1]

mod2 = lm(log(MPG) ~ Speed + I(Speed^2) )
mod3 = lm(log(MPG) ~ Speed + I(Speed^2) + I(Speed^3) )
mod4 = lm(log(MPG) ~ Speed + I(Speed^2) + I(Speed^3) + I(Speed^4) )
mod5 = lm(log(MPG) ~ Speed + I(Speed^2) + I(Speed^3) + I(Speed^4) + I(Speed^5) )
summ2 = summary(mod2)
summ3 = summary(mod3)
summ4 = summary(mod4)
summ5 = summary(mod5)

CI4 = predict(mod4, interval = "confidence", level = 0.9)
CI4 = cbind(Speed, CI4)
CI4 = CI4[order(CI4[,1]),]
plot(mpg.data[,1],mpg.data[,2], main="Scatterplot of Fuel Effiency(MPG) vs Speed",xlab="Speed (in miles per hour)",ylab="Fuel Efficiency (in MPG)")
points(CI4[,1], exp(CI4[,2]), type = "l", col = 4)


fitted = exp(predict(mod4))
resid = MPG - fitted
mean = mean(MPG)
TSS = sum((MPG-mean)^2)
RSS = sum(resid^2)
ESS = TSS-RSS
Rsq = ESS/TSS
Rsq

maxMPG = max(exp(CI4[,2]))
maxMPG

speeds <- seq(0, 120, by = 0.1)
log_mpg <- predict(mod4, newdata = data.frame(Speed_.mph. = speeds))
mpg <- exp(log_mpg)

s = CI4[,1]
# Find speed with maximum MPG
max.mpg <- which.max(mpg)
speed_max <- Speed[max.mpg]
mpg.max <- mpg[max.mpg]

CI4 = predict(mod4, interval = "confidence", level = 0.9)
CI4 = cbind(Speed, CI4)
CI4 = CI4[order(CI4[,1]),]
plot(mpg.data[,1],mpg.data[,2], main="Scatterplot of Fuel Effiency(MPG) vs Speed with 90% confidence bands",xlab="Speed (in miles per hour)",ylab="Fuel Efficiency (in MPG)")
points(CI4[,1], exp(CI4[,2]), type = "l", col = 4)
points(CI4[,1], exp(CI4[,3]), type = "l", lty = 2, col = 3)
points(CI4[,1], exp(CI4[,4]), type = "l", lty = 2, col = 3)
