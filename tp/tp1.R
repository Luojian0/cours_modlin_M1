##################
# Exercice 1
##################
data(anscombe)
attach(anscombe)

# create models
m1 = lm(y1~x1)
m2 = lm(y2~x2)
m3 = lm(y3~x3)
m4 = lm(y4~x4)

# model details
summary(m1)
summary(m2)
summary(m3)
summary(m4)

# plot
layout(matrix(1:4,2), respect=TRUE)
plot(x1,y1, xlim = c(3, 19), ylim = c(3, 13), main="m1")
abline(m1)
abline(a=m1$coefficients[[1]],b=m1$coefficients[[2]], lty=2, lwd=3, col=2)
plot(x2,y2, xlim = c(3, 19), ylim = c(3, 13), main="m2")
abline(m2)
plot(x3,y3, xlim = c(3, 19), ylim = c(3, 13), main="m3")
abline(m3)
plot(x4,y4, xlim = c(3, 19), ylim = c(3, 13), main="m4")
abline(m4)



##################
# Exercice 2
##################
library(MASS)
data(cats)
attach(cats)

# scatterplot
layout(1, respect=TRUE)
plot(Bwt, Hwt, xlab="Body weight", ylab="Heart weight")

# model
cats.lm = lm(Hwt~Bwt)
summary(cats.lm)

# regression line
abline(a=cats.lm$coefficients[[1]],b=cats.lm$coefficients[[2]])

# residual study
cats.lm$residuals
plot(density(cats.lm$residuals))
hist(cats.lm$residuals)
hist(cats.lm$residuals, nclass=30)

# Are residuals normal?
shapiro.test(cats.lm$residuals)

# QQplot
qqnorm(cats.lm$residuals)

# KS test
ks.test(cats.lm$residuals, rnorm(length(cats.lm$residuals), mean(cats.lm$residuals), sd(cats.lm$residuals)))


shapiro.test(resid(cats.lm))
# Shapiro-Wilk test for normality
# p-value = 0.1046 (normalite ok)

lmtest::raintest(cats.lm,order.by=~Bwt)
# Rainbow test for non linearity
# p-value = 0.1846 (linearite ok)

# Goldfeld-Quandt test against heteroskedasticity
layout(matrix(1:2,1), respect=TRUE)
nb_ind = 50
x = runif(nb_ind) * 2
y = x + rnorm(nb_ind)/10
plot(x,y)
y2 = x + rnorm(nb_ind)/10 * x
plot(x,y2)
lmtest::gqtest(lm(y~x))
lmtest::gqtest(lm(y2~x))

# Performs the Breusch-Pagan test against heteroskedasticity.
layout(matrix(1:2,1), respect=TRUE)
nb_ind = 50
x = runif(nb_ind) * 2
y = x + rnorm(nb_ind)/10
plot(x,y)
y2 = x + rnorm(nb_ind)/10 * x
plot(x,y2)
lmtest::bptest(lm(y~x))
lmtest::bptest(lm(y2~x))



lmtest::dwtest(cats.lm)
# Durbin-Watson Test for autocorrelation
# p-value = 0.002 (autocorrelation !)
 


##################
# Exercice 3
##################
data(cars)
attach(cars)

cars.lm <- lm(dist~speed)
summary(cars.lm)

par(mfrow=c(3,2))
plot(cars.lm,which=1)
plot(cars.lm,which=2)
plot(cars.lm,which=3)
plot(cars.lm,which=4)
acf(resid(cars.lm))

shapiro.test(resid(cars.lm))
# p-valeur = 0.02 (pas normalite !)
raintest(cars.lm)
# p-value = 0.7954 (linearite ok)
gqtest(cars.lm)
# p-value = 0.1498 (pas heteroscedasticite)
dwtest(cars.lm)
# p-valeur = 0.09 (pas autocorrelation)

speed2 <- speed^2
cars.lm2 <- lm(dist~speed2+0)
summary(cars.lm)

par(mfrow=c(3,2))
plot(cars.lm2,which=1)
plot(cars.lm2,which=2)
plot(cars.lm2,which=3)
plot(cars.lm2,which=4)
acf(resid(cars.lm2))

shapiro.test(resid(cars.lm2))
# p-valeur = 0.1537 (normalite ok)
raintest(cars.lm2)
# p-value = 0.818 (linearite ok)
gqtest(cars.lm2)
# p-value = 0.1608 (homoscedasticite ok)
dwtest(cars.lm2)
# p-valeur = 0.105 (independance ok)



##################
# Exercice 4
##################

library(MPV)
data(p9.10)
attach(p9.10)

g <- lm(y~x1)

par(mfrow=c(3,2))
plot(g,which=1)
plot(g,which=2)
plot(g,which=3)
plot(g,which=4)
plot(g,which=5)
acf(resid(g))

shapiro.test(resid(g))
# p-valeur = 0.8215 (normalite ok)
raintest(g)
# p-value = 0.005 (pas linearite !)
gqtest(g)
# p-value = 0.0022 (heteroscedasticite !)
dwtest(g)
# p-valeur = 0.002 (autocorrelation !)


y1 <- y[x4==-1]
x11 <- x1[x4==-1]
g1 <- lm(y~x1,subset=(x4==-1))

x11()
par(mfrow=c(3,2))
plot(g1,which=1)
plot(g1,which=2)
plot(g1,which=3)
plot(g1,which=4)
plot(g1,which=5)
acf(resid(g1))

shapiro.test(resid(g1))
# p-valeur = 0.978 (normalite ok)
raintest(g1)
# p-value = 0.3028 (linearite ok)
gqtest(g1)
# p-value = 0.2593 (homoscedasticite limite mais ok)
dwtest(g1)
# p-valeur = 0.4035 (independance limite mais ok)


y2 <- y[x4==1]
x12 <- x1[x4==1]
g2 <- lm(y2~x12)

x11()
par(mfrow=c(3,2))
plot(g2,which=1)
plot(g2,which=2)
plot(g2,which=3)
plot(g2,which=4)
plot(g2,which=5)
acf(resid(g2))

shapiro.test(resid(g2))
# p-valeur = 0.52 (normalite ok)
raintest(g2)
# p-value = 0.3722 (linearite ok)
gqtest(g2)
# p-value = 0.08 (homoscedasticite limite mais ok)
dwtest(g2)
# p-valeur = 0.06 (independance limite mais ok)





###################################################################################
###################################################################################

###################
#  Exercice 
###################

library(faraway)

data(corrosion)
attach(corrosion)
plot(Fe,loss)
g <- lm(loss~Fe)
summary(g)

par(mfrow=c(3,2))
plot(g,which=1)
plot(g,which=2)
plot(g,which=3)
plot(g,which=4)
plot(g,which=5)
acf(resid(g))

shapiro.test(resid(g))
#W = 0.93304, p-value = 0.3733
dwtest(g)
#DW = 2.5348, p-value = 0.8524
raintest(g)
#Rain = 2.2514, df1 = 7, df2 = 4, p-value = 0.2259
gqtest(g)
#GQ = 0.84669, df1 = 5, df2 = 4, p-value = 0.5802





##################
# Exercice
##################
data(airquality)
attach(airquality)

air.Wind <- lm(Ozone~Wind)
summary(air.Wind)

par(mfrow=c(3,2))
plot(air.Wind,which=1)
plot(air.Wind,which=2)
plot(air.Wind,which=3)
plot(air.Wind,which=4)
acf(resid(air.Wind),lag.max=10)

shapiro.test(resid(air.Wind))
# p-valeur = 0.006 (pas normalite !)
raintest(air.Wind)
# p-value = 0.7402 (linearite ok)
gqtest(air.Wind)
# p-value = 0.601 (pas heteroscedasticite)
dwtest(air.Wind)
# p-valeur = 0.01569 (autocorrelation !)


air.Temp <- lm(Ozone~Temp)
summary(air.Temp)

par(mfrow=c(3,2))
plot(air.Temp,which=1)
plot(air.Temp,which=2)
plot(air.Temp,which=3)
plot(air.Temp,which=4)
plot(air.Temp,which=56)
acf(resid(air.Temp))

shapiro.test(resid(air.Temp))
# p-valeur = 1e-07 (pas normalite !)
raintest(air.Wind)
# p-value = 0.7402 (linearite ok)
gqtest(air.Temp)
# p-value = 0.3223 (pas heteroscedasticite)
dwtest(air.Wind)
# p-valeur = 0.01569 (autocorrelation !)

x11()

durbinWatsonTest(air.Temp,maxlag=5)
# p-valeur = 0.338 (pas autocorrelation)


Wind1<-1/Wind
air.lm <- lm(Ozone~Wind1+Temp+Solar.R, subset=c(1:153)[-c(30,117)])#,99,117)])
summary(air.lm)

par(mfrow=c(3,2))
plot(air.lm,which=1)
plot(air.lm,which=2)
plot(air.lm,which=3)
plot(air.lm,which=4)
plot(air.lm,which=5)
acf(resid(air.lm))

shapiro.test(resid(air.lm))
# p-valeur = 1e-05 (pas normalite !)
raintest(air.lm)
# p-value = 0.7402 (linearite ok)
gqtest(air.lm)
# p-value = 0.4361 (homoscedasticite ok)
dwtest(air.lm)
# p-valeur = 0.502 (independance ok)



################################################

 plot(women, xlab = "Height (in)", ylab = "Weight (lb)", main = "women data: American women aged 30-39")

#################################################

library(faraway)
data(star)


