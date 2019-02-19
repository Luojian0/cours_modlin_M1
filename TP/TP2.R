library(lmtest)
library(car)
library(ppcor)

############################
# Exercice 1
############################

data(LifeCycleSavings)
str(LifeCycleSavings)
sr.lm.tot <- lm(sr~.,data=LifeCycleSavings)

layout(matrix(1:4, 2, byrow=TRUE), respect=TRUE)
plot(sr ~ pop75,data=LifeCycleSavings,xlab="pop75",ylab="sr",
     main="% Population over 75 vs sr",pch=19,cex=1.25)
plot(sr ~ pop15,data=LifeCycleSavings,xlab="pop15",ylab="sr",
     main="% Population under 15 vs sr",pch=19,cex=1.25)
plot(sr ~ dpi,data=LifeCycleSavings,xlab="dpi",ylab="sr",
     main="% dpi vs sr",pch=19,cex=1.25)
plot(sr ~ ddpi,data=LifeCycleSavings,xlab="ddpi",ylab="sr",
     main="% ddpi vs sr",pch=19,cex=1.25)

drop1(sr.lm.tot)
sr.lm.temp <- lm(sr~.-dpi,data=LifeCycleSavings)
drop1(sr.lm.temp)

sr.lm <- step(lm(sr~.,data=LifeCycleSavings),method="backward")
sr.lm$anova

sr.lm.bis <- lm(sr~pop15,data=LifeCycleSavings)
anova(sr.lm.tot,sr.lm)
anova(sr.lm.bis,sr.lm)


summary(sr.lm)


par(mfrow=c(3,2))
plot(sr.lm,which=1)
plot(sr.lm,which=2)
plot(sr.lm,which=3)
plot(sr.lm,which=4)
plot(sr.lm,which=5)
acf(resid(sr.lm),ci=0.95)

shapiro.test(resid(sr.lm))
# Shapiro-Wilk test for normality
# p-value = 0.8764 (normalite ok)
raintest(sr.lm)
# Rainbow test for non linearity
# p-value = 0.5122 (linearite ok)
gqtest(sr.lm)
# Golfeld-Quant Test for heteroscedasticity
# p-value = 0.3094 (homoscedasticite ok)
dwtest(sr.lm)
# Durbin-Watson Test for autocorrelation
# p-value = 0.375 (independance ok)

vif(sr.lm)
#   pop15    pop75     ddpi 
#5.745478 5.736014 1.004186 
# inferieures à 10 donc pas de colinearite


layout(matrix(1:2, 1, byrow=TRUE), respect=TRUE)
plot(sr ~ pop75,data=LifeCycleSavings,xlab="pop75",ylab="sr",
     main="% Population over 75 vs sr",pch=19,cex=1.25)
pop75.lm = lm(sr ~ pop75,data=LifeCycleSavings)
abline(coef(pop75.lm),col="red")
plot(sr ~ pop15,data=LifeCycleSavings,xlab="pop15",ylab="sr",
     main="% Population under 15 vs sr",pch=19,cex=1.25)
pop15.lm = lm(sr ~ pop15,data=LifeCycleSavings)
abline(coef(pop15.lm),col="red")



############################
# Exercice 2
############################

data(longley)
str(longley)
attach(longley)


modele <- lm(Employed~1,data=longley)
add1(modele,~GNP+GNP.deflator+Unemployed+Year+Population+Armed.Forces,data=longley)
modele<-update(modele,~.+GNP,data=longley)
add1(modele,~GNP+GNP.deflator+Unemployed+Year+Population+Armed.Forces,data=longley)
modele<-update(modele,~.+Unemployed,data=longley)
add1(modele,~GNP+GNP.deflator+Unemployed+Year+Population+Armed.Forces,data=longley)
modele<-update(modele,~.+Armed.Forces,data=longley)
add1(modele,~GNP+GNP.deflator+Unemployed+Year+Population+Armed.Forces,data=longley)
modele<-update(modele,~.+Year,data=longley)


long.lm <- step(lm(Employed~.,data=longley),method="backward")
long.lm$anova
summary(long.lm)

par(mfrow=c(3,2))
plot(long.lm,which=1)
plot(long.lm,which=2)
plot(long.lm,which=3)
plot(long.lm,which=4)
plot(long.lm,which=5)
acf(resid(long.lm),ci=0.95)

shapiro.test(resid(long.lm))

# Shapiro-Wilk test for normality
# p-value = 0.6005 (normalite ok)
raintest(long.lm)
# Rainbow test for non linearity
# p-value = 0.5122 (linearite ok)
gqtest(long.lm)
# Golfeld-Quant Test for heteroscedasticity
# p-value = 0.3094 (homoscedasticite ok)
dwtest(long.lm)
# Durbin-Watson Test for autocorrelation
# p-value = 0.375 (independance ok)

vif(long.lm)
#         GNP   Unemployed Armed.Forces         Year 
#  515.123851    14.108642     3.141581   638.128041 
# colinearite !

x<longley[-Employed]
cor(x)
solve(t(x)%*%x)
solve(t(x)%*%x,t(x)%*%Employed)
lm(Employed~.,data=longley)

x <- as.matrix(cbind(GNP, Unemployed, Armed.Forces, Year))
cor(x)
solve(t(x)%*%x)
solve(t(x)%*%x,t(x)%*%Employed)

summary(lm(Employed~GNP + Unemployed + Armed.Forces ,data=longley))



#####################################
# Exercice  4
#####################################

data(Prestige)
str(Prestige)
Prestige.wc<-subset(Prestige,type=="wc")
Prestige2<-data.frame(education=Prestige.wc$education,income=Prestige.wc$income,women=Prestige.wc$women,prestige=Prestige.wc$prestige)
attach(Prestige2)

pairs(Prestige2)


#correlation
cor(as.matrix(Prestige2))
pcor(as.matrix(Prestige2))$estimate

#modele avec constante
pr.lm1 <- step(lm(prestige~education+income+women),method="backward")
pr.lm1$anova
summary(pr.lm)

#modele sans constante
pr.lm.temp<-lm(prestige~0)
add1(pr.lm.temp,scope=~0+education+income+women,data=Prestige2)
pr.lm.temp<-lm(prestige~0+education)
add1(pr.lm.temp,scope=~0+education+income+women,data=Prestige2)
pr.lm.temp<-lm(prestige~0+education+income)
add1(pr.lm.temp,scope=~0+education+income+women,data=Prestige2)
pr.lm0<-lm(prestige~0+education+income+women)


pr.lm <- step(lm(prestige~0),~education+income+women,method="both")
pr.lm$anova
summary(pr.lm)

#comparaison des modeles
extractAIC(pr.lm1)
extractAIC(pr.lm0)
extractAIC(pr.lm)
anova(pr.lm,pr.lm0)
anova(pr.lm,pr.lm1)


# graphique
points3D(income,women,prestige2,phi=20,theta=50)
library(rgl)
scatter3d(prestige ~ income + education, data=Prestige2, radius=rep(0, 45), residuals=TRUE)


par(mfrow=c(3,2))
plot(pr.lm,which=1)
plot(pr.lm,which=2)
plot(pr.lm,which=3)
plot(pr.lm,which=4)
plot(pr.lm,which=5)
acf(resid(pr.lm),ci=0.95)

shapiro.test(resid(pr.lm))
# Shapiro-Wilk test for normality
# p-value = 0.9371 (normalite ok)
raintest(pr.lm)
# Rainbow test for non linearity
# p-value = 0.7869 (linearite ok)
gqtest(pr.lm)
# Golfeld-Quant Test for heteroscedasticity
# p-value = 0.9222 (homoscedasticite ok)
dwtest(pr.lm)
# Durbin-Watson Test for autocorrelation
# p-value = 0.03745 (corerelation !)

vif(pr.lm)
# ne va pas car pas de constante

predict(pr.lm,data.frame(income=42210,women=37),intervall="confidence")

Prestige['ministers',]
Prestige['economists',]


############################
# Exercice 5
############################

data(airquality)
str(airquality)

cor(as.matrix(airquality),use="complete.obs")
# pcor : missing values are note allowed
air.lm.tot <- lm(Ozone~Wind+Temp+Solar.R+Month+Day,data=airquality)

air.lm <- step(lm(Ozone~Wind+Temp+Solar.R,data=airquality))#,subset=c(1:153)[-117]),method="backward")
air.lm$anova
summary(air.lm)

par(mfrow=c(3,2))
plot(air.lm,which=1)
plot(air.lm,which=2)
plot(air.lm,which=3)
plot(air.lm,which=4)
plot(air.lm,which=5)
acf(resid(air.lm),ci=0.95)

shapiro.test(resid(air.lm))
# Shapiro-Wilk test for normality
# p-value = 0.0006 (pas normalite !)
raintest(air.lm)
# Rainbow test for non linearity
# p-value = 0.3517 (linearite ok)
gqtest(air.lm)
# Golfeld-Quant Test for heteroscedasticity
# p-value = 0.9533 (homoscedasticite ok)
dwtest(air.lm)
# Durbin-Watson Test for autocorrelation
# p-value = 0.36 (independance ok)

vif(air.lm)
#    Wind     Temp  Solar.R 
#1.333987 1.438366 1.094996 
# inferieures à 10 donc pas de colinearite



attach(airquality)
new.ozone=log(Ozone)
air.lm <- step(lm(new.ozone~Wind+Temp+Solar.R,data=airquality))#,subset=c(1:153)[-117]),method="backward")
air.lm$anova
summary(air.lm)

par(mfrow=c(3,2))
plot(air.lm,which=1)
plot(air.lm,which=2)
plot(air.lm,which=3)
plot(air.lm,which=4)
plot(air.lm,which=5)
acf(resid(air.lm),ci=0.95)

shapiro.test(resid(air.lm))
# Shapiro-Wilk test for normality
# p-value = 0.0006 (pas normalite !)
raintest(air.lm)
# Rainbow test for non linearity
# p-value = 0.3517 (linearite ok)
gqtest(air.lm)
# Golfeld-Quant Test for heteroscedasticity
# p-value = 0.9533 (homoscedasticite ok)
dwtest(air.lm)
# Durbin-Watson Test for autocorrelation
# p-value = 0.36 (independance ok)

vif(air.lm)




############################
# Exercice 6
############################

base = read.table("heartdisease.txt", header=TRUE)

names(base)

summary(base)

attach(base)
shapiro.test(sbp)
hist(sbp)
qqnorm((sbp-mean(sbp))/sd(sbp))
abline(0,1)
t.test(sbp~famhist)


hist(sbp)
sqrt.sbp <- log(sbp)
hist(sqrt(sbp))

#sqrt.sbp <- sbp

res = lm(sqrt.sbp~tobacco+ldl+adip+obesity+alchohol+age,data=base)
g <- step(res,direction="both")
par(mfrow=c(2,3))
plot(g)
plot(g,which=4)
acf(resid(g))

shapiro.test(resid(g)) #non
raintest(g) #ok
gqtest(g) #ok
dwtest(g) #ok

res1 = lm(sqrt.sbp~tobacco+ldl+adip+obesity+alchohol+age,data=base,subset=(famhist=="Absent"))
g1 <- step(res1,direction="both")
x11()
par(mfrow=c(2,3))
plot(g1)
plot(g1,which=4)
acf(resid(g1))

shapiro.test(resid(g1)) #non
raintest(g1) #ok
gqtest(g1) #ok
dwtest(g1) #ok


res2 = lm(sbp~tobacco+ldl+adip+obesity+alchohol+age,data=base,subset=(famhist=="Present"))
g2 <- step(res2,direction="both")
x11()
par(mfrow=c(2,3))
plot(g2)
plot(g2,which=4)
acf(resid(g2))

shapiro.test(resid(g2)) #non
raintest(g2) #ok
gqtest(g2) #ok
dwtest(g2) #ok


summary(g)
summary(g1)
summary(g2)


res.tot = lm(sbp~(tobacco+ldl+adip+obesity+alchohol+age)*famhist,data=base)
g.tot <- step(res.tot,direction="both")
summary(g.tot)


#######################################################################################################
# Exercices supplémentaires

############################
# Exercice
############################

house<-read.table('houseprices.dat',header=T)
str(house)
attach(house)
pairs(house)
Age<-as.numeric(Age)
Tax<-as.numeric(Tax)
house.lm <- step(lm(Price~0+SQFT+Age+Features+Tax,subset=(1:117)[-79]),method="backward")
house.lm$anova
summary(house.lm)

par(mfrow=c(3,2))
plot(house.lm,which=1)
plot(house.lm,which=2)
plot(house.lm,which=3)
plot(house.lm,which=4)
plot(house.lm,which=5)
acf(resid(house.lm),ci=0.95)

shapiro.test(resid(house.lm))
# Shapiro-Wilk test for normality
# p-value = 0.0009 (normalite !)
raintest(house.lm)
# Rainbow test for non linearity
# p-value = 0.0457 (linearite !)
gqtest(house.lm)
# Golfeld-Quant Test for heteroscedasticity
# p-value = 0.8746 (homoscedasticite ok)
dwtest(house.lm)
# Durbin-Watson Test for autocorrelation
# p-value = 0.0073 (correlation !)

#vif(house.lm)
# pas de constante donc pas adapte

log.price <-(Price)
house.lm <- step(lm(log.price~.,subset=(1:117)[-79]))
summary(house.lm)

par(mfrow=c(3,2))
plot(house.lm,which=1)
plot(house.lm,which=2)
plot(house.lm,which=3)
plot(house.lm,which=4)
plot(house.lm,which=5)
acf(resid(house.lm),ci=0.95)

shapiro.test(resid(house.lm))
# Shapiro-Wilk test for normality
# p-value = 0.0009 (normalite !)
raintest(house.lm)
# Rainbow test for non linearity
# p-value = 0.0457 (linearite !)
gqtest(house.lm)
# Golfeld-Quant Test for heteroscedasticity
# p-value = 0.8746 (homoscedasticite ok)
dwtest(house.lm)
# Durbin-Watson Test for autocorrelation
# p-value = 0.0073 (correlation !)

vif(house.lm)


############################
# Exercice
############################

data(stackloss)
str(stackloss)
attach(stackloss)
pairs(stackloss)
stackloss.lm <- step(lm(stack.loss~.,data=stackloss,subset=1:20),method="backward")
stackloss.lm$anova
summary(stackloss.lm)

par(mfrow=c(3,2))
plot(stackloss.lm,which=1)
plot(stackloss.lm,which=2)
plot(stackloss.lm,which=3)
plot(stackloss.lm,which=4)
plot(stackloss.lm,which=5)
acf(resid(stackloss.lm),ci=0.95)

shapiro.test(resid(stackloss.lm))
# Shapiro-Wilk test for normality
# p-value = 0.9148 (normalite ok)
raintest(stackloss.lm)
# Rainbow test for non linearity
# p-value = 0.0022 (pas linearite !)
gqtest(stackloss.lm)
# Golfeld-Quant Test for heteroscedasticity
# p-value = 0.9489 (homoscedasticite ok)
dwtest(stackloss.lm)
# Durbin-Watson Test for autocorrelation
# p-value = 0.03166 (correlation !)

vif(stackloss.lm)
#  Air.Flow Water.Temp 
#  2.572632   2.572632 
# inferieures à 10 donc pas de colinearite



############################
# Exercice
############################

library(ISwR)
attach(kfm)
pairs(kfm)
g<-lm(weight~dl.milk+ml.suppl+mat.weight+mat.height)
res<-lm(weight~dl.milk)
res<-step(g)
summary(res)

par(mfrow=c(3,2))
plot(res,which=1)
plot(res,which=2)
plot(res,which=3)
plot(res,which=4)
plot(res,which=5)
acf(resid(res),ci=0.95)

shapiro.test(resid(res))
# Shapiro-Wilk test for normality
# p-value = 0.36 (normalite ok)
raintest(res)
# Rainbow test for non linearity
# p-value = 0.056 (linearite ok mais limite)
gqtest(res)
# Golfeld-Quant Test for heteroscedasticity
# p-value = 0.56 (homoscedasticite ok)
dwtest(res)
# Durbin-Watson Test for autocorrelation
# p-value = 0.53 independance ok)

vif(res)
#   dl.milk   ml.suppl mat.weight 
#  1.233734   1.007330   1.236448 
# inferieures à 10 donc pas de colinearite

new.weight=exp(weight)
g<-lm(new.weight~dl.milk+ml.suppl+mat.weight+mat.height)
res<-step(g)
summary(res)

par(mfrow=c(3,2))
plot(res,which=1)
plot(res,which=2)
plot(res,which=3)
plot(res,which=4)
plot(res,which=5)
acf(resid(res),ci=0.95)

shapiro.test(resid(res))
# Shapiro-Wilk test for normality
# p-value = 0.36 (normalite ok)
raintest(res)
# Rainbow test for non linearity
# p-value = 0.056 (linearite ok mais limite)
gqtest(res)
# Golfeld-Quant Test for heteroscedasticity
# p-value = 0.56 (homoscedasticite ok)
dwtest(res)
# Durbin-Watson Test for autocorrelation
# p-value = 0.53 independance ok)

vif(res)
#   dl.milk   ml.suppl mat.weight 
#  1.233734   1.007330   1.236448 
# inferieures à 10 donc pas de colinearite


#################################################################################


#####################################
# Exercice 
#####################################

library(faraway)
data(ozone)

o3.lm<-step(lm(O3~.,data=ozone))
summary(o3.lm)

par(mfrow=c(3,2))
plot(o3.lm,which=1)
plot(o3.lm,which=2)
plot(o3.lm,which=3)
plot(o3.lm,which=4)
plot(o3.lm,which=5)
acf(resid(o3.lm),ci=0.95)

shapiro.test(resid(o3.lm))
# Shapiro-Wilk test for normality
# p-value = 0.8432 (normalite ok)
raintest(o3.lm)
# Rainbow test for non linearity
# p-value = 0.6513 (linearite ok)
gqtest(o3.lm)
# Golfeld-Quant Test for heteroscedasticity
# p-value = 0.016 (homoscedasticite !)
dwtest(o3.lm)
# Durbin-Watson Test for autocorrelation
# p-value = 7e-08 (correlation !)

vif(o3.lm)
#humidity     temp      ibt      vis      doy 
#1.361547 4.559989 4.433784 1.439140 1.091938 
# inferieures à 10 donc pas de colinearite




#########################################################"

data(trees)
str(trees)
trees.lm <- step(lm(Volume~.,data=trees, subset=1:30),method="backward")
trees.lm$anova
summary(trees.lm)

par(mfrow=c(3,2))
plot(trees.lm,which=1)
plot(trees.lm,which=2)
plot(trees.lm,which=3)
plot(trees.lm,which=4)
plot(trees.lm,which=5)
acf(resid(trees.lm),ci=0.99)

shapiro.test(resid(trees.lm))
# Shapiro-Wilk test for normality
# p-value = 0.644 (normalite ok)
raintest(trees.lm)
# Rainbow test for non linearity
# p-value = 0.087 (linearite ok)
gqtest(trees.lm)
# Golfeld-Quant Test for heteroscedasticity
# p-value = 0.006 (heteroscedasticite !)
dwtest(trees.lm)
# Durbin-Watson Test for autocorrelation
# p-value = 0.009 (correlation !)

vif(trees.lm)
# Girth   Height 
# 1.36921 1.36921 
# inferieures à 10 donc pas de colinearite




#####################################


library(TH.data)
data(bodyfat)

