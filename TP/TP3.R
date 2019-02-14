library(lmtest)
library(car)
library(lsmeans)

############################
# Exercice 1
############################


graphics.off()

library(faraway)
data(coagulation)
str(coagulation)
attach(coagulation)

hist(coag)

par(mfrow=c(1,2))
plot(diet,coag)
plot(as.numeric(diet),coag)

modele<-aov(coag~diet)

anova(modele)
#            Df Sum Sq Mean Sq F value   Pr(>F)    
#diet         3    228    76.0   13.57 4.66e-05 ***
#Residuals   20    112     5.6                   

(res<-lsmeans(modele,~diet))
pairs(res)
plot(res)


x11()
par(mfrow=c(2,3))
plot(modele)
plot(modele,which=4)
acf(resid(modele))


shapiro.test(resid(modele))
# Shapiro-Wilk test for normality
# p-value = 0.86 (normalite ok)


bartlett.test(coag,diet)
# Test for homogeneity of variances between groups
# p-value = 0.6441 (homogeneite ok)


#############

new.diet <- factor(0*(diet=="A")+0*(diet=="D")+1*(diet=="B")+1*(diet=="C"),labels=c("(A-D)","(B-C)"))

plot(new.diet,coag)

new.modele<-aov(coag~new.diet)

summary(new.modele) #ou anova(modele)
#            Df Sum Sq Mean Sq F value   Pr(>F)    
#new.diet     2    228  114.00   21.38 8.63e-06 ***
#Residuals   21    112    5.33                     
   

x11()
par(mfrow=c(2,3))
plot(new.modele)
acf(resid(new.modele))
plot(new.modele,which=4)

TukeyHSD(new.modele,"new.diet")
# Toujours diff entre B et C non significative

bartlett.test(coag,new.diet)
# Test for homogeneity of variances between groups
# p-value = 0.54 (homogeneite ok)

anova(modele,new.modele)

############################
# Exercice  2
############################

library(multcomp)
attach(recovery)

(modele <- aov(minutes ~ blanket, data = recovery))
coef(modele)
# pour mettre la "b2" comme référence
#(modele <- aov(minutes ~ C(blanket,base=3), data = recovery))
#coef(modele)
  
anova(modele)




shapiro.test(resid(modele))
# Shapiro-Wilk test for normality
# p-value = 0.77

bartlett.test(minutes,blanket)
# p.val = 0.82 (homogeneite ok)

library(lsmeans)
TukeyHSD(modele,"blanket")
(res<-lsmeans(modele,~blanket))
pairs(res)
plot(res)


res <- lsmeans(modele, ~blanket)
( contrast(res, "trt.vs.ctrl", ref = 3) )



############################
# Exercice 3
############################


library(MASS)
head(anorexia)

summary(anorexia)

names(anorexia)

attach(anorexia)
hist(Prewt)
plot(Prewt,Postwt)
cor.test(Prewt,Postwt)

#différence avant apres en terme de moyenne ?

boxplot(Prewt,Postwt)
t.test(Prewt,Postwt, paired=TRUE)
wilcox.test(Prewt,Postwt,paired=TRUE)
t.test(Prewt,Postwt, paired=TRUE, alternative="less")


#différence poids avant entre groupe de traitement
boxplot(Prewt~Treat,main="Boites à moustache du poids avant traitement pour chaque groupe",names=c("Comportemental","Controle","Familial"))
summary(aov(Prewt~Treat))
#différence poids apres entre groupe de traitement
boxplot(Postwt~Treat,main="Boites à moustache du poids après traitement pour chaque groupe",names=c("Comportemental","Controle","Familial"))
summary(aov(Postwt~Treat))

#difference avant apres selon les groupes
#creer nouvelle variable
anorexia$diff=Postwt-Prewt
attach(anorexia)
shapiro.test(diff)
tapply(diff,Treat,mean)
boxplot(diff~Treat,main="Boites à moustache de la prise de poids pour chaque groupe",names=c("Comportemental","Controle","Familial"))
#gain pour groupe controle : 
controle=anorexia[Treat=="Cont",]
t.test(controle$diff)
#gain pour groupe comportemental
comport=anorexia[Treat=="CBT",]
t.test(comport$diff)
t.test(comport$diff,alternative="greater")
#gain pour groupe famille
famille=anorexia[Treat=="FT",]
t.test(famille$diff)
t.test(famille$diff,alternative="greater")


#comparaison des 3 groupes
Treat=as.factor(Treat)
res=aov(diff~Treat)

summary(res)
TukeyHSD(res,Treat)
#     CBT    Cont  
#Cont 0.1368 -     
#FT   0.1368 0.0048


bartlett.test(diff,Treat)
#	Bartlett test of homogeneity of variances
# data:  diff and Treat
# Bartlett's K-squared = 0.30428, df = 2, p-value = 0.8589



###################################################################
###################################################################
graphics.off()

library(MASS)
data(whiteside)
str(whiteside)
attach(whiteside)

hist(Gas)
plot(Insul,Gas)

(modele<-lm(Gas~Insul))

anova(modele)


res<-lsmeans(modele,~Insul)
pairs(res)
plot(res)
#      <25     25-29   30-35  
#25-29 0.076   -       -      
#30-35 0.027   0.570   -      
#>35   1.9e-09 5.9e-06 3.9e-05


x11()
par(mfrow=c(2,3))
plot(modele)
acf(resid(modele))
plot(modele,which=4)
# autocorrelation !

shapiro.test(resid(modele))
# Shapiro-Wilk test for normality
# p-value = 0.80

bartlett.test(Gas,Insul)
# p.val = 0.06 (homogeneite ok)

############################
# Exercice 
############################
graphics.off()

library(MASS)
data(Insurance)
str(Insurance)
attach(Insurance)

hist(Claims)
log.claims <- claims#log(Claims+1)

plot(Age,log.claims)

(modele<-aov(log.claims~Age))

anova(modele)

(res<-TukeyHSD(modele,"Age"))
plot(res)

res<-lsmeans(modele,~Age)
plot(res)
pairs(res)


x11()
par(mfrow=c(2,3))
plot(modele)
acf(resid(modele))
plot(modele,which=4)
# autocorrelation !

shapiro.test(resid(modele))
# Shapiro-Wilk test for normality
# p-value = 0.80

bartlett.test(log.claims,Age)
# p.val = 0.56 (homogeneite ok)


res<-lsmeans(modele,~Age)
pairs(res)
plot(res)


############################
# Exercice 
############################

data(Salaries)
str(Salaries)
attach(Salaries)

indices <- (yrs.service>=10)
salary10 <- salary[indices]
sex10 <- sex[indices]
hist(salary10)
shapiro.test(log(salary10))
# Shapiro-Wilk test for normality
# p-value = 0.67
salary10<-log(salary10)

plot(sex10,salary10)

modele<-lm(salary10~sex10)

anova(modele)
#           Df     Sum Sq    Mean Sq F value   Pr(>F)   
#sex         1 6.9800e+09 6980014930  7.7377 0.005667 **
#Residuals 395 3.5632e+11  902077538      

pairwise.t.test(salary10,sex10)
#     Female
#Male 0.16??anova

x11()
par(mfrow=c(2,2))
plot(modele)

x11()
par(mfrow=c(2,2))
plot(aov(salary~sex))

bartlett.test(salary,sex)
# p.val = 0.95 (homogeneite ok)



############################
# Exercice 
############################

data(iris)
attach(iris)
names(iris)
help(iris)

y<-Sepal.Length
x<-Species

hist(y)
shapiro.test(log(y))
# p-val = 0.053, limite
y<-log(y)

(modele<-lm(y~x))
anova(modele)

x11()
par(mfrow=c(2,2))
plot(modele)

pairwise.t.test(y,x)
# ok
bartlett.test(y,x)
# non

#################################################################################
## Exercices supplémentaires
############################
# Exercice 2
############################


data(OrchardSprays) # 1947 = niveaux de lime sulfur
str(OrchardSprays)
attach(OrchardSprays)

par(mfrow=c(1,2))
plot(treatment,decrease)
plot(as.numeric(treatment),decrease)

modele<-aov(decrease~treatment)
modele
#Call:
#   aov(formula = decrease ~ treatment)
#
#Terms:
#                treatment Residuals
#Sum of Squares   56159.98  23569.62
#Deg. of Freedom         7        56

#Residual standard error: 20.51551
#Estimated effects may be unbalanced

summary(modele)
#            Df Sum Sq Mean Sq F value  Pr(>F)    
#treatment    7  56160    8023   19.06 9.5e-13 ***
#Residuals   56  23570     421                    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(modele)
#Analysis of Variance Table
#
#Response: decrease
#          Df Sum Sq Mean Sq F value    Pr(>F)    
#treatment  7  56160  8022.9  19.062 9.499e-13 ***
#Residuals 56  23570   420.9                      
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

pairwise.t.test(decrease,treatment)

plot(modele)

shapiro.test(resid(modele))
# Shapiro-Wilk test for normality
# p-value = 0.0004 (normalite !)
bartlett.test(decrease,treatment)
# Test for homogeneity of variances between groups
# p-value = 5.128e-07 (homogeneite !)




