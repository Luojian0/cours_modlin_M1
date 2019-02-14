############################TP4

####################
#### Exercice 1 ####
####################

#1 
#foret<-rep(1:3,c(10,12,8))
J=c(6, 6, 6)
foret<-rep(1:3,c(6,6,6))
foret
#hauteur<-c(25.2,24.7,24.6,24.8,24.0,25.8,25.5,
           # 26.1,24.5,25.3,22.6,22.1,23.3,21.7,23.5,22.5,
          #  21.6,22.7,21.3,21.5,22.2,22.4,23.4,23.9,23.7,
           # 24.2,24.0,23.1,24.5,24.3)
f1 = c(23.4, 24.4, 24.6, 24.9, 25.0, 26.2)
f2 = c(18.9, 21.1, 21.1, 22.1, 22.5, 23.5)
f3 = c(22.5, 22.9, 23.7, 24.0, 24.0, 24.5)
#hauteur<-data.frame(foret_1 = f1, foret_2 = f2, foret_3 = f3)
hauteur<-c(f1, f2, f3)
hauteur
foret<-factor(foret)
arbre<-data.frame(foret,hauteur)
#rm(foret)
#rm(hauteur)
#arbre
#str(arbre)


moyennes<-tapply(arbre$hauteur,arbre$foret,mean)
moyenne
moy_tot<-mean(arbre$hauteur)
moy_tot
mean(moyennes)

ecart<-tapply(arbre$hauteur,arbre$foret,sd)
ecart
ecart.g<-sd(arbre$hauteur) 
ecart.g 
boxplot(arbre$hauteur~arbre$foret) 
#plot(arbre$foret, arbre$hauteur)
points(1:3, moyennes, pch="@")
abline(h=moy_tot)

### Q4

options(contrasts=c("contr.sum", "contr.poly"))  #indique que, dans le cas équilibré, les estimations des effets akpha de chaque groupe doivent être de somme nulle
m = lm(hauteur~foret, data=arbre)
anova(m)

### Q4
m_aov = aov(hauteur~foret, data=arbre)
summary(m_aov)

### Q6

#pas de test sur l'independance, dépend de la connaissance de l'expérimentateur
#normalité
shapiro.test(residuals(m))
#homogénéité des variances
bartlett.test(residuals(m)~foret, data=arbre)

### Q7
#tableau d'analyse de la variance
SCtot = sum((arbre$hauteur-moy_tot)^2) #variation totale (somme des carrés)
SCtot
SCfac = J[1]*(moyennes[1]-moy_tot)^2 + J[2]*(moyennes[2]-moy_tot)^2 + J[3]*(moyennes[3]-moy_tot)^2
SCfac
SCres = sum((arbre$hauteur-rep(moyennes, J))^2)
SCres

### Q8
#test de l'adéquation du modèle
eta2 = 1 - SCres/SCtot
eta2

### Q9
coef(m)
-sum(coef(m)[2:3]) #aplha3

library(granova)
granova::granova.1w(arbre$hauteur, arbre$foret)

### Q10
#Les 3 conditions fondamentales sont remplies. On rehete H0. Les moyennes sont différentes dans leur ensemble

###Q11
m1 = aov(hauteur~foret, data=arbre)
TukeyHSD(m1)
plot(TukeyHSD(m1))
#Permet de faire des comparaisons multiples. A n'utiliser que si on a des facteur à effet fixe. Le Modèle aov spécifie qu'on est bien dans le cadre de facteur à effet fixe!


####################
#### Exercice 2 ####
####################

### Q1
#Yij = mu + alpha_i + epsilon(_(i,j)) avec la contrainte somme des alpha = 0
options(contrasts=c("contr.sum", "contr.poly"))

### Q2
J=c(5, 5, 5, 5, 5,5)
variete<-rep(1:6,J)
variete
v1 = c(93.6, 95.3, 96.0, 93.7, 96.2)
v2 = c(95.3, 96.9, 95.8, 97.3, 97.7)
v3 = c(94.5, 97.0, 97.8, 97.0, 98.3)
v4 = c(98.8, 98.2, 97.8, 97.2, 97.9)
v5 = c(94.6, 97.8, 98.0, 95.0, 98.9)
v6 = c(93.2, 94.4, 93.8, 95.6, 94.8)

vitamine<-c(v1, v2, v3, v4, v5, v6)
vitamine
variete<-factor(variete)
df<-data.frame(variete,vitamine)
df
str(df)

m = lm(vitamine~variete, data=df)
#pas de test sur l'independance, dépend de la connaissance de l'expérimentateur
#normalité
shapiro.test(residuals(m))
#homogénéité des variances
bartlett.test(residuals(m)~variete, data=df)

### Q3
anova(m)

### Q4
# rejet de H0
granova::granova.1w(df$vitamine, df$variete)

### Q5 
anova(m)[2,3] #carre moyen residuel du tableau de l'anova

### Q6
# oui car fisher signiticatif
m1 = aov(vitamine~variete, data=df)
TukeyHSD(m1)
plot(TukeyHSD(m1))
