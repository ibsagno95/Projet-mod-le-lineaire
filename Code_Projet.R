library(readxl)
library(car)
library(ggplot2)
library(dplyr)
library(stringr)
library(psych)

setwd("E:/Study/Master_SEP_2019/S2/Echantillonnage/Projet")

#Importation de la base de donn?es :
data <- read_xlsx("Dataset.xlsx") 
View (data)

# statistiques descriptives ------------------------------------------------
describe(data)

# Graphiques --------------------------------------------------------------

data=data%>%
  mutate(Init=str_sub(Pays,1,2))

ggplot(data )+ geom_bar(aes(x = Init,y=Taux_mort,width=0.3,fill=Pays),stat = "identity",) +
  ggtitle("Répartition des taux de mortalité par pays")+ xlab("Pays")+ylab("Taux de mortalité")

data[which((data$Taux_mort)>1078.08),]

#Recodage de la base de donnée
data <- as.data.frame(data)
row.names(data) <- data[,1]
data <- data[,-1]
n <- nrow(data)



#S?lection du mod?le :
#Application de la m?thode exhaustive :
XX <- model.matrix(Taux_mort ~., data = data) #Matrice de design
p <- ncol(XX)-1 #Nombre de variables num?riques explicatives dans le mod?le de RLM complet
require(leaps)
select.modeles <- regsubsets(Taux_mort ~ ., data = data, 
                             nbest = 1, nvmax = p, method = "exhaustive")

summary(select.modeles)$bic

plot(select.modeles, scale = "bic")


#selon BIC
select.mod.gen <- glmulti(Taux_mort ~ ., data = data, level = 1, method = "g", 
                          fitfunction = lm, crit = 'bic', plotty = F)
bic.best.model <- summary(select.mod.gen)$bestmodel
bic.best.model


## Estimation de l'erreur de pr?vision par la m?thode LOOCV :
modele1 <- glm(Taux_mort ~ alcool + dep_sante, data = data)
modele1


library(boot) 
estimation_erreur_modele1 <- cv.glm(data = data, glmfit = modele1, K = n)$delta[1] #estimation de l'erreur du modele1 par la m?thode LOOCV
estimation_erreur_modele1

modele2 <- glm(formula = Taux_mort ~ alcool + I(alcool^2) + dep_sante + I(dep_sante^2), data = data)
modele2


estimation_erreur_modele2 <- cv.glm(data = data, glmfit = modele2, K = n)$delta[1] #estimation de l'erreur du modele2 par la m?thode LOOCV

modele3 <- glm(formula = Taux_mort ~ alcool + I(alcool^2) + I(alcool^3) + dep_sante + I(dep_sante^2) + I(dep_sante^3), data = data)
modele3

estimation_erreur_modele3 <- cv.glm(data = data, glmfit = modele3, K = n)$delta[1] #estimation de l'erreur du modele3 par la m?thode LOOCV

print(c("R?sultats des estimations par LOOCV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation_erreur_modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2)),
        paste("Estimation de l'erreur du modele3 = ", as.character(estimation_erreur_modele3)))) 




## Mod?le de r?gression lin?aire qui minimise l'erreur de pr?vision:
modele2 


##v?rification des hypoth?ses de la r?gression
##Tester la non-corr?lation (d'ordre 1) des erreurs : test de Durbin-Watson
require(lmtest)
dwtest(modele2, alternative = c("two.sided"))
#Visualisation graphique :
acf(modele2$residuals)


##V?rification de l'hypoth?se d'homoscedasticit? des erreurs :
#Test d'homoscedasticit? de Breusch-Pagan
require(lmtest)
bptest(modele2, studentize = FALSE)
#Visualisation graphique :
plot(modele2, 3)


## v?rification de la normalit? des erreurs :
#Test de Shapiro-Wilk pour tester l'hypoth?se de normalit? du terme d'erreur
shapiro.test(residuals(modele2))

##R?sultats d'estimation :
summary(model)

## Reconstruction du mod?le lin?aire choisi par l'algorithme g?n?tique :
model <- lm (Taux_mort ~ alcool + dep_sante, data = data)


##Tester la non-corr?lation (d'ordre 1) des erreurs : test de Durbin-Watson
require(lmtest)
dwtest(model, alternative = c("two.sided"))
#Visualisation graphique :
acf(model$residuals)


##V?rification de l'hypoth?se d'homoscedasticit? des erreurs :
#Test d'homoscedasticit? de Breusch-Pagan
require(lmtest)
bptest(model, studentize = FALSE)
#Visualisation graphique :
plot(model, 3)


## v?rification de la normalit? des erreurs :
#Test de Shapiro-Wilk pour tester l'hypoth?se de normalit? du terme d'erreur
shapiro.test(residuals(model))

##Valeurs influentes:
# Distance de Cook :
plot(model, 4)
abline(h=4/(28-2-1), col="red")
# suppression de la valeur influente :
data <- data[-28,]

##R?sultats de l'estimation :
summary(model)

##Classement des variables explicatives selon les valeurs 
#des p_values croissantes (du test de Student)
vect.pvalues.Student <- summary(model)$coefficients[,"Pr(>|t|)"]
#On supprime la p_value de l'intercept 
vect.pvalues.Student <-vect.pvalues.Student[2:length(vect.pvalues.Student)] 
#Variables explicatives ordonn?es, de la plus significative ? la moins significative
sort(vect.pvalues.Student)

##Classement des variables explicatives selon les valeurs des 
#P_values croissantes du test de Fisher
tests.Fisher <- anova(model)
tests.Fisher
m <- nrow(tests.Fisher)
vect.pvalues.Fisher <- tests.Fisher[1:m-1,"Pr(>F)"] 
names(vect.pvalues.Fisher) <- rownames(tests.Fisher[1:m-1,])
sort(vect.pvalues.Fisher)

#Comparaison avec le classement selon le test de Student
sort(vect.pvalues.Fisher)
sort(vect.pvalues.Student) 





#Importation de la base de donn?es :
data <- read_xlsx("Dataset.xlsx") 
View (data)
data <- as.data.frame(data)
row.names(data) <- data[,1]
data <- data[,-1]
n <- nrow(data)

XX <- model.matrix(Taux_mort ~., data = data)[,-1] #Matrice de design
#Regression ridge 
require(glmnet)
reg.ridge <- glmnet(x = scale(XX), y = data[,1], alpha = 0)
par(mfrow = c(1,2))
plot(reg.ridge, label = TRUE)
plot(reg.ridge, xvar = "lambda", label = TRUE, lwd = 2)
reg.cvridge <- cv.glmnet(x = scale(XX), y = data[,1], alpha = 0)
#Le choix de lambda 
bestlam <- reg.cvridge$lambda.min
bestlam
par(mfrow = c(1,1))
plot(reg.cvridge)
#coeficient de la regression ridge :
coef(reg.cvridge)

#erreur de prevision du modele ridge optimal 
erreur.modele.ridge.opt =min(reg.cvridge$cvm) 
erreur.modele.ridge.opt
#Erreur de prevision du modele choisi par l'lagorithme genetique:
#par la méthode LOOCV : 
estimation_erreur_modele1
#par la methode K-fold CV :
erreur.modele1 <- cv.glm(data = data, 
                                    glmfit =  glm(formula = model, 
                                                  data = data), K = 9)$delta[1]
erreur.modele1

#Regression Lasso
reg.lasso <- glmnet(x = scale(XX), y = data[,1], alpha = 1)
par(mfrow = c(1,2))
plot(reg.lasso, label = TRUE)
plot(reg.lasso, xvar = "lambda", label = TRUE, lwd = 2)

reg.cvlasso <- cv.glmnet(x = scale(XX), y = data[,1], alpha = 1)
bestlam <- reg.cvlasso$lambda.min
bestlam
par(mfrow = c(1,1))
plot(reg.cvlasso)
coef(reg.cvlasso)
#erreur de prevision du modele lasso optimal 
erreur.modele.lasso.opt <- min(reg.cvlasso$cvm) 
erreur.modele.lasso.opt
