#GLM: Marco común

#y = variable respuesta (i = tamaño muestral), a = y cuando x = 0, b = pendiente de la recta (tasa de cambio de y en función de x)
#epsilon = residuos/error del modelo: diferencia entre valores observados y valores esperados
#los residuos del modelo tienen que tener una distribución normal centrada en 0, con una varianza homogénea: NORMAL, HOMOCEDÁSTICO Y NO SESGADO

#Toy dataset
trees <- read.csv("trees.csv")

#exploramos
plot(trees$height)
hist(trees$height)

hist(trees$dbh)

plot(height ~ dbh, data = trees, las = 1)

m1 <- lm(height ~ dbh, data = trees)

#paquete que te da la ecuacion del modelo
library(equatiomatic)

equatiomatic::extract_eq(m1, use_coefs = T)
#\operatorname{\widehat{height}} = 19.34 + 0.62(\operatorname{dbh})

plot(residuals(m1))
hist(residuals(m1))

library(parameters)
parameters(m1)

library(easystats)
check_model(m1)

#################EJERCICIO
str(trees)
plot(trees$height ~ trees$sex)
m2 <- lm(height ~ sex, data = trees)
trees$sex <- as.factor(trees$sex)
summary(m2)
check_model(m2)
visreg(m2)
gam1 <- gam(height ~ sex, data = trees)
visreg(gam1)

boxplot(trees$height ~ trees$site)
trees$site <- as.factor(trees$site)
m2 <- lm(height ~ site, data = trees)
summary(m2)
check_model(m2)
visreg(m2)

filter(trees, site == "2")

plot(all.effe)

#para comparar por pares:
estimate_contrasts(m2)




#subir datos de DIVERSIFICA

DIVERSIFICA.plot <- read.csv2("st.DIVERSIFICA.plot.csv")
str(DIVERSIFICA.plot)
DIVERSIFICA.tree <- read.csv2("st.DIVERSIFICA.tree.strange.csv")

#packages for modelling and visualization
library(MuMIn)
library(DHARMa)
library(ggplot2)
library(mgcv) 
library(tidymv) 
library(visreg) 
library(mgcViz) 
library(dplyr)

#ANALISIS PLOT############################################

#remove one row
DIVERSIFICA.plot <- DIVERSIFICA.plot[-27, ]

DIVERSIFICA.plot$def <- as.numeric(DIVERSIFICA.plot$def)

#check assumptions
hist(DIVERSIFICA.plot$def)
shapiro.test(DIVERSIFICA.plot$def)

str(DIVERSIFICA.plot)
DIVERSIFICA.plot <- DIVERSIFICA.plot %>% mutate_if(is.character,as.numeric)
DIVERSIFICA.plot$plot <- as.character(DIVERSIFICA.plot)
DIVERSIFICA.plot$DBH2 <- DIVERSIFICA.plot$DBH^2
FullModel<- lm (def ~ DBH + DBH2 + mycrhz + NO3 + comp, data = DIVERSIFICA.plot.na, na.action = na.fail)
#############################
#to run in background

job::job({
  res <- dredge(FullModel, trace=2)
})

res <- dredge(FullModel, trace=2)
subset(res, delta <= 2)
summary(model.avg(res, subset = delta < 2, revised.var=FALSE))
sw(res)
sw(subset(model.sel(res), delta <= 2))
##Paquete DHARMA
residuals <- simulateResiduals(fittedModel = FullModel)
plot(residuals)

##########################################################