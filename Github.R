library()

#PARA DARSE DE ALTA, SOLO HAY QUE HACERLO UNA VEZ

library(usethis)
use_git_config(user.name = "andresdgalan",
               user.email = "andresdgalan@gmail.com")

git_sitrep()

#para crear un token
usethis::create_github_token()

#este es el c?digo del token pero no sirve para nada porque la siguiente funci?n lo coge del portapapeles
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

gitcreds::gitcreds_set()
#sale un mensajito: copias el c?digo de arriba en la consola y enter

#CREO UN REPOSITORIO (como una carpeta o un proyecto) EN GITHUB Y ME LO TRAIGO A RSTUDIO

#en R creo un proyecto con version control ----> Github y le copio el URL del repositorio que sale en la web 

library(tidyverse)
library(ggplot2)
datos <- read.csv("github_data.csv")

#para guardar la versión creas un commit: pestaña git ---- commit --- staged ticks ---- push

plot(happiness ~ work.hours, data = datos)

#apuntar cosas en issues

ggplot(datos) + geom_point(aes(work.hours, happiness))

modelo <- lm(happiness ~ work.hours, data = datos)

#Pruebas por mi cuenta

hist(datos$work.hours)
hist(datos$happiness)

cor.test(datos$work.hours, datos$happiness)

#subir datos de DIVERSIFICA

DIVERSIFICA.plot <- read.csv2("st.DIVERSIFICA.plot.csv")
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

#remove one row
DIVERSIFICA.plot <- DIVERSIFICA.plot[-27, ]

DIVERSIFICA.plot$def <- as.numeric(DIVERSIFICA.plot$def)

#check assumptions
hist(DIVERSIFICA.plot$def)
shapiro.test(DIVERSIFICA.plot$def)


