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
datos <- read.csv("github_data.csv")

#para guardar la versión creas un commit: pestaña git ---- commit --- staged ticks