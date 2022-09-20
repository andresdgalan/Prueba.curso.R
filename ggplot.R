library(here)
library(readr)
but_sum <- read_csv("butterfly_summary.csv")
  
#Datos=but_sum
#abundancia en funcion de la fecha   
#triángulos naranjas
ggplot(but_sum, aes(x=date, y=abundance)) +
  geom_point(color = "orange",
             shape = "triangle")

#especies en función del tiempo
#tamaño del punto representa la abundancia
#cuando te refieres a algo del dataset tienes que meterlo en aes
ggplot(but_sum, aes(x=date, y=n_species, size = abundance)) +
  geom_point()


ggplot(but_sum, aes(x=date, y=abundance, color = n_species)) +
  geom_point() + 
  scale_color_viridis_b()

ggplot(but_sum, aes(x=date, y=abundance, color = n_species)) +
  geom_point() + 
  scale_color_viridis_c()

ggplot(but_sum, aes(x=date, y=abundance, color = n_species)) +
  geom_point() + 
scale_color_gradient(low = "gold",
                     high = "red")



library(RColorBrewer)
display.brewer.all(
  colorblindFriendly = TRUE)

ggplot(but_sum, aes(x=date, y=abundance, color = n_species)) +
  geom_point() + 
  scale_color_distiller(palette = "BuGn")


#modificar los ejes
ggplot(but_sum, aes(x=date, y=abundance, color = n_species)) +
  geom_point() + 
  scale_y_log10()

ggplot(but_sum, aes(x=date, y=abundance, color = n_species)) +
  geom_point() + 
  scale_y_reverse()

#Graficar numero de especies vs. abundancia, donde el color del punto represente el tipo de hábitat (forest vs.open).
ggplot(but_sum, aes(x=abundance, y=n_species, color = habitat)) +
  geom_point() 

#linea que se ajusta a los datos
ggplot(but_sum, aes(x=abundance, y=n_species, color = habitat)) +
  geom_point() +
  geom_smooth()

#añadir dónde aparecen los datos
ggplot(but_sum, aes(x=abundance, y=n_species, color = habitat)) +
  geom_point() +
  geom_smooth() + 
  geom_rug()

#acotar la gráfica, hay una forma de que no tiene en cuenta lo que se sale y otra que solamente no lo enseña
ggplot(but_sum, aes(x=abundance, y=n_species, color = habitat)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  scale_y_continuous(limits = c(0,40)) +
  scale_x_continuous(limits = c(0,250))


ggplot(but_sum, aes(x=date, y=n_species, color = abundance)) +
  geom_point() +
  labs(x = "Año",
       y = "Número de especies") +
  scale_color_distiller(palette = "BuGn")
