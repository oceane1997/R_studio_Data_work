#####PACKAGES

install.packages(c("arrow", "babynames", "curl", "duckdb", "gapminder", "ggrepel", 
                     "ggridges", "ggthemes", "hexbin", "janitor", "Lahman", "leaflet", "maps", 
                     "nycflights13", "openxlsx", "palmerpenguins", "repurrrsive", "tidymodels", "writexl"))





library(tidyverse)

library(palmerpenguins)
library(ggthemes)

palmerpenguins::penguins
glimpse(penguins)

view(penguins)

names(penguins)


######VISUALISATION AVEC GGPLOT----


#d'abord appeler la fonction ggplot, la base concernee, puis les mapping avec la bonne esthetic aes(x,y)


ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g )) ##ne nous donne qu'un repere avec les axes


#il faut maintenant choisir la forme geometrique qu'on veut que prennt les variables, d'où l'argument
#geom [geom_point, geom_bar, geom_line , geom_boxplot, geom_line]


ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g )) +
       geom_point()

#representer les points enfonctions de leur espece

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g , color = species )) +
  geom_point()

#rajouter une droite de regression pour mieux voir le lien entre la longueur des ailes et le poids
#avec la fonction geom_smooth

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g , color = species )) +
  geom_point() +
  geom_smooth(method = "lm")


#pour avoir une seule droite de regression au lieu de l'appliquer a meme geom_smooth, appliquer l'esthetiaue
#a seulement geom_point


ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g  )) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")


#pour changer la legende, utiliser lla fonction labs avec les arguments
#title, subtitle, x,y...

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g  )) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")+
  labs(title = "Flipper length and body mass",
       subtitle = "Dimensions for each species",
       x = "Flipper length (mm) ",
       y = "Body mass (g) ",
       color = "Species",
       shape = "Species") +
  scale_color_colorblind()






































