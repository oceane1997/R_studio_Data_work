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


######EXERCISES 1 ----
###### 1.How many rows are in penguins? How many columns?

str(penguins)
#344 lignes et 8 colonnes

###### 2.What does the bill_depth_mm variable in the penguins data frame describe? Read the help for ?penguins to find out. »

?penguins #epaisseur du bec de penguin


##### 3.«Make a scatterplot of bill_depth_mm versus bill_length_mm. 
#That is, make a scatterplot with bill_depth_mm on the y-axis and bill_length_mm on the x-axis.
# Describe the relationship between these two variables.»



ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, y= bill_depth_mm))+
  geom_point(mapping = aes(color = species))


ggplot(data = penguins,
       mapping = aes(x = species, y= bill_depth_mm))+
  geom_line()


ggplot(data = penguins,
       mapping = aes(x = species, y= bill_depth_mm))+
  geom_boxplot()


ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, y= bill_depth_mm))+
  geom_point(mapping = aes(color = species), na.rm = T)        #pas de message lorsque les valeurs manquantes sont retirees


ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, y= bill_depth_mm))+
  geom_point(mapping = aes(color = species), na.rm = T) +
  labs(caption = "Data come from the palmerpenguins package") #argument caption dans labs pour un petit texte
                                                              #en bas à droite










