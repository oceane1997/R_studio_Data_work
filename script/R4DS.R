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


# 8. recreate visualization

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(colour = bill_depth_mm))+
  geom_smooth()

levels(penguins$island)

# 9. same
levels(penguins$island)

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)) +
  geom_point()+
  geom_smooth(se = F)


ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

##categorical variable visualization ----
ggplot(penguins,
       aes(x=species))+
  geom_bar()

#pour les variables categorielles non ordinales, preferer une presentation selon un ordre de frequence 
#avec la fonction fct_infreq


ggplot(penguins,
       aes(x = fct_infreq(species)))+
  geom_bar()

##############numerical variable (continuous)
####                 HISTOGRAM
ggplot(penguins,
       aes(x = body_mass_g))+
  geom_histogram(binwidth = 200) #play with different binwidth to see which one show a better distribution of our data set


ggplot(penguins,
       aes(x = body_mass_g))+
  geom_histogram(binwidth = 20)
ggplot(penguins,
       aes(x = body_mass_g))+
  geom_histogram(binwidth = 2000)


####                DENSITY PLOT
#moins detaillé qu'un histogram but we can quickly see the shape of the distribution (modes and skewness)

ggplot(penguins,
       aes(x = body_mass_g))+
  geom_density()



######EXERCISES 2 ----

#1. «Make a bar plot of species of penguins, where you assign species to the y aesthetic.»

ggplot(penguins,
       aes(y = species)) +
  geom_bar()

#2. 

ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")       #utiliser l'argument fill plutot que color pour colorer les bar



#3. the bins argument in geom_histogram shows the number of bins (Defaults to 30)

str(diamonds)

ggplot(diamonds,
       aes(x = carat)) +
  geom_histogram()

ggplot(diamonds,
       aes(x = carat)) +
  geom_histogram(binwidth = 0.30)

ggplot(diamonds,
       aes(x = carat)) +
  geom_density()

######Visualizing relationship
#Numerical and categorical variable

         #we can use side by side boxplot for each level of the categorical variable



ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()

        #density plot for each species body mass

ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75) #epaisseur des lignes


ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5) #alpha to add transparency between 0 and 1


