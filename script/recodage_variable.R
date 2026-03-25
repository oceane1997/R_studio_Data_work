library(questionr)
library(tidyverse)

library(labelled)

data("hdv2003")
View(hdv2003)

look_for(hdv2003)
look_for(hdv2003,'col')
#------------------------------- RENOMMER
hdv2003 <-  hdv2003 |> rename(bricolage = bricol) 

#------------------------------- CONVERTIR 
describe(hdv2003$freres.soeurs)

x <- c("a","b","a","c","b","a")

describe(x)

y <- factor(x)  #factor offre plus d'options que as.factor. on peut mettre les level et meme des label
describe(y)

y <- factor(x, levels = c("c","b","a"))   #on peut preciser les levels et leur ordre
describe(y)

as.character(y)
as.integer(y)   #donne la position du level c'est c b a donc a est en 3ieme position

#-----------------
data(fecondite)
look_for(femmes)
glimpse(femmes)
view(femmes)

describe(femmes$region)

to_factor(femmes$region) |> describe()

#conversion en facteur de toutes les variables labelisees

ff <- femmes |> to_factor(levels = "p") #p pour maintenir les numeros ou valeurs apres conversion, dangereuc car parfois variables numeriques sont labellisees

look_for(ff) 

#voire unlabelled qui trqnsforme strictement les variables entierment labelisees en factor et les autres en numerique 
#conversioon automatisee avec unlabelled webinR 05 : 45MIN15 ----

##conversion variable numerique en classe ----## Cutting hdv2003$age into hdv2003$groupe_age
hdv2003$groupe_age <- cut(hdv2003$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(18, 25, 40, 60, 75, 97)
)
## Recoding hdv2003$nivetud into hdv2003$niveau_etude #shift tab et tab pour deplacer un bloc de code a gauche ou a droite
hdv2003$niveau_etude <- hdv2003$nivetud %>%
  fct_recode(
    "Primaire" = "N'a jamais fait d'etudes",
    "Primaire" = "A arrete ses etudes, avant la derniere annee d'etudes primaires",
    "Primaire" = "Derniere annee d'etudes primaires",
    "Secondaire" = "1er cycle",
    "Secondaire" = "2eme cycle",
    "Technique" = "Enseignement technique ou professionnel court",
    "Technique" = "Enseignement technique ou professionnel long",
    "Superieur" = "Enseignement superieur y compris technique superieur"
  ) %>%
  fct_explicit_na("Manquant")

#fonction de forcat tres interessantes pour manipuler des variables factor 
#eg : fct_lump et ses variations pour regrouper les modalites peu presentes selon certains criteres



library(readxl)
library(dplyr)

file_path <- "Data/stastique 2024.xlsx"
sheets <- excel_sheets(file_path)

df_all <- lapply(sheets, function(sh) {
  read_excel(file_path, sheet = sh) %>% mutate(.sheet = sh)
}) %>% bind_rows()

print(df_all)




