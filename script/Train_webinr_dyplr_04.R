library(readxl)
library(gtsummary)
library(broom)
library(GGally)
library(tidyverse)
library(nycflights13)


data("flights")
view(flights)
?flights

data("airports")
view(airports)


data("airlines")
view(airlines)

####-------------------SLICE : selection d'observations ou de lignes par rapport a leur position

slice(flights, 10)  #meme resultat flights[10,]

slice(airports, 10:45)   # idem que airports[10:45,]

slice(airports, 10,12,14)   # idem que airports[c(10,12,14),] ou slice(airports, c(10,12,14))

pos <- c(10,12,14)

slice(airports,pos)

tmp <- slice(airports,pos)

#####----------------FILTER : Selection sur certaines conditions

flights[flights$month == 1,]  # selection des lignes du mois de janvier

filter(flights, month == 1)   # idem mais ecritrure plus courte car comprend que month est choisi dans flight

filter(flights, dep_delay >= 10 & dep_delay <= 15) # selection vol avec un retard au depart entre 10 et 15 min

filter(flights, dep_delay >= 10, dep_delay <= 15) # idem passer plusieurs conditions separees par une virgule revient a utiliser &

filter(flights, distance == max(distance))       # vol les plus longs

?filter # pour aller plus loin 

####-------------SELECT : Selection de variables

select(airports, lat, lon) #idem airports[,c("lat","lon")]

select(airports, -lat, -lon)

glimpse(flights)

select(flights, starts_with("dep"))  # selection des variables dont le nom commence par ...

select(flights, starts_with("dep"), hour, flight) #serie de conditions possible

select(flights, day:dest) #selection de toutes les variables d'un a une autre

select(flights, !day:dest) # ou plutot tout sauf

?select

select(flights, where(is.integer)) #selection des variables de type integer

v <- c("month", "day","distance","dest")

#select(flights, v) #ne fonctionne pas car il v n'existe pas dans flights donc utiliser all_of ou any_of selon qu'on veut etre strict ou non

select(flights, all_of(v))  #any_of est plus permissif pour les variables non concordantes

select(airports, latitude= lat, longitude = lon) #renommer les variables avec select

select(airports, lon, lat, everything()) # pour changer l'ordre d'apparition des variables et ajouter toutes celles qui restent avec everything

###------------------RENAME : Renomme une variable

rename(airports, latitude = lat) #pour le modifier reellement dans le tableau il faut airports <- rename(airports, latitude = lat)

###-------------- RELOCATE : envoie a la premiere place une variable 

relocate(airports, lon)


####################----------------------------
#------------------ENCHAINEMENT DES OPERATIONS

#Selectionner les vols partis en avance, garder que 10, selectionner que certaines variables(mois, jour),

select( slice(filter(flights, dep_delay < 0), 1:10 ), month, day, dep_delay)

#plus simple

d <- flights %>%
  filter(dep_delay < 0) %>%
  slice(1:10) %>%
  select(month, day, dep_delay)
d

########---------------------ARRANGE : Trier les observations

flights %>% arrange(dep_delay)   # si trie sur numerique donc croissante si charactere donc alphabetique


flights %>% arrange(month, day) # trie sur deux variables. sur la premiere puis la seconde 

flights %>% arrange(desc(dep_delay)) # decroissant donc le plus haut en premier

#-------------        MUTATE : Modifier une variable

#exemple cinvertire une variable en terme d'unite 

airports <- airports %>% 
  mutate(alt_m = alt / 3.2808)

flights %>%
  mutate(
    distance_km = distance / 0.62137,
    vitesse = distance_km / air_time * 60
  ) %>%
  select(distance, distance_km, vitesse)


########## OPERATIONS GROUPEES

####------------------------   GROUP BY

flights %>%
  group_by(month) %>%
  slice(1)              # Pour chaque mois on retient le premiere ligne, group by s'impose aux autres verbes
                        #Tout se fait par groupe

#eg : calculer un retard moyen par mois----

flights %>%
  group_by(month) %>%
  mutate(
    mean_delay = mean(dep_delay, na.rm = T)) %>%
  relocate(mean_delay)

flights <- flights %>%
  group_by(month) %>%
  mutate(
    mean_delay = mean(dep_delay, na.rm = T))  #ca garde toutes les observations mais si je veux juste avoir un tableau resumant par mois la moyenne des retard
                                              #utiliser summarise a la place de mutate

flights %>%
  group_by(month) %>%
  summarise(
    mean_delay = mean(dep_delay, na.rm = T)) #lorsqu'on a des fonctions de type resumé donc qui prenent plusieurs valeurs et n'en renvoient qu'une
                                            #mutate repete toutes les observations mais summarise fait un tableau synthétique 




########## fonctionS A FENETRES

#operations lignes a lignes
#operation de resume
#fonctions a fenetre ; fonction qui prend n valeurs et renvoie n valeur mais qui depend des valeurs voisines 
           #exemples   des fonctions cumulees cumsum, cummin, cummax

v <- c(4,6,7,8,9,1)

cumsum(v) # [1]  4 10 17 25 34 35

cummin(v) #[1] 4 4 4 4 4 1

cummax(v) #[1] 4 6 7 8 9 9

cummean(v) #[1] 4.000000 5.000000 5.666667 6.250000 6.800000 5.833333 moyenne cumulee a chaque etape

#fonctions de rang
rank(v) #[1] 2 3 4 5 6 1  c'est a dire que LE PREMIER element est a la 2IEME position par ordre croissant et le 5IEME correpondant a 1 est a la premiere 
        # petit probleme quand on a des valeurs qui se repetent 

v <- c(2,4,1,2,7,3,4,2,7)
?rank #rank(x, na.last = TRUE,
      #ties.method = c("average", "first", "last", "random", "max", "min"))

rank(v) #[1] 3.0 6.5 1.0 3.0 8.5 5.0 6.5 3.0 8.5 par defaut average ou moyenne des positions prendre tou sles rangs et leur appliquer la moyenne

rank(v, ties.method = "first") #[1] 2 6 1 3 8 5 7 4 9  


rank(v, ties.method = "min") #[1] 2 6 1 2 8 5 6 2 8 les excequo ont le meme rang 

lead(v) #[1]  4  1  2  7  3  4  2  7 NA   #video webinr 04 : 1H12MIN code tbl summary
lag(v)

################################################################
####------------------------   FUSION DES TABLES
#AJOUT D'OBSERVASION 
#AJOUT DE VARIABLES
#merge et jointure

##analyse bivariee

#package gtsummary et fonction tbl_summary
?add_p.tbl_summary


trial <- read_excel("trial.xlsx")
View(trial)

save(trial, file = "Data/trial.RData")


trial%>%
  tbl_summary()


trial%>%
  tbl_summary(
    by= "trt",
    percent = "row")%>%
  add_overall(last = T)%>%
  add_p()                   #library broom necessaire pour calculer pvalue

?tbl_merge
?add_p.tbl_summary

trial |>
  tbl_summary(by = trt, include = c(age, grade)) |>
  add_p()

#visualisation

ggbivariate(trial,
            outcome = "trt",
            explanatory = c("age","stage","grade"))

ggtable(trial,
        columnsX = c("trt"),
        columnsY = c("grade","stage"),
        cells = "row.prop",
        fill = "std.resid")


